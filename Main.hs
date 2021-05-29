{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Api.Pastebin             (paste, pasteUrlLength)
import           Control.Concurrent       (forkIO)
import           Control.EvalM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader     (ReaderT (ReaderT))
import           Data.Aeson
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Char
import           Data.Coerce              (coerce)
import qualified Data.Config              as Config
import           Data.Default             (def)
import           Data.List                (sortOn)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import qualified Data.Text.IO             as T
import           Data.Time.Clock
import           Data.UniqueQueue
import           Discord                  hiding (def)
import           Discord.Internal.Rest
import           Discord.Requests
import           GHC.IO.Encoding          (setLocaleEncoding, utf8)
import           GHC.Stack                (HasCallStack, callStack)
import           System.IO                (BufferMode (NoBuffering), hClose,
                                           hSetBuffering, stdout)
import           System.Process

configFile :: FilePath
configFile = "corgi-the-bot.conf"

logM :: String -> EvalM ()
logM s = do
  time <- liftIO getCurrentTime
  let line = unlines $ map (show time ++) $ zipWith (++) (": " : repeat "> ") $ lines s
  logFile <- view Config.log
  liftIO $ appendFile logFile line
  liftIO $ putStr line

logShowM :: Show a => a -> EvalM ()
logShowM = logM . show

traceM :: HasCallStack => EvalM ()
traceM = logShowM callStack

newtype RestCallError = RestCallError RestCallErrorCode
  deriving newtype Show
  deriving anyclass Exception

sendEither :: (FromJSON a, Request (r a)) => r a -> EvalM (Either RestCallError a)
sendEither = coerce . liftDiscordHandler . restCall

sendTrace :: (HasCallStack, FromJSON a, Request (r a)) => r a -> EvalM ()
sendTrace req = do
  resp <- sendEither req
  case resp of
    Left err -> do
      logM "Error while calling REST:"
      logShowM err
      traceM
    Right _ -> pure ()

send :: (FromJSON a, Request (r a)) => r a -> EvalM a
send req = do
  resp <- sendEither req
  case resp of
    Left err -> throwM err
    Right x  -> pure x

main :: IO ()
main = do
  -- To guarantee that this works with Hangul characters
  setLocaleEncoding utf8
  config <- either (error . show) id <$> eitherDecodeFileStrict' configFile
  hSetBuffering stdout NoBuffering
  c <- newUChan
  let initEnv = def { _channel = c }
  unliftingEvalM config initEnv $ \unlift -> do
    discordToken <- unlift $ view Config.auth
    _ <- forkIO $ unlift backendLoop
    let
      discordOnStart = ReaderT $ unlift . assign botHandle
      discordOnEnd = unlift . assign botHandle $ def ^. botHandle
      discordOnEvent event = ReaderT $ \h -> unlift $ do
        assign botHandle h
        logShowM event
        catch (handleEvent event)
          (\e -> do
              logM "Exception in event loop:"
              logShowM (e :: SomeException))
      discordOnLog = T.putStrLn
      discordForkThreadForEvents = False
      discordOpts = RunDiscordOpts {..}
    void . runDiscord $ discordOpts

parseMode :: Text -> Mode
parseMode t = case T.toLower t of
  "hs"      -> HaskellEval
  "haskell" -> HaskellEval
  "ghc"     -> Haskell
  "runghc"  -> Haskell
  _         -> HaskellEval

parseMessage :: Int -> Text -> ParsedRequest
parseMessage maxBlocks cmd
  | (m, rest) <- T.break (\x -> x == '`' || isSpace x) cmd = take maxBlocks $ go (parseMode m) rest
  where
    go mode t
      | Just next <- T.stripPrefix "!reset" t
      = Reset mode : go mode next
      | Just rest <- T.stripPrefix "```" t
      , "```" `T.isInfixOf` rest
      , (inner, next) <- T.breakOn "```" rest
      , (tag, code) <- dropLanguageTag inner
      = EvalBlock (maybe mode parseMode tag) (T.copy code) : go mode (T.drop 3 next)
      | Just rest <- T.stripPrefix "``" t
      , "``" `T.isInfixOf` rest
      , (inner, next) <- T.breakOn "``" rest
      = EvalLine mode (T.map stripControl inner) : go mode (T.drop 2 next)
      | Just rest <- T.stripPrefix "`" t
      , "`" `T.isInfixOf` rest
      , (inner, next) <- T.breakOn "`" rest
      = EvalLine mode (T.map stripControl inner) : go mode (T.drop 1 next)
      | Just next <- T.stripPrefix "\\" t
      = go mode (T.tail next)
      | T.null t
      = []
      | otherwise
      = go mode (T.tail t)

    dropLanguageTag t
      | (fstLine, rest) <- T.break (== '\n') t
      , not (T.any isSpace fstLine)
      , not (T.null rest)
      = (mfilter (not . T.null) $ Just fstLine, rest)
      | otherwise
      = (Nothing, t)

    stripControl c
      | isControl c = ' '
      | otherwise   = c

pruneMessages :: EvalM ()
pruneMessages = do
  time <- liftIO getCurrentTime
  !secs <- view Config.pruneAfterSeconds
  modifying recentMsgs
    (M.filter (\msg -> diffUTCTime time (msg ^. msgTime) < secs))

checkTest :: Maybe GuildId -> EvalM () -> EvalM ()
checkTest mb c = do
  !guilds <- view Config.testGuilds
  !test <- view Config.test
  when (test == any (`elem` guilds) mb) c

getMyId :: EvalM UserId
getMyId = do
  cache <- liftDiscordHandler readCache
  pure $ userId $ _currentUser cache

handleEvent :: HasCallStack => Event -> EvalM ()
handleEvent (MessageCreate Message {..}) = do
  logM $ "[" ++ maybe "" show messageGuild ++ "] <#" ++ show messageChannel ++ "> <@" ++ show (userId messageAuthor) ++ "> <" ++ T.unpack (userName messageAuthor) ++ "#" ++ T.unpack (userDiscrim messageAuthor) ++ "> " ++ T.unpack messageText ++ " (" ++ show messageId ++ ")"
  myId <- getMyId
  let _msgMentionsMe = myId `elem` (userId <$> messageMentions)
  when _msgMentionsMe $ do
    checkTest messageGuild $ do
      pruneMessages
      _msgTime <- liftIO getCurrentTime
      !maxBlocks <- view Config.maxBlocksPerMsg
      let
        _msgRequest = parseMessage maxBlocks messageText
        _msgMyResponse = Nothing
        _msgRequestor = userId messageAuthor
      assign (recentMsgs . at (messageChannel, messageId)) $ Just $ RecentMsg {..}
      unless (null _msgRequest) $ do
        queue <- use channel
        status <- liftIO $ writeUChan queue (messageChannel, messageId)
        when (status == Just False) $ do
          !wait <- view Config.reactWait
          sendTrace $ CreateReaction (messageChannel, messageId) wait
handleEvent (MessageUpdate mChan mId) = do
  pruneMessages
  x <- use (recentMsgs . at (mChan, mId))
  !maxBlocks <- view Config.maxBlocksPerMsg
  case x of
    Just RecentMsg {..} -> do
      Message {..} <- send $ GetChannelMessage (mChan, mId)
      logM $ "[" ++ maybe "" show messageGuild ++ "] <#" ++ show messageChannel ++ "> <@" ++ show (userId messageAuthor) ++ "> <" ++ T.unpack (userName messageAuthor) ++ "#" ++ T.unpack (userDiscrim messageAuthor) ++ "> " ++ T.unpack messageText ++ " (" ++ show messageId ++ " edited)"
      myId <- getMyId
      let mentionsMe = myId `elem` (userId <$> messageMentions)
      let req = parseMessage maxBlocks messageText
      assign (recentMsgs . at (messageChannel, messageId) . _Just . msgMentionsMe) mentionsMe
      when (_msgRequest /= req) $ do
        if null req
          then
            case _msgMyResponse of
              Just respMId -> do
                sendTrace $ DeleteMessage (messageChannel, respMId)
                assign (recentMsgs . at (messageChannel, messageId) . _Just . msgMyResponse) Nothing
              _ -> pure ()
          else do
            assign (recentMsgs . at (messageChannel, messageId) . _Just . msgRequest) req
            queue <- use channel
            status <- liftIO $ writeUChan queue (messageChannel, messageId)
            when (status == Just False) $ do
              !wait <- view Config.reactWait
              sendTrace $ CreateReaction (messageChannel, messageId) wait
    _ -> pure ()
handleEvent (MessageDelete messageChannel messageId) = do
  logM $ "<#" ++ show messageChannel ++ "> (" ++ show messageId ++ " deleted)"
  pruneMessages
  x <- use (recentMsgs . at (messageChannel, messageId))
  case x of
    Just RecentMsg{_msgMyResponse = Just msgId} -> sendTrace $ DeleteMessage (messageChannel, msgId)
    _ -> pure ()
handleEvent (MessageDeleteBulk messageChannel messageIds) = do
  logM $ "<#" ++ show messageChannel ++ "> (" ++ show messageIds ++ " deleted)"
  pruneMessages
  forM_ messageIds $ \messageId -> do
    x <- use (recentMsgs . at (messageChannel, messageId))
    case x of
      Just RecentMsg{_msgMyResponse = Just msgId} -> sendTrace $ DeleteMessage (messageChannel, msgId)
      _ -> pure ()
handleEvent (MessageReactionAdd ReactionInfo {..}) = do
  checkTest reactionGuildId $ do
    !cancel <- view Config.reactCancel
    myId <- getMyId
    when (reactionUserId /= myId && emojiName reactionEmoji == cancel) $ do
      msgs <- use (recentMsgs . to M.toList . to (filter $ filterResponse reactionChannelId reactionMessageId reactionUserId))
      forM_ msgs $ \((chanId, reqId), RecentMsg {..}) -> do
        forM_ _msgMyResponse $ \respMId -> do
          sendTrace $ DeleteMessage (chanId, respMId)
        assign (recentMsgs . at (chanId, reqId)) Nothing
    where
      filterResponse chanId msgId userId ((chanId', _), msg) =
        chanId == chanId' && msg ^. msgMyResponse == Just msgId && msg ^. msgRequestor == userId
handleEvent _ = pure ()

backendLoop :: EvalM ()
backendLoop = forever $ catch (do
  queue <- use channel
  (chan, msg) <- liftIO $ readUChan queue
  !wait <- view Config.reactWait
  sendTrace $ DeleteOwnReaction (chan, msg) wait
  x <- use (recentMsgs . at (chan, msg))
  case x of
    Just RecentMsg {..} -> do
      outs <- forM _msgRequest $ \cmd -> do
        sendTrace $ TriggerTypingIndicator chan
        case cmd of
          Reset mode -> do
            resetMode mode
            pure ""
          EvalLine mode ln -> evalLine mode (encodeUtf8 ln)
          EvalBlock mode blk -> evalBlock mode (encodeUtf8 blk)
      text <- formatResults (decodeUtf8With (replace '?') <$> outs)
      !cancel <- view Config.reactCancel
      case _msgMyResponse of
        Just respMId -> do
          sendTrace $ EditMessage (chan, respMId) text Nothing
          if _msgMentionsMe
            then sendTrace $ DeleteOwnReaction (chan, respMId) cancel
            else sendTrace $ CreateReaction (chan, respMId) cancel
        _ -> do
          Message {..} <- send $ CreateMessage chan text
          assign (recentMsgs . at (chan, msg) . _Just . msgMyResponse) $ Just messageId
          unless _msgMentionsMe $ do
            sendTrace $ CreateReaction (chan, messageId) cancel
    Nothing -> pure ())
  (\e -> do
      logM "Exception in backendLoop:"
      logShowM (e :: SomeException))

formatResults :: [Text] -> EvalM Text
formatResults res = do
  !maxChars <- view Config.maxCharsPerMsg
  msg <- doFormat maxChars
  if T.null msg
    then view Config.reactCancel
    else pure msg
  where
    doFormat maxChars = T.concat <$> mapM format nonempty
      where
        nonempty = zip [(0 :: Int)..] $ filter (not . T.null . fst) $ map ((,) =<< sanitize) res

        sanitize s =
          let
            r = T.replace "``" "``\x200D" $ T.filter (\x -> x == '\n' || not (isControl x)) s
          in
            if T.isSuffixOf "`" r
            then T.append r "\x200D"
            else r

        sorted = sortOn (T.length . fst . snd) nonempty

        accumulate :: Int -> Set Int -> [(Int, (Text, Text))] -> Set Int
        accumulate _ s [] = s
        accumulate n s ((i, (x, _)):xs)
          | n + 8 + T.length x < maxChars - (3 + pasteUrlLength) * length xs
          = accumulate (n + 8 + T.length x) (S.insert i s) xs
          | otherwise = s

        small = accumulate 0 S.empty sorted

        format (i, (san, orig))
          | i `S.member` small = pure $ T.concat ["```\n", san, "```"]
          | otherwise = do
              link <- liftIO $ paste $ encodeUtf8 orig -- TODO: don't double encode
              pure $ T.concat ["<", decodeUtf8With lenientDecode link, ">\n"]

resetMode :: Mode -> EvalM ()
resetMode HaskellEval = void $ launchWithData ["kill", "corgi-ghci"] ""
resetMode _           = pure ()

evalLine :: Mode -> ByteString -> EvalM ByteString
evalLine HaskellEval = launchWithLine ["corgi-ghci"]
evalLine mode        = evalBlock mode

evalBlock :: Mode -> ByteString -> EvalM ByteString
-- evalBlock HaskellEval block = do
--   !maxChars <- view Config.maxOutput
--   fmap (B.take maxChars . B.concat) $ mapM (evalLine HaskellEval) $ [":{"] ++ B.split (fromIntegral $ fromEnum '\n') block ++ [":}"]
evalBlock _ = launchWithData ["corgi-runghc"]

launchWithLine :: [String] -> ByteString -> EvalM ByteString
launchWithLine args s =
  launchWithData args (B.filter (/= fromIntegral (fromEnum '\n')) s `B.snoc` fromIntegral (fromEnum '\n'))

launchWithData :: [String] -> ByteString -> EvalM ByteString
launchWithData args s = do
  !cmd <- view Config.sandboxCmd
  !conf <- view Config.sandboxConf
  (p, inH, outH) <- liftIO (createCmdProcess cmd conf)
  !maxChars <- view Config.maxOutput
  liftIO $ B.hPut inH s
  liftIO $ finally (B.hGet outH maxChars) $ do
    hClose outH
    waitForProcess p
  where
    createCmdProcess cmd conf = do
      (inRh, inWh) <- createPipe
      (outRh, outWh) <- createPipe
      let
        std_in = UseHandle inRh
        std_out = UseHandle outWh
        std_err = UseHandle outWh
        close_fds = True
      (_, _, _, p) <- createProcess
        (proc cmd (conf:args)) { std_in, std_out, std_err, close_fds }
      pure (p, inWh, outRh)
