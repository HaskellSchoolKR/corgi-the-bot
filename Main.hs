{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Char
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Ord
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Text.Lens
import           Data.Time.Clock
import           Discord
import           Discord.Internal.Rest
import           Discord.Requests
import           Discord.Types
import           DupQueue
import           GHC.Stack
import           Pastebin
import           StateM
import           System.IO
import           System.Process

configFile = "corgi-the-bot.conf"

data Mode
  = HaskellEval
  | Haskell
  | Shell
  deriving (Eq, Show)

data Command
  = Reset Mode
  | EvalLine Mode Text
  | EvalBlock Mode Text
  deriving (Eq, Show)

type ParsedReq = [Command]

data RecentMsg
  = RecentMsg
    { _msgTime       :: UTCTime
    , _msgRequest    :: ParsedReq
    , _msgMentionsMe :: Bool
    , _msgMyResponse :: Maybe MessageId
    , _msgRequestor  :: UserId
    }
  deriving Show
makeLenses ''RecentMsg

data EvalState
  = EvalState
    { _botHandle  :: DiscordHandle
    , _recentMsgs :: Map (ChannelId, MessageId) RecentMsg
    , _channel    :: UChan (ChannelId, MessageId)
    }
makeLenses ''EvalState

type EvalM = StateM Value EvalState

logM :: String -> EvalM ()
logM s = do
  time <- liftIO getCurrentTime
  let line = unlines $ map (show time ++) $ zipWith (++) (": " : repeat "> ") $ lines s
  preuse (persistent . key "log" . _String . _Text) >>= \case
    Just logFile -> liftIO $ appendFile logFile line
    Nothing      -> pure ()
  liftIO $ putStr line

logShowM :: Show a => a -> EvalM ()
logShowM = logM . show

traceM :: HasCallStack => EvalM ()
traceM = logShowM callStack

instance Exception RestCallErrorCode

sendEither :: (FromJSON a, Request (r a)) => r a -> EvalM (Either RestCallErrorCode a)
sendEither req = use (dynamic . botHandle) >>= \bot -> liftIO $ runReaderT (restCall req) bot

sendTrace :: HasCallStack => (FromJSON a, Request (r a)) => r a -> EvalM ()
sendTrace req = do resp <- sendEither req
                   case resp of
                     Left err -> do logM "Error while calling REST:"
                                    logShowM err
                                    traceM
                     Right _ -> pure ()

send :: HasCallStack => (FromJSON a, Request (r a)) => r a -> EvalM a
send req = do resp <- sendEither req
              case resp of
                Left err -> throwM err
                Right x  -> pure x

main = do hSetBuffering stdout NoBuffering
          h <- newEmptyMVar
          c <- newUChan
          let initEnv = EvalState { _botHandle = error "Not connected"
                                  , _recentMsgs = M.empty
                                  , _channel = c
                                  }
          unliftingStateM (readJsonFile configFile) (encodeFile configFile) initEnv $ \unlift -> do
            token <- unlift $ use (persistent . key "auth" . _String)
            forkIO $ unlift backendLoop
            runDiscord $ RunDiscordOpts
              { discordToken = token
              , discordOnStart = ReaderT $ \handle -> unlift $ do
                  assign (dynamic . botHandle) handle
              , discordOnEnd = unlift $ do
                  assign (dynamic . botHandle) $ error "Not connected"
              , discordOnEvent = \event -> ReaderT $ \handle -> do
                  unlift $ do assign (dynamic . botHandle) handle
                              logShowM event
                  catch (unlift $ handleEvent event)
                        (\e -> unlift $ do logM "Exception in event loop:"
                                           logShowM (e :: SomeException))
              , discordOnLog = putStrLn . T.unpack
              , discordForkThreadForEvents = False
              }

parseMode :: Text -> Mode
parseMode t = case T.toLower t of
  ""        -> HaskellEval
  "hs"      -> HaskellEval
  "haskell" -> HaskellEval
  "ghc"     -> Haskell
  "!"       -> Shell
  "sh"      -> Shell
  "bash"    -> Shell
  "shell"   -> Shell
  _         -> HaskellEval

parseMessage :: Int -> Text -> ParsedReq
parseMessage maxBlocks cmd | (m, rest) <- T.break (\x -> x == '`' || isSpace x) cmd = take maxBlocks $ go (parseMode m) rest
  where go mode t | Just next <- T.stripPrefix "!reset" t
                  = Reset mode : go mode next
                  | Just rest <- T.stripPrefix "```" t
                  , "```" `T.isInfixOf` rest
                  , (inside, next) <- T.breakOn "```" rest
                  , (tag, code) <- dropLanguageTag inside
                  = EvalBlock (maybe mode parseMode tag) (T.copy code) : go mode (T.drop 3 next)
                  | Just rest <- T.stripPrefix "``" t
                  , "``" `T.isInfixOf` rest
                  , (inside, next) <- T.breakOn "``" rest
                  = EvalLine mode (T.map stripControl inside) : go mode (T.drop 2 next)
                  | Just rest <- T.stripPrefix "`" t
                  , "`" `T.isInfixOf` rest
                  , (inside, next) <- T.breakOn "`" rest
                  = EvalLine mode (T.map stripControl inside) : go mode (T.drop 1 next)
                  | Just next <- T.stripPrefix "\\" t
                  = go mode (T.tail next)
                  | T.null t = []
                  | otherwise = go mode (T.tail t)
        dropLanguageTag t | (first, rest) <- T.break (== '\n') t
                          , not (T.any isSpace first)
                          , not (T.null rest) = (mfilter (not . T.null) $ Just first, rest)
                          | otherwise = (Nothing, t)
        stripControl c | isControl c = ' '
                       | otherwise = c

pruneMessages :: EvalM ()
pruneMessages = do time <- liftIO getCurrentTime
                   Just !secs <- preuse (persistent . key "pruneAfterSeconds" . _Integer)
                   modifying (dynamic . recentMsgs)
                     (M.filter (\msg -> diffUTCTime time (msg ^. msgTime) < fromIntegral secs))

checkTest :: Maybe GuildId -> EvalM () -> EvalM ()
checkTest mb c = do Just !guilds <- preuses (persistent . key "testGuilds" . _Array)
                                            (toListOf $ folded . _Integer . to fromIntegral)
                    Just !test <- preuse (persistent . key "test" . _Bool)
                    when (test == any (`elem` guilds) mb) c

getMyId :: EvalM UserId
getMyId = do
  handle <- use (dynamic . botHandle)
  cache <- liftIO $ runReaderT readCache handle
  pure $ userId $ _currentUser cache

handleEvent :: HasCallStack => Event -> EvalM ()
handleEvent (MessageCreate Message{..}) = do
  logM $ "[" ++ maybe "" show messageGuild ++ "] <#" ++ show messageChannel ++ "> <@" ++ show (userId messageAuthor) ++ "> <" ++ T.unpack (userName messageAuthor) ++ "#" ++ T.unpack (userDiscrim messageAuthor) ++ "> " ++ T.unpack messageText ++ " (" ++ show messageId ++ ")"
  myId <- getMyId
  let mentionsMe = myId `elem` (userId <$> messageMentions)
  let hasPrefix = T.isPrefixOf ">" messageText
  when (hasPrefix || mentionsMe) $ do
    checkTest messageGuild $ do
      pruneMessages
      time <- liftIO getCurrentTime
      Just !maxBlocks <- preuse (persistent . key "maxBlocksPerMsg" . _Integral)
      let req = parseMessage maxBlocks messageText
      assign (dynamic . recentMsgs . at (messageChannel, messageId)) $ Just $ RecentMsg
        { _msgTime = time
        , _msgRequest = req
        , _msgMentionsMe = mentionsMe
        , _msgMyResponse = Nothing
        , _msgRequestor = userId messageAuthor
        }
      unless (null req) $ do
        queue <- use (dynamic . channel)
        status <- liftIO $ writeUChan queue (messageChannel, messageId)
        when (status == Just False) $ do
          Just !wait <- preuse (persistent . key "reactWait" . _String)
          sendTrace $ CreateReaction (messageChannel, messageId) wait
handleEvent (MessageUpdate messageChannel messageId) = do
  pruneMessages
  x <- use (dynamic . recentMsgs . at (messageChannel, messageId))
  Just !maxBlocks <- preuse (persistent . key "maxBlocksPerMsg" . _Integral)
  case x of
    Just RecentMsg{..} -> do
      Message{..} <- send $ GetChannelMessage (messageChannel, messageId)
      logM $ "[" ++ maybe "" show messageGuild ++ "] <#" ++ show messageChannel ++ "> <@" ++ show (userId messageAuthor) ++ "> <" ++ T.unpack (userName messageAuthor) ++ "#" ++ T.unpack (userDiscrim messageAuthor) ++ "> " ++ T.unpack messageText ++ " (" ++ show messageId ++ " edited)"
      myId <- getMyId
      let mentionsMe = myId `elem` (userId <$> messageMentions)
      let req = parseMessage maxBlocks messageText
      assign (dynamic . recentMsgs . at (messageChannel, messageId) . _Just . msgMentionsMe) mentionsMe
      when (_msgRequest /= req) $ do
        if null req
        then case _msgMyResponse of
          Just id -> do sendTrace $ DeleteMessage (messageChannel, id)
                        assign (dynamic . recentMsgs . at (messageChannel, messageId) . _Just . msgMyResponse) Nothing
          _ -> pure ()
        else do assign (dynamic . recentMsgs . at (messageChannel, messageId) . _Just . msgRequest) req
                queue <- use (dynamic . channel)
                status <- liftIO $ writeUChan queue (messageChannel, messageId)
                when (status == Just False) $ do
                  Just !wait <- preuse (persistent . key "reactWait" . _String)
                  sendTrace $ CreateReaction (messageChannel, messageId) wait
    _ -> pure ()
handleEvent (MessageDelete messageChannel messageId) = do
  logM $ "<#" ++ show messageChannel ++ "> (" ++ show messageId ++ " deleted)"
  pruneMessages
  x <- use (dynamic . recentMsgs . at (messageChannel, messageId))
  case x of
    Just RecentMsg{_msgMyResponse = Just msgId} -> sendTrace $ DeleteMessage (messageChannel, msgId)
    _ -> pure ()
handleEvent (MessageDeleteBulk messageChannel messageIds) = do
  logM $ "<#" ++ show messageChannel ++ "> (" ++ show messageIds ++ " deleted)"
  pruneMessages
  forM_ messageIds $ \messageId -> do
    x <- use (dynamic . recentMsgs . at (messageChannel, messageId))
    case x of
      Just RecentMsg{_msgMyResponse = Just msgId} -> sendTrace $ DeleteMessage (messageChannel, msgId)
      _ -> pure ()
handleEvent (MessageReactionAdd ReactionInfo{..}) = do
  checkTest reactionGuildId $ do
    Just !cancel <- preuse (persistent . key "reactCancel" . _String)
    myId <- getMyId
    when (reactionUserId /= myId && emojiName reactionEmoji == cancel) $ do
      msgs <- use (dynamic . recentMsgs . to M.toList . to (filter $ filterResponse reactionChannelId reactionMessageId reactionUserId))
      forM_ msgs $ \((chanId, reqId), RecentMsg{..}) -> do
        forM_ _msgMyResponse $ \id -> do
          sendTrace $ DeleteMessage (chanId, id)
        assign (dynamic . recentMsgs . at (chanId, reqId)) Nothing
    where
      filterResponse chanId msgId userId ((chanId', _), msg)
        = chanId == chanId' && msg ^. msgMyResponse == Just msgId && msg ^. msgRequestor == userId
handleEvent _ = pure ()

backendLoop :: EvalM ()
backendLoop = forever $ catch (do
  queue <- use (dynamic . channel)
  (chan, msg) <- liftIO $ readUChan queue
  Just !wait <- preuse (persistent . key "reactWait" . _String)
  sendTrace $ DeleteOwnReaction (chan, msg) wait
  x <- use (dynamic . recentMsgs . at (chan, msg))
  case x of
    Just RecentMsg{..} -> do
      outs <- forM _msgRequest $ \cmd -> do
        sendTrace $ TriggerTypingIndicator chan
        case cmd of
          Reset mode -> do resetMode mode
                           pure ""
          EvalLine mode ln -> evalLine mode (encodeUtf8 ln)
          EvalBlock mode blk -> evalBlock mode (encodeUtf8 blk)
      text <- formatResults _msgMentionsMe (decodeUtf8With (replace '?') <$> outs)
      Just !cancel <- preuse (persistent . key "reactCancel" . _String)
      case _msgMyResponse of
        Just id -> do
          sendTrace $ EditMessage (chan, id) text Nothing
          if _msgMentionsMe then sendTrace $ DeleteOwnReaction (chan, id) cancel
                            else sendTrace $ CreateReaction (chan, id) cancel
        _ -> do Message{..} <- send $ CreateMessage chan text
                assign (dynamic . recentMsgs . at (chan, msg) . _Just . msgMyResponse) $ Just messageId
                unless _msgMentionsMe $ do
                  sendTrace $ CreateReaction (chan, messageId) cancel
    Nothing -> pure ())
  (\e -> do
      logM "Exception in backendLoop:"
      logShowM (e :: SomeException))

formatResults :: Bool -> [Text] -> EvalM Text
formatResults mentionsMe res = do
  Just !maxChars <- preuse (persistent . key "maxCharsPerMsg" . _Integral)
  Just !cancel <- preuse (persistent . key "reactCancel" . _String)
  addWarning cancel <$> do
    msg <- doFormat maxChars
    if T.null msg
    then do Just !check <- preuse (persistent . key "reactCheck" . _String)
            pure check
    else pure msg
  where doFormat maxChars = T.concat <$> mapM format nonempty
          where
            nonempty = zip [0..] $ filter (not . T.null . fst) $ map (sanitize &&& id) res

            sanitize s = let r = T.replace "``" "``\x200D" $ T.filter (\x -> x == '\n' || not (isControl x)) s
                         in if T.isSuffixOf "`" r then T.append r "\x200D" else r

            sorted = sortOn (T.length . fst . snd) nonempty

            accumulate :: Int -> Set Int -> [(Int, (Text, Text))] -> Set Int
            accumulate n s [] = s
            accumulate n s ((i, (x, _)):xs)
              | n + 8 + T.length x < maxChars - (3 + pasteUrlLength) * length xs
              = accumulate (n + 8 + T.length x) (S.insert i s) xs
              | otherwise = s

            small = accumulate 0 S.empty sorted

            format (i, (san, orig))
              | i `S.member` small = pure $ T.concat ["```\n", san, "```"]
              | otherwise = do link <- liftIO $ paste $ encodeUtf8 orig -- TODO: don't double encode
                               pure $ T.concat ["<", decodeUtf8With lenientDecode link, ">\n"]

        addWarning cancel xs | mentionsMe = xs
                             | otherwise = xs <> " (``> `...` `` syntax is deprecated, please @ me instead, " <> cancel <> " to cancel)"

resetMode :: Mode -> EvalM ()
resetMode HaskellEval = launchWithData ["kill", "Dghci"] "" >> pure ()
resetMode _           = pure ()

evalLine :: Mode -> ByteString -> EvalM ByteString
evalLine HaskellEval line = launchWithLine ["Dghci"] line
evalLine mode line        = evalBlock mode line

evalBlock :: Mode -> ByteString -> EvalM ByteString
evalBlock HaskellEval block = do
  Just !maxChars <- preuse (persistent . key "maxOutput" . _Integral)
  fmap (B.take maxChars . B.concat) $ mapM (evalLine HaskellEval) $ [":{"] ++ B.split (fromIntegral $ fromEnum '\n') block ++ [":}"]
evalBlock Shell block = launchWithData ["bash"] block
evalBlock Haskell block = launchWithData ["runghc"] block

launchWithLine :: HasCallStack => [String] -> ByteString -> EvalM ByteString
launchWithLine args s = launchWithData args (B.filter (/= fromIntegral (fromEnum '\n')) s `B.snoc` fromIntegral (fromEnum '\n'))

launchWithData :: HasCallStack => [String] -> ByteString -> EvalM ByteString
launchWithData args s = do
  Just !cmd <- preuse (persistent . key "sandboxCmd" . _String . _Text)
  Just !conf <- preuse (persistent . key "sandboxConf" . _String . _Text)
  liftIO $ B.writeFile "input" s
  input <- liftIO $ openBinaryFile "input" ReadMode
  (outr, outw) <- liftIO createPipe
  (_, _, _, p) <- liftIO $ createProcess (proc cmd (conf:args)) { std_in = UseHandle input
                                                                , std_out = UseHandle outw
                                                                , std_err = UseHandle outw
                                                                , close_fds = True
                                                                }
  Just !maxChars <- preuse (persistent . key "maxOutput" . _Integral)
  liftIO $ finally (B.hGet outr maxChars)
                   (do hClose outr
                       waitForProcess p)

readJsonFile :: FilePath -> IO Value
readJsonFile file = do result <- eitherDecodeFileStrict' file
                       case result of
                         Left err   -> error (show err)
                         Right json -> pure json
