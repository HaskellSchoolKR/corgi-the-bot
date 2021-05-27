{-# LANGUAGE TemplateHaskell #-}
module Control.EvalM where

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar, readMVar)
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader (ask, local),
                                            ReaderT (ReaderT, runReaderT), asks)
import           Control.Monad.State.Class (MonadState (get, state), gets)
import           Data.Config               (Config)
import           Data.Default              (Default (def))
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import           Data.UniqueQueue          (UChan)
import           Discord                   (DiscordHandle, DiscordHandler)
import           Discord.Types             (ChannelId, MessageId, UTCTime,
                                            UserId)

data Mode
  = HaskellEval
  | Haskell
  deriving stock (Eq, Show)

data Command
  = Reset Mode
  | EvalLine Mode Text
  | EvalBlock Mode Text
  deriving stock (Eq, Show)

type ParsedRequest = [Command]

data RecentMsg
  = RecentMsg
    { _msgTime       :: UTCTime
    , _msgRequest    :: ParsedRequest
    , _msgMentionsMe :: Bool
    , _msgMyResponse :: Maybe MessageId
    , _msgRequestor  :: UserId
    }
  deriving stock (Show)
makeLenses ''RecentMsg

data EvalState
  = EvalState
    { _botHandle  :: DiscordHandle
    , _recentMsgs :: Map (ChannelId, MessageId) RecentMsg
    , _channel    :: UChan (ChannelId, MessageId)
    }
makeLenses ''EvalState

instance Default EvalState where
  def =
    EvalState
    { _botHandle = error "Not connected"
    , _recentMsgs = M.empty
    , _channel = error "No channel provided"
    }

data EvalMRecord = EvalMRecord
  { config        :: !Config
  , evalStateMVar :: !(MVar EvalState)
  }

newtype EvalM a = EvalM { unEvalM :: ReaderT EvalMRecord IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   )

instance MonadReader Config EvalM where
  ask = EvalM $ asks config
  local f (EvalM r) = EvalM $ flip local r $ \mr -> mr { config = f (config mr) }

instance MonadState EvalState EvalM where
  get = EvalM $ ReaderT $ \EvalMRecord {..} -> readMVar evalStateMVar
  state f = EvalM $ ReaderT $ \EvalMRecord {..} -> modifyMVar evalStateMVar $ \ms -> do
    let (a, !ms') = f ms
    pure (ms', a)

instance MonadFail EvalM where
  fail = error

unliftingEvalM :: Config -> EvalState -> ((forall x. EvalM x -> IO x) -> IO a) -> IO a
unliftingEvalM config evalSt k = do
  evalStateMVar <- newMVar evalSt
  k $ \(EvalM sm) -> runReaderT sm $ EvalMRecord {..}

liftDiscordHandler :: DiscordHandler a -> EvalM a
liftDiscordHandler d = do
  h <- gets _botHandle
  liftIO $ runReaderT d h
