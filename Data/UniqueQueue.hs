module Data.UniqueQueue where

import           Control.Concurrent.MVar
import           Control.Exception       (evaluate, mask_)
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as S

-- | A Chan capable of reporting whether any thread is currently reading.
data CChan a = CChan (MVar (MVar (ChItem a))) (MVar (MVar (ChItem a))) (MVar Int)

data ChItem a = ChItem a (MVar (ChItem a))

newCChan :: IO (CChan a)
newCChan = do
  hole <- newEmptyMVar
  CChan <$> newMVar hole <*> newMVar hole <*> newMVar 0

readCChan :: CChan a -> IO a
readCChan (CChan rMVar _ cnt) = modifyMVarMasked rMVar $ \oldHole -> do
  ChItem x newHole <- readMVar oldHole
  modifyMVar_ cnt (evaluate . pred)
  pure (newHole, x)

-- | Push into a queue and return whether the value was immediately popped.
writeCChan :: CChan a -> a -> IO Bool
writeCChan (CChan rMVar wMVar cnt) x = do
  newHole <- newEmptyMVar
  modifyMVarMasked wMVar $ \oldHole -> do
    waiting <- modifyMVar cnt $ \c -> do
      waiting <-
        if c == 0
        then isEmptyMVar rMVar
        else pure False
      pure (succ c, waiting)
    putMVar oldHole (ChItem x newHole)
    pure (newHole, waiting)

-- | A chan that doesn't accept duplicate values.
data UChan a = UChan (CChan a) (MVar (Seq a))

newUChan :: IO (UChan a)
newUChan = UChan <$> newCChan <*> newMVar S.empty

readUChan :: UChan a -> IO a
readUChan (UChan chan seqMVar) = mask_ $ do
  a <- readCChan chan
  modifyMVar_ seqMVar (evaluate . S.drop 1)
  pure a

-- | If the value isn't currently in the queue, push it and return whether it
-- was immediately popped. Otherwise return Nothing.
writeUChan :: Eq a => UChan a -> a -> IO (Maybe Bool)
writeUChan (UChan chan seqMVar) x = mask_ $ takeMVar seqMVar >>= \s ->
  if x `elem` s
  then do
    putMVar seqMVar s
    pure Nothing
  else do
    w <- writeCChan chan x
    putMVar seqMVar $ s S.|> x
    pure (Just w)
