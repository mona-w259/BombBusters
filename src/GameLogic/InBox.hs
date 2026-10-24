module GameLogic.InBox where

import Control.Concurrent.STM
import qualified Data.Map as Map

newtype InBox = InBox (Map.Map Int (TQueue String))
  deriving (Eq)

instance Show InBox where
  show _ = "<InBox>"


newInBox :: [Int] -> IO InBox
newInBox pids = InBox . Map.fromList <$> mapM (\p -> (,) p <$> newTQueueIO) pids

pushInput :: InBox -> Int -> String -> IO Bool
pushInput (InBox m) pid txt = case Map.lookup pid m of
  Nothing -> pure False
  Just q  -> atomically (writeTQueue q txt) >> pure True

readInputFor :: InBox -> Int -> IO String
readInputFor (InBox m) pid = atomically (readTQueue (m Map.! pid))

clearInput :: InBox -> Int -> IO ()
clearInput (InBox m) pid = case Map.lookup pid m of
  Nothing -> pure ()
  Just q  -> atomically (flushTQueue q) >> pure ()