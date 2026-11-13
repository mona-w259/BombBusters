module GameLogic.InputBox where

import Control.Concurrent.STM
import qualified Data.Map as Map

newtype InputBox = InputBox (Map.Map Int (TQueue String))
  deriving (Eq)

instance Show InputBox where
  show _ = "<InputBox>"


newInputBox :: [Int] -> IO InputBox
newInputBox pids = InputBox . Map.fromList <$> mapM (\p -> (,) p <$> newTQueueIO) pids

pushInput :: InputBox -> Int -> String -> IO Bool
pushInput (InputBox m) pid txt = case Map.lookup pid m of
  Nothing -> pure False
  Just q  -> atomically (writeTQueue q txt) >> pure True
