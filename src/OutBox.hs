module OutBox where

import Control.Concurrent.STM
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

data Visibility = ToAll | ToPlayer Int
  deriving (Eq, Show)

data Msg = Msg
  { msgId           :: Int
  , msgVisibility   :: Visibility
  , msgText         :: String
  }
  deriving (Show)

newtype OutBox = OutBox (TVar (Seq Msg))
  deriving (Eq)

instance Show OutBox where
  show _ = "<OutBox>"

newOutBox :: IO OutBox
newOutBox = OutBox <$> newTVarIO Seq.empty

emit :: OutBox -> Visibility -> String -> IO ()
emit (OutBox ob) visible txt =
  atomically $ modifyTVar' ob $ \s -> s |> Msg (Seq.length s) visible txt

messagesSince :: OutBox -> Int -> Int -> STM (Int, [Msg])
messagesSince (OutBox ob) since pid = do
  s <- readTVar ob
  let new = toList (Seq.drop since s)
  return (Seq.length s, filter visible new)
  where
    visible m = case msgVisibility m of
      ToAll      -> True
      ToPlayer p -> p == pid
