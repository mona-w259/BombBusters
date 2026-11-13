module GameLogic.InputReader where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Exception (Exception, throwIO)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import GameLogic.Output
import GameLogic.InputBox
import GameLogic.Util
import Types


data GoBack = GoBack deriving Show
instance Exception GoBack

isBack :: String -> Bool
isBack s = s == "\ESC" || s == "back"

clearInput :: InputBox -> Int -> IO ()
clearInput (InputBox m) pid = case Map.lookup pid m of
  Nothing -> pure ()
  Just q  -> atomically (flushTQueue q) >> pure ()


------------------------------------------------------------------
--                   READ INPUT FUNCTIONS                       --
------------------------------------------------------------------

readInput :: PlayerView -> GameM String
readInput pv = do
  let pid = playerId (pvSelf pv)
  iB <- gets inputBox
  liftIO (clearInput iB pid)
  syncState
  liftIO (readInputFor iB pid)

readInputFor :: InputBox -> Int -> IO String
readInputFor (InputBox m) pid = atomically (readTQueue (m Map.! pid))

readInt :: PlayerView -> String -> GameM Int
readInt pv prompt = do
  let pid = playerId (pvSelf pv)
  tellAction pid prompt
  input <- readInput pv
  case readMaybe input :: Maybe Int of
    Just n -> return n
    Nothing -> do
      tellResult pid "Invalid number." -- integer
      readInt pv prompt

readLabel :: PlayerView -> String -> GameM Int
readLabel pv prompt = do
  let pid = playerId (pvSelf pv)
  tellAction pid prompt
  input <- readInput pv
  case input of
    [c] | c >= 'a' && c <= 'z' -> return (fromEnum c - fromEnum 'a')
    _ -> do
      tellResult pid "Invalid input." -- single lowercase letter (e.g. a, b, c)
      readLabel pv prompt

readIntB :: PlayerView -> String -> GameM Int
readIntB pv prompt = do
  let pid = playerId (pvSelf pv)
  tellAction pid prompt 
  input <- readInput pv
  if isBack input
    then liftIO (throwIO GoBack)
    else case readMaybe input :: Maybe Int of
      Just n -> return n
      Nothing -> do
        tellResult pid "Invalid number."    --  integer
        readIntB pv prompt

readLabelB :: PlayerView -> String -> GameM Int
readLabelB pv prompt = do
  let pid = playerId (pvSelf pv)
  tellAction pid prompt 
  input <- readInput pv
  if isBack input
    then liftIO (throwIO GoBack)
    else case input of
      [c] | c >= 'a' && c <= 'z' -> return (fromEnum c - fromEnum 'a')
      _ -> do
        tellResult pid "Invalid input."     -- single lowercase letter
        readLabelB pv prompt

readPlayerId :: PlayerView -> String -> GameM Int
readPlayerId pv prompt = setAwaiting pv "player" >> readIntB pv prompt

readWire :: PlayerView -> String -> GameM Int
readWire pv prompt = setAwaiting pv "wire" >> readLabelB pv prompt

readPromptInput :: PlayerView -> String -> GameM String
readPromptInput pv prompt = do
  let pid = playerId (pvSelf pv)
  iB <- gets inputBox
  liftIO (clearInput iB pid)
  tellAction pid prompt
  liftIO (readInputFor iB pid)
