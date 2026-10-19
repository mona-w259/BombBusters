module Output where

import Control.Monad.State
import Control.Concurrent.STM
import qualified Data.Map as Map
import Types
import OutBox


colorEmoji :: WireColor -> String
colorEmoji Blue = "🔵"
colorEmoji Yellow = "🟡"
colorEmoji Red = "🔴"
colorEmoji Grey = "⚫"
colorEmoji Hidden = "⬜"

syncState :: GameM ()
syncState = do
  gs <- get
  liftIO $ atomically $ modifyTVar' (syncVar gs) (Map.insert (code gs) gs)

errorMessage :: String -> a
errorMessage err = error err

instructionMessage :: OutBox -> Visibility -> String -> IO ()
instructionMessage ob visible instr = emit ob visible (instr ++ "\n")

infoMessage :: String -> StateT GameState IO ()
infoMessage info = do
    ob <- gets outBox
    liftIO $ emit ob ToAll (info ++ "\n")

inlineMessage :: OutBox -> Visibility -> String -> IO ()
inlineMessage ob visible msg = emit ob visible msg


printYRWires :: GameState -> String
printYRWires gs =
    let colorMarks = colorMarkers gs
    in if null colorMarks 
        then "(none)" 
        else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks)
