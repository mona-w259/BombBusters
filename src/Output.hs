module Output where

import Control.Monad.State
import Types
import OutBox


colorEmoji :: WireColor -> String
colorEmoji Blue = "🔵"
colorEmoji Yellow = "🟡"
colorEmoji Red = "🔴"
colorEmoji Grey = "⚫"
colorEmoji Hidden = "⬜"


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
