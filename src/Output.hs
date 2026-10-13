module Output where

import Control.Monad.State
import Types
import OutBox


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