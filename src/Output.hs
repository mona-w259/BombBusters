module Output where

import Control.Monad.State
import Types


errorMessage :: String -> a
errorMessage err = error err

instructionMessage :: String -> IO ()
instructionMessage instr = putStrLn instr

infoMessage :: String -> StateT GameState IO ()
infoMessage info = liftIO $ putStrLn info

inlineMessage :: String -> IO ()
inlineMessage msg = putStr msg