module Output where

import Control.Monad.State
import Control.Concurrent.STM
import qualified Data.Map as Map
import Types


syncState :: GameM ()
syncState = do
  gs <- get
  liftIO $ atomically $ modifyTVar' (syncVar gs) (Map.insert (code gs) gs)

errorMessage :: String -> a
errorMessage err = error err

tellAction :: PlayerId -> String -> GameM ()
tellAction pid msg = modify (\gs -> gs { actionMsg = Map.insert pid msg (actionMsg gs) }) >> syncState

tellResult :: PlayerId -> String -> GameM ()
tellResult pid msg = modify (\gs -> gs { resultMsg = Map.insert pid msg (resultMsg gs) }) >> syncState

tellAllExceptOneAction :: PlayerId -> String -> GameM ()
tellAllExceptOneAction pid msg = modify (\gs -> gs { actionMsg = Map.fromList [(p, msg) | p <- turnOrder gs, p /= pid] }) >> syncState

tellAllAction :: String -> GameM ()
tellAllAction msg = modify (\gs -> gs { actionMsg = Map.fromList [(p, msg) | p <- turnOrder gs] }) >> syncState

tellAllResult :: String -> GameM ()
tellAllResult msg = modify (\gs -> gs { resultMsg = Map.fromList [(p, msg) | p <- turnOrder gs] }) >> syncState

printYRWires :: GameState -> String
printYRWires gs =
    let colorMarks = colorMarkers gs
    in if null colorMarks 
        then "(none)" 
        else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks)
