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


printYRWires :: GameState -> String
printYRWires gs =
    let colorMarks = colorMarkers gs
    in if null colorMarks 
        then "(none)" 
        else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks)
