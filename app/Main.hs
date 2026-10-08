{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Web.Scotty
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)


import GameLoop
import InitGame

{-
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  gameState <- initGameState
  runGame gameState
-}


homepage :: Response
homepage = responseFile
    status200
    [("Content-Type", "text/html")]
    "./frontend/homepage.html"
    Nothing
    
newGame :: Response
newGame = responseFile
    status200
    [("Content-Type", "text/html")]
    "./frontend/newGame.html"
    Nothing
        
joinGame :: Response
joinGame = responseFile
    status200
    [("Content-Type", "text/html")]
    "./frontend/joinGame.html"
    Nothing
        
game :: Response
game = responseFile
    status200
    [("Content-Type", "text/html")]
    "./frontend/game.html"
    Nothing
        
waiting :: Response
waiting = responseFile
    status200
    [("Content-Type", "text/html")]
    "./frontend/waiting.html"
    Nothing

stylesheet :: Response
stylesheet = responseFile
    status200
    [("Content-Type", "text/css; charset=utf-8")]
    "./frontend/style.css"
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/html")]
    "404 - Not Found"



webPage :: Application
webPage request respond = respond $ case rawPathInfo request of
    "/"           -> homepage
    "/newGame"    -> newGame
    "/joinGame"   -> joinGame
    "/game"       -> game
    "/waiting"    -> waiting
    "/style.css"  -> stylesheet
    _             -> Main.notFound

main :: IO ()
main = do
    putStrLn $ "http://localhost:3000/"
    run 3000 webPage
