{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Web.Scotty
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL

import GameLoop
import InitGame

main :: IO ()
main = do 
  -- generate initial state at beginning
  gameState <- initGameState
  ref <- newIORef gameState
  
  -- create scotty web server on port 3000  
  scotty 3000 $ do
  
    -- HTTP GET requests 

    -- HOMEPAGE - choose if you wanna create or join a game - route "/"
    get "/" $ do
      gameSt <- liftIO (readIORef ref)
      html $ TL.concat
        [
          "<h1>Hello!</h1>",
          "<h2>Do you want to start a game or do you want to join a game?</h1>",
          "<form method='get' action='/newGame'><button>New Game</button></form>",
          "<form method='get' action='/joinGame'><button>Join Game</button></form>"
        ]
    
    -- creation of a new game - route "/newGame"
    get "/newGame" $ do
      html $ TL.concat
        [ 
          "<h1>Create your Game</h1>", 
--          "<pre>", TL.pack (show gameSt), "</pre>",
          "<form method='post' action='/exit'><button>Exit</button></form>"
        ]
      gameSt <- liftIO initGameState
      liftIO (writeIORef ref gameSt)
      -- runGame gameState

    -- join a game - route "/joinGame"
    get "/joinGame" $ do
      html $ TL.concat
        [ 
          "<h1>Enter your Gaming Code here:</h1>", 
          "<form method='post' action='/exit'><button>Exit</button></form>"
        ]

    -- wait until all human playyers entered the game - route "/waiting"
    get "/waiting" $ do
      html $ TL.concat
        [ 
          "<h1>Wait until all players have entered the game...</h1>", 
          "<form method='post' action='/exit'><button>Exit</button></form>"
        ]

    -- ongoing game - route "/gameCODE"
    get "/game$$$$" $ do 
      html $ TL.concat
        [
          "<h1>show game</h1>",
          "<form method='post' action='/exit'><button>Exit</button></form>"
        ]



    -- HTTP POST requests
    
    -- exit
    post "/exit" $ redirect "/"
