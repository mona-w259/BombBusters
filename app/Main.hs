{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as MS
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Random (randomRIO)
import Text.Printf (printf)


import GameLoop
import InitGame


page :: FilePath -> ActionM ()
page name = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file ("./frontend/" <> name)

validatePL :: Int -> Int -> Int -> Maybe String
validatePL h l ml
  | total < 4   = Just "The amount of players is less then 4!"
  | total > 4   = Just "The amount of players is more then 4!"
  | otherwise   = Nothing
  where total = h + l + ml

codeGenerator :: IO String
codeGenerator = do
  n <- randomRIO (0, 9999 :: Int)
  return (printf "%04d" n)


main :: IO ()
main = do
    games <- newTVarIO MS.empty          -- Spielcode -> GameState
    putStrLn $ "http://localhost:3000/"
    scotty 3000 $ do
      middleware (staticPolicy (addBase "frontend"))


      get "/"         $ page "homepage.html"
      get "/newGame"  $ page "newGame.html"
      get "/joinGame" $ page "joinGame.html"
      get "/waiting"  $ page "waiting.html"
      get "/game"     $ page "game.html"


      post "/"        $ page "homepage.html"

      post "/newGame" $ do
        players   <- formParam "players"    :: ActionM Int
        logic     <- formParam "logic_bots" :: ActionM Int
        ml        <- formParam "ml_bots"    :: ActionM Int
        case (validatePL players logic ml) of 
          Just err -> 
            redirect ("/newGame?error=" <> TL.pack err)
          Nothing -> do
            -- Spiel in `games` ablegen
            code <- liftIO codeGenerator
            redirect "/waiting"
            gameState <- liftIO (initGameState logic ml code)
            liftIO (runGame gameState)
      
      post "/joinGame" $ do
        code      <- formParam "game_code"  :: ActionM String
        -- Code in `games` nachschlagen, sonst Fehlerseite
        redirect "/waiting"
