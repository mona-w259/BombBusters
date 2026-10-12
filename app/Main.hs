{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as MS
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Random (randomRIO)
import Text.Printf (printf)
import Data.Aeson (object, (.=))
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import GameLoop
import InitGame
import WaitingRoom
import Types

----------------- small functions -----------------
page :: FilePath -> ActionM ()
page name = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file ("./frontend/" <> name)

waitingURL :: Code -> Int -> TL.Text
waitingURL code slot = TL.pack("/waiting?code=" ++ code ++ "&slot=" ++ show slot)

validatePL :: Int -> Int -> Int -> Maybe String
validatePL h l ml
  | total < 4   = Just "The amount of players is less then 4!"
  | total > 4   = Just "The amount of players is more then 4!"
  | otherwise   = Nothing
  where total = h + l + ml

prepareGame :: WaitingRooms -> WaitingRoom -> IO ()
prepareGame wrs wr = do
  let l   = l_playersWR wr
      ml  = ml_playersWR wr
  gs <- initGameState l ml (codeWR wr)
  registerGame wrs (codeWR wr) gs
  void . forkIO $ runGame gs

errorText :: JoinGameError -> TL.Text
errorText NoSuchGame = "Kein Spiel mit diesem Code gefunden"
errorText GameFull   = "Das Spiel ist bereits voll"
errorText GameClosed = "Das Spiel wurde bereits gestartet oder ist abgelaufen"



------------------ main function ------------------
main :: IO ()
main = do
    wrs     <- newWaitingRooms
    putStrLn $ "http://localhost:3000/"

    scotty 3000 $ do
      middleware (staticPolicy (addBase "frontend"))


      -------------- GET REQUESTS --------------
      get "/"         $ page "homepage.html"
      get "/newGame"  $ page "newGame.html"
      get "/joinGame" $ page "joinGame.html"
      get "/waiting"  $ page "waiting.html"
      
      get "/game"     $ do
        code <- queryParam "code" :: ActionM String
        slot <- queryParam "slot" :: ActionM Int
        started <- liftIO (atomically (isStarted wrs code))
        if started 
          then page "game.html"
          else redirect (waitingURL code slot)




      -------------- POST REQUESTS -------------
      post "/"        $ page "homepage.html"

      post "/newGame" $ do
        humans    <- formParam "humans"     :: ActionM Int
        logic     <- formParam "logic_bots" :: ActionM Int
        ml        <- formParam "ml_bots"    :: ActionM Int
        case (validatePL humans logic ml) of 
          Just err -> redirect ("/newGame?error=" <> TL.pack err)
          Nothing -> do
            code <- liftIO (createWaitingRoom wrs humans logic ml)
            _ <- liftIO . forkIO . void $ watchWR wrs code (prepareGame wrs)
            redirect (waitingURL code 1)
      
      post "/joinGame" $ do
        code      <- formParam "game_code"  :: ActionM String
        result <- liftIO (atomically (joinWaitingRoom wrs code))
        case result of
          Left err            -> redirect ("/joinGame?error=" <> errorText err)
          Right (code, slot)  -> redirect (waitingURL code slot)



      ---------------- API PAGE ----------------
      get "/api/waitingRoom/:code" $ do
        code  <- pathParam "code"    :: ActionM String
        now   <- liftIO getCurrentTime
        room  <- liftIO (atomically (lookupWaitingRoom wrs code))
        case room of
          Nothing   -> json (object ["status" .= ("missing" :: String)])
          Just wr   -> json $ object
            [
              "status"        .= statusName (WaitingRoom.status wr),
              "registered"    .= reg_playersWR wr,
              "humans"        .= h_playersWR wr,
              "secondsLeft"   .= max 0 (floor (diffUTCTime (deadline wr) now) :: Int)
            ]
      where 
        statusName :: WaitingRoomStatus -> String
        statusName Waiting = "waiting"
        statusName Started = "started"
        statusName Expired = "expired"