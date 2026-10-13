{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad 
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.Aeson (object, (.=))
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import GameLoop
import InitGame
import WaitingRoom
import Types
import OutBox

----------------- small functions -----------------
page :: FilePath -> ActionM ()
page pName = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file ("./frontend/" <> pName)

waitingURL :: Code -> Int -> TL.Text
waitingURL cd slot = TL.pack("/waiting?code=" ++ cd ++ "&slot=" ++ show slot)

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
  registerGame wrs (codeWR wr) (outBox gs)
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
        cd <- queryParam "code" :: ActionM String
        slot <- queryParam "slot" :: ActionM Int
        started <- liftIO (atomically (isStarted wrs cd))
        if started 
          then page "game.html"
          else redirect (waitingURL cd slot)




      -------------- POST REQUESTS -------------
      post "/"        $ page "homepage.html"

      post "/newGame" $ do
        humans    <- formParam "humans"     :: ActionM Int
        logic     <- formParam "logic_bots" :: ActionM Int
        ml        <- formParam "ml_bots"    :: ActionM Int
        case (validatePL humans logic ml) of 
          Just err -> redirect ("/newGame?error=" <> TL.pack err)
          Nothing -> do
            cd <- liftIO (createWaitingRoom wrs humans logic ml)
            _ <- liftIO . forkIO . void $ watchWR wrs cd (prepareGame wrs)
            redirect (waitingURL cd 1)
      
      post "/joinGame" $ do
        cd     <- formParam "game_code"  :: ActionM String
        result <- liftIO (atomically (joinWaitingRoom wrs cd))
        case result of
          Left err            -> redirect ("/joinGame?error=" <> errorText err)
          Right (cdR, slot)  -> redirect (waitingURL cdR slot)



      ---------------- API PAGE ----------------
      get "/api/waitingRoom/:code" $ do
        cd    <- pathParam "code"    :: ActionM String
        now   <- liftIO getCurrentTime
        room  <- liftIO (atomically (lookupWaitingRoom wrs cd))
        case room of
          Nothing   -> json (object ["status" .= ("missing" :: String)])
          Just wr   -> json $ object
            [
              "status"        .= statusName (WaitingRoom.status wr),
              "registered"    .= reg_playersWR wr,
              "humans"        .= h_playersWR wr,
              "secondsLeft"   .= max 0 (floor (diffUTCTime (deadline wr) now) :: Int)
            ]
      
      get "/api/game/:code/output" $ do
        cd    <- pathParam "code"  :: ActionM String
        slot  <- queryParam "slot"  :: ActionM Int
        since <- queryParam "since" :: ActionM Int
        wr    <- liftIO (atomically (Map.lookup cd <$> readTVar (games wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just ob -> do
            (next, msgs) <- liftIO (atomically (messagesSince ob since slot))
            json (object ["next" .= next, "messages" .= map msgText msgs])

      
      where 
        statusName :: WaitingRoomStatus -> String
        statusName Waiting = "waiting"
        statusName Started = "started"
        statusName Expired = "expired"