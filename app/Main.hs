{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad 
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.Aeson (object, (.=), Value, FromJSON(..), withObject, (.:))
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import GameLoop
import InitGame
import WaitingRoom
import Types
import Output
import GameLogic.Display
import GameLogic.Util
import GameLogic.InBox

-------------- data types, instances --------------
data InputReq = InputReq {
  irSlot :: Int, 
  irText :: String
  }

instance FromJSON InputReq where
  parseJSON = withObject "InputReq" $ \obj -> InputReq <$> obj .: "slot" <*> obj .: "text"

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
      cd  = codeWR wr
  gs <- initGameState l ml cd (gamesGS wrs)
  registerGame wrs cd gs
  void . forkIO $ runGame gs

errorText :: JoinGameError -> TL.Text
errorText NoSuchGame = "Kein Spiel mit diesem Code gefunden"
errorText GameFull   = "Das Spiel ist bereits voll"
errorText GameClosed = "Das Spiel wurde bereits gestartet oder ist abgelaufen"

gsStatusText :: GameStatus -> String
gsStatusText InProgress           = "in progress"
gsStatusText (WaitingForChoice _) = "waiting for choice"
gsStatusText Detonated            = "detonated"
gsStatusText Defused              = "defused"

wrStatusText :: WaitingRoomStatus -> String
wrStatusText Waiting = "waiting"
wrStatusText Started = "started"
wrStatusText Expired = "expired"

rackFormatJson :: [InfoMarker] -> Player -> [Value]
rackFormatJson infos pl = 
  [ object
    [ "index"   .= wireIndex ws
    , "label"   .= indexToLabel (wireIndex ws)
    , "color"   .= colorToString (color ws)
    , "value"   .= val
    , "isCut"   .= isCut ws
    , "hasHint" .= (emoji == "💡")
    ]
    | ws <- playerWires pl, let (emoji, val) = wireFace infosForPlayer ws
  ]
  where
    infosForPlayer = 
      [ (idx, (val, col)) | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos, owner == playerId pl ]
    colorToString :: WireColor -> String
    colorToString Blue   = "blue"
    colorToString Yellow = "yellow"
    colorToString Red    = "red"
    colorToString Grey   = "grey"
    colorToString Hidden = "hidden"



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
              "status"        .= wrStatusText (WaitingRoom.status wr),
              "registered"    .= reg_playersWR wr,
              "humans"        .= h_playersWR wr,
              "secondsLeft"   .= max 0 (floor (diffUTCTime (deadline wr) now) :: Int)
            ]
      
      get "/api/game/:code/yrwires" $ do
        cd <- pathParam "code"      :: ActionM String
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just gs -> do 
            json (object ["text" .= printYRWires gs])
      
      get "/api/game/:code/detonator" $ do
        cd <- pathParam "code"      :: ActionM String
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just gs -> do 
            json (object ["text" .= detonator gs])

      
      get "/api/game/:code/racks" $ do
        cd <- pathParam "code"      :: ActionM String
        sl <- queryParam "slot"     :: ActionM Int
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of 
          Nothing -> json (object ["error" .= ("no rack yet" :: String)])
          Just gs -> do
            let pv    = getPlayerView sl gs
                infos = pvInfoMarkers pv
            json $ object
              [ "own"     .= object [ "playerId" .= sl, "wires" .= rackFormatJson infos (pvSelf pv) ]
              , "others"  .= [ object [ "playerId" .= playerId p, "wires" .= rackFormatJson infos p ]     | p <- pvOthers pv ]
              ]

      get "/api/game/:code/turn" $ do
        cd <- pathParam "code"      :: ActionM String
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just gs -> do
            json (object ["text" .= currentPlayer gs])

      get "/api/game/:code/actionmessage" $ do
        cd <- pathParam "code"      :: ActionM String
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just gs -> do
            json (object ["text" .= actionMsg gs])

      get "/api/game/:code/resultmessage" $ do
        cd <- pathParam "code"      :: ActionM String
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just gs -> do
            json (object ["text" .= resultMsg gs])
      
      get "/api/game/:code/statusgame" $ do
        cd <- pathParam "code"      :: ActionM String
        wr <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["error" .= ("no such game" :: String)])
          Just gs -> do
            json (object ["text" .= gsStatusText (Types.status gs)])

      post "/api/game/:code/input" $ do
        cd  <- pathParam "code"      :: ActionM String
        req <- jsonData
        wr  <- liftIO (atomically (Map.lookup cd <$> readTVar (gamesGS wrs)))
        case wr of
          Nothing -> json (object ["ok" .= False, "error" .= ("no such game" :: String)])
          Just gs -> do
            ok <- liftIO (pushInput (inBox gs) (irSlot req) (irText req))
            json $ if ok
              then object ["ok" .= True]
              else object ["ok" .= False, "error" .= ("unknown player" :: String)]