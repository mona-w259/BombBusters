module WaitingRoom where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (void)
import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Random (randomRIO)
import Text.Printf (printf)
import Types (Code, GameState)


---------- Data Types ----------
data WaitingRoomStatus = Waiting | Started | Expired
  deriving (Eq, Show)

data WaitingRoom = WaitingRoom {
  codeWR        :: Code,
  h_playersWR   :: Int,
  reg_playersWR :: Int,
  l_playersWR   :: Int,
  ml_playersWR  :: Int,
  status        :: WaitingRoomStatus,
  deadline      :: UTCTime
}

data WaitingRooms = WaitingRooms {
  waitingRooms :: TVar (Map.Map Code WaitingRoom),
  gamesGS :: TVar (Map.Map Code GameState)
}

data JoinGameError = NoSuchGame | GameFull | GameClosed
  deriving Show

data WatchResult = RoomBecameFull | RoomTimedOut
  deriving (Show)



-- max. waiting time: 10 min
maxWait :: Int
maxWait = 10 * 60

newWaitingRooms :: IO WaitingRooms
newWaitingRooms = WaitingRooms <$> newTVarIO Map.empty <*> newTVarIO Map.empty

codeGenerator :: IO Code
codeGenerator = do
  n <- randomRIO (0, 9999 :: Int)
  return (printf "%04d" n)

validateCode :: WaitingRooms -> WaitingRoom -> Code -> IO Bool
validateCode wrs wr code = atomically $ do
  rooms <- readTVar (waitingRooms wrs)
  if Map.member code rooms
    then return False
    else writeTVar (waitingRooms wrs) (Map.insert code wr rooms) >> return True

lookupWaitingRoom :: WaitingRooms -> Code -> STM (Maybe WaitingRoom)
lookupWaitingRoom wrs code = Map.lookup code <$> readTVar (waitingRooms wrs)

isStarted :: WaitingRooms -> Code -> STM Bool
isStarted wrs code = maybe False ((== Started) . status) <$> lookupWaitingRoom wrs code

registerGame :: WaitingRooms -> Code -> GameState -> IO ()
registerGame wrs code gs = do
  atomically $ modifyTVar' (gamesGS wrs) (Map.insert code gs)

setStatus :: WaitingRooms -> Code -> WaitingRoomStatus -> STM ()
setStatus wrs code newStatus = 
  modifyTVar' (waitingRooms wrs) (Map.adjust (\wr -> wr {status = newStatus}) code)



createWaitingRoom :: WaitingRooms -> Int -> Int -> Int -> IO Code
-- no humans are playing -> no waitingRoom necessary
-- createWaitingRoom _ 0 _ _ = codeGenerator
-- only 1 human is playing -> no waitingRoom necessary
-- createWaitingRoom _ 1 _ _ = codeGenerator
-- more than 1 human is playing -> waitingRoom is necessary
createWaitingRoom wrs h l ml = do
  timeNow <- getCurrentTime
  code    <- codeGenerator
  let deadl = addUTCTime (fromIntegral maxWait) timeNow
      wr    = WaitingRoom code h 1 l ml Waiting deadl
  isValid <- validateCode wrs wr code
  if isValid 
    then return code 
    else createWaitingRoom wrs h l ml


joinWaitingRoom :: WaitingRooms -> String -> STM (Either JoinGameError (Code, Int))
joinWaitingRoom wrs rawCode = do
  let code = map toUpper rawCode
  rooms <- readTVar (waitingRooms wrs)
  result <- lookupWaitingRoom wrs code
  case result of
    Nothing -> return (Left NoSuchGame)
    Just wr 
      | status wr /= Waiting                -> return (Left GameClosed)
      | reg_playersWR wr >= h_playersWR wr  -> return (Left GameFull)
      | otherwise -> do
          let slot = ((4 - h_playersWR wr) + reg_playersWR wr ) +1 -- % 4 +1     -- let bots be first slots, human players last ones
          writeTVar (waitingRooms wrs) (Map.insert code wr {reg_playersWR = slot} rooms)
          return (Right (code, slot))


watchWR :: WaitingRooms -> Code -> (WaitingRoom -> IO ()) -> IO WatchResult
watchWR wrs code funcWhenFull = do
  timeout <- registerDelay (maxWait * 1000000)

  -- watch until a waitingRoom is full or the code is expired
  result  <- atomically $
    (
      do 
        room <- lookupWaitingRoom wrs code
        case room of 
          Just wr | reg_playersWR wr >= h_playersWR wr -> return (RoomBecameFull, Just wr)
          _ -> retry
    )
    `orElse`
    (
      do 
        readTVar timeout >>= check
        return (RoomTimedOut, Nothing)
    )
  
  -- return if room is full or code is expired
  case result of
    (RoomBecameFull, Just wr) -> do
      funcWhenFull wr
      atomically (setStatus wrs code Started)
      return RoomBecameFull
    _ -> do
      atomically (setStatus wrs code Expired)
      void . forkIO $ do
        threadDelay (2 * 60 * 1000000)
        atomically (modifyTVar' (waitingRooms wrs) (Map.delete code))
      return RoomTimedOut