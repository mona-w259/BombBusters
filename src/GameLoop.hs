module GameLoop where

import Control.Monad.State
import Data.Time.Clock (getCurrentTime)
import BenchmarkLog (writeGameLog)
import GameLogic.Actions (handleAction)
import GameLogic.Core
import GameLogic.Display (displayTurnResult, displayDecision, announceHumanTurn)
import GameLogic.Input (handlePlaceInfoMarker, handleChooseBots)
import GameLogic.Util
import GameLogic.Output
import BotLogic.Interface
import Config (bbDelay)
import Types

runGame :: GameState -> IO ()
runGame gs = do
    startT   <- getCurrentTime
    finalGs  <- execStateT startGame gs
    endT     <- getCurrentTime
    writeGameLog startT endT finalGs

startGame :: GameM ()
startGame = do
  gs <- get
  botIds <- liftIO $ (handleChooseBots gs [1..4])
  modify $ selectBots botIds
  syncState
  gameLoop

gameLoop :: GameM ()
gameLoop = do
  modify evaluateGameEnd
  syncState
  gs <- get
  let currStatus = status gs
  case currStatus of
    Detonated -> pure ()
    Defused   -> pure ()
    _ -> do
      modify checkActivePlayers
      modify unlockEquipment
      syncState
      gs' <- get
      let pid = currentPlayer gs'
          pv = getPlayerView pid gs'
          isFirstRound = roundCount gs' < length (players gs')
          
      if isFirstRound
        then do
          liftIO (bbDelay 1000000)
          act <- if isBot (pvSelf pv)
            then do
              let a = botPlaceMarker pv
              displayDecision (Just (toPublicAction a)) []
              pure a
            else do
              announceHumanTurn pv 
              tellAllExceptOneAction pid ("Player " ++ show pid ++ " is thinking...")
              handlePlaceInfoMarker pv
          _ <- handleAction pv act id
          pure ()
        else do
          outcome <- playerTurn pv
          gs'' <- get
          case status gs'' of
            Detonated -> pure ()
            WaitingForChoice pc -> handlePendingChoice pc >> displayTurnResult outcome
            _ -> displayTurnResult outcome
      setRackToChooseFrom 0 0
      liftIO (bbDelay 3000000)
      modify advanceTurn >> syncState
      gameLoop
