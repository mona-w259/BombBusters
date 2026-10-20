module GameLoop where

import Control.Monad.State
import Data.Time.Clock (getCurrentTime)
import BenchmarkLog (writeGameLog)
import GameLogic.Actions (handleAction)
import GameLogic.Core
import GameLogic.Display (displayPlayerView, displayTurnResult, displayBotDecision, announceHumanTurn)
import GameLogic.Input (handlePlaceInfoMarker, handleChooseBots)
import GameLogic.Util
import BotLogic.Interface
import Types
import Output

runGame :: GameState -> IO ()
runGame gs = do
    startT   <- getCurrentTime
    finalGs  <- execStateT startGame gs
    endT     <- getCurrentTime
    writeGameLog startT endT finalGs

startGame :: GameM ()
startGame = do
  gs <- get
  botIds <- liftIO $ (handleChooseBots gs (turnOrder gs))
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
    Detonated -> modify (\gsEnd -> gsEnd { endState = 0 }) >> syncState
    Defused   -> modify (\gsEnd -> gsEnd { endState = 1 }) >> syncState
    WaitingForChoice pc -> handlePendingChoice pc >> gameLoop
    InProgress -> do
      modify checkActivePlayers
      modify unlockEquipment
      syncState
      gs' <- get
      let pid = currentPlayer gs'
          pv = getPlayerView pid gs'
          isFirstRound = roundCount gs' < length (players gs')
          
      if isFirstRound
        then do
          act <- if isBot (pvSelf pv)
            then do
              displayPlayerView pv
              let a = botPlaceMarker pv
              displayBotDecision (Just (toPublicAction a)) []
              pure a
            else announceHumanTurn pv >> handlePlaceInfoMarker pv
          _ <- handleAction pv act id
          pure ()
        else do
          outcome <- playerTurn pv
          gs'' <- get
          case status gs'' of
            Detonated -> pure ()
            _ -> displayTurnResult outcome

      modify advanceTurn
      syncState
      gameLoop
