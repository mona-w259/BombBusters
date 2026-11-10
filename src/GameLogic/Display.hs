module GameLogic.Display (displayPlayerView, displayTurnResult, displayDecision, announceHumanTurn, indexToLabel, wireFace) where

import Control.Monad (unless, when)
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Types
import Config (bbDelay)
import Output

indexToLabel :: Int -> String
indexToLabel i = [toEnum (fromEnum 'a' + i)]

displayPlayerView :: PlayerView -> GameM ()
displayPlayerView pv = do
  let self = pvSelf pv
      pid = playerId self
      botStatus = isBot self
      header = "Player " ++ show pid ++ " isBot: " ++ show botStatus
      equipText = "\nAvailable Equipment: " ++ equipmentSummary pv

  tellAction pid (header ++ equipText)

equipmentSummary :: PlayerView -> String
equipmentSummary pv =
  let availEq = [ show (name e) | e <- pvEquipment pv, unlocked e, not (used e) ]
      dd = [ "DoubleDetector" | doubleDetector (pvSelf pv) ]
      items = availEq ++ dd
  in if null items then "(none)" else intercalate ", " items

announceHumanTurn :: PlayerView -> GameM ()
announceHumanTurn pv = do
  let self = pvSelf pv
      multiHuman = any (not . isBot) (pvOthers pv)
  when (not (isBot self) && multiHuman) $ do
    modify (\gs -> gs { currentPlayer = playerId self })
    syncState
    liftIO (bbDelay 3000000)

wireFace :: [(Int, (Int, WireColor))] -> Wire -> (String, String)
wireFace infos w = 
  let maybeInfo = lookup (wireIndex w) infos
  in case (color w, maybeInfo) of
    (Grey, _)                   -> ("⚫", show (value w))
    (Hidden, Just (_, Yellow))  -> ("💡", "🟡")
    (Hidden, Just (v, _))       -> ("💡", show v)
    (c, Just (v, _))            -> (colorEmoji c, show v ++ "💡")
    (Hidden, Nothing)           -> ("⬜", "?")
    (c, Nothing)                -> (colorEmoji c, show (value w))

displayTurnResult :: Outcome -> GameM ()
displayTurnResult ActionDone = pure ()
displayTurnResult outcome = do
  let msg = case outcome of
              CutSuccess -> "Successful cut!"
              _ -> "Oops! Wrong wire..."
  tellAllResult msg
  liftIO (bbDelay 3000000)

displayDecision :: Maybe PublicAction -> [PublicEquipmentUse] -> GameM ()
displayDecision act equip = do
  let equipText = case equip of
        [] -> Nothing
        _  -> Just ("Equipment: " ++ intercalate ", " (map showEquipUse equip))
      parts = catMaybes [equipText, fmap showBotAction act]

  unless (null parts) $ do
    tellAllAction (intercalate " | " parts)
    liftIO (bbDelay 1500000)

showBotAction :: PublicAction -> String
showBotAction (PubSoloCut2 (a, b)) =
  "Solo Cut 2 - " ++ show (a + 1) ++ "., " ++ show (b + 1) ++ ". wire"
showBotAction (PubSoloCut4 (a, b, c, d)) =
  "Solo Cut 4 - " ++ intercalate "., " (map show $ map (+1) [a, b, c, d]) ++ ". wire" 
showBotAction (PubRemoveRed idx) =
  "Remove Red - " ++ show (idx + 1) ++ ". wire"
showBotAction (PubDualCut pid targetIdx w) =
  "Dual Cut - called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ "'s " ++ show (targetIdx + 1) ++ ". wire"
showBotAction (PubPlaceInfoMarker idx) =
  "Placed Info Marker on " ++ show (idx + 1) ++ ". wire"

showEquipUse :: PublicEquipmentUse -> String
showEquipUse PubUseStabilizer =
  "Stabilizer"
showEquipUse (PubUseSuperDetector pid w) =
  "SuperDetector called " ++ showCallOut w 
    ++ " → Player " ++ show pid
showEquipUse (PubUseTripleDetector pid (a, b, c) w) =
  "TripleDetector called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ " [" ++ intercalate "., " (map show $ map (+1) [a, b, c]) ++ ". wire]"
showEquipUse (PubUseDoubleDetector pid (a, b) w) =
  "DoubleDetector called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ " [" ++ intercalate "., " (map show $ map (+1) [a, b]) ++ ". wire]"
showEquipUse (PubUseDoubleRadiator pid tgt (w1, w2)) =
  "DoubleRadiator called " ++ showCallOut w1 ++ ", " ++ showCallOut w2
    ++ " → Player " ++ show pid ++ " [" ++ show (tgt + 1) ++ ". wire]"

showCallOut :: Wire -> String
showCallOut w = case color w of
  Yellow -> colorEmoji Yellow
  _      -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")"
