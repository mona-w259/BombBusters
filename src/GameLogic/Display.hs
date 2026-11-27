module GameLogic.Display (displayTurnResult, displayDecision, announceHumanTurn, indexToLabel, wireFace, showAction) where

import Control.Monad (unless, when)
import Control.Monad.State
import Data.List (intercalate, sort)
import Data.Maybe (catMaybes)
import GameLogic.Output
import Types
import Config (bbDelay)

indexToLabel :: Int -> String
indexToLabel i = [toEnum (fromEnum 'a' + i)]

announceHumanTurn :: PlayerView -> GameM ()
announceHumanTurn pv = do
  let self = pvSelf pv
      multiHuman = any (not . isBot) (pvOthers pv)
  when (not (isBot self) && multiHuman) $ do
    modify (\gs -> gs { currentPlayer = playerId self })
    syncState
    liftIO (bbDelay 200000)

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
              CutSuccess w -> "Successful cut of " ++ showCallOut w ++ "!"
              _ -> "Oops! Wrong wire..."
  tellAllResult msg
  liftIO (bbDelay 500000)

displayDecision :: Maybe PublicAction -> [PublicEquipmentUse] -> GameM ()
displayDecision act equip = do
  let equipText = case equip of
        [] -> Nothing
        _  -> Just ("Equipment: " ++ intercalate ", " (map showEquipUse equip))
      parts = catMaybes [equipText, fmap showAction act]

  unless (null parts) $ do
    tellAllAction (intercalate " | " parts)

showAction :: PublicAction -> String
showAction (PubSoloCut2 w (a, b)) =
  "Solo Cut 2 - " ++ showCallOut w ++ " (" ++ intercalate "., " (map show $ map (+1) (sort [a, b])) ++ ". wire)"
showAction (PubSoloCut4 w (a, b, c, d)) =
  "Solo Cut 4 - " ++ showCallOut w ++ " (" ++ intercalate "., " (map show $ map (+1) (sort [a, b, c, d])) ++ ". wire)" 
showAction (PubRemoveRed w) =
  "Remove Red - " ++ showCallOut w ++ " (" ++ show ((wireIndex w) + 1) ++ ". wire)"
showAction (PubDualCut pid targetIdx w) =
  "Dual Cut - called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ "'s " ++ show (targetIdx + 1) ++ ". wire"
showAction (PubPlaceInfoMarker pl idx) = do
  let w = (playerWires pl) !! idx
  "Player " ++ show (playerId pl) ++ " placed an Info Marker on value " ++ showCallOut w ++ " (" ++ show (idx + 1) ++ ". wire)"

showEquipUse :: PublicEquipmentUse -> String
showEquipUse PubUseStabilizer =
  "Stabilizer"
showEquipUse (PubUseSuperDetector pid w) =
  "SuperDetector called " ++ showCallOut w 
    ++ " → Player " ++ show pid
showEquipUse (PubUseTripleDetector pid (a, b, c) w) =
  "TripleDetector called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ " (" ++ intercalate "., " (map show $ map (+1) (sort [a, b, c])) ++ ". wire)"
showEquipUse (PubUseDoubleDetector pid (a, b) w) =
  "DoubleDetector called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ " (" ++ intercalate "., " (map show $ map (+1) (sort [a, b])) ++ ". wire)"
showEquipUse (PubUseDoubleRadiator pid tgt (w1, w2)) =
  "DoubleRadiator called " ++ showCallOut w1 ++ ", " ++ showCallOut w2
    ++ " → Player " ++ show pid ++ " (" ++ show (tgt + 1) ++ ". wire)"

showCallOut :: Wire -> String
showCallOut w = case color w of
  Yellow -> colorEmoji Yellow
  Red    -> colorEmoji Red ++ "(" ++ show (value w) ++ ".5)"
  _      -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")"
