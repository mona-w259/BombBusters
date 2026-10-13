module GameLogic.Display (displayPlayerView, displayWirePickView, displayInfoMarkerView, displayTurnResult, displayBotDecision, announceHumanTurn, indexToLabel) where

import Control.Monad (unless, when)
import Data.List (intercalate)
import Types
import Config (bbDelay, bbClearScreen)
import Output
import OutBox

indexToLabel :: Int -> String
indexToLabel i = [toEnum (fromEnum 'a' + i)]

displayPlayerView :: PlayerView -> IO ()
displayPlayerView pv = do
  let self = pvSelf pv
      pid = playerId self
      det = pvDetonator pv
      colorMarks = pvColorMarkers pv
      cutWs = pvCutWires pv
      allInfos = pvInfoMarkers pv
      others = pvOthers pv
      botStatus = isBot self
      hasHumanOnlooker = any (not . isBot) others
      displaySelf
        | botStatus && hasHumanOnlooker = censorPlayerWires self
        | otherwise = self

  bbClearScreen

  instructionMessage (pvOutBox pv) (ToPlayer pid) "\n============================================="
  instructionMessage (pvOutBox pv) (ToPlayer pid) $ "PLAYER " ++ show pid ++ " isBot: " ++ show botStatus ++ " - Detonator: " ++ show det
  instructionMessage (pvOutBox pv) (ToPlayer pid) "============================================="
  instructionMessage (pvOutBox pv) (ToPlayer pid) $
    "Color Markers: "
      ++ (if null colorMarks then "(none)" else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks))
  instructionMessage (pvOutBox pv) (ToPlayer pid) $ "Unlocking Values: " ++ unlockingSummary pv
  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "YOUR RACK"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
  printRack (pvOutBox pv) self allInfos
  instructionMessage (pvOutBox pv) (ToPlayer pid) "   ⬜ = Hidden   💡 = Info   ⚫ = Cut"

  unless (null others) $ do
    instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
    instructionMessage (pvOutBox pv) (ToPlayer pid) "OTHER PLAYERS"
    instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
    mapM_ (\oth -> printRack (pvOutBox pv) oth allInfos) others

  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "CUT WIRES (All Players)"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
  if null cutWs
    then instructionMessage (pvOutBox pv) (ToPlayer pid) "   (No cuts yet.)"
    else mapM_ (printCut pv) cutWs >> instructionMessage (pvOutBox pv) (ToPlayer pid) ""

  when botStatus $ do
    instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
    instructionMessage (pvOutBox pv) (ToPlayer pid) "EQUIPMENT AVAILABLE"
    instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
    instructionMessage (pvOutBox pv) (ToPlayer pid) $ "   " ++ equipmentSummary pv

  instructionMessage (pvOutBox pv) (ToPlayer pid) "\n=============================================\n"

equipmentSummary :: PlayerView -> String
equipmentSummary pv =
  let availEq = [ show (name e) | e <- pvEquipment pv, unlocked e, not (used e) ]
      dd = [ "DoubleDetector" | doubleDetector (pvSelf pv) ]
      items = availEq ++ dd
  in if null items then "(none)" else intercalate ", " items

unlockingSummary :: PlayerView -> String
unlockingSummary pv =
  let toUnlock = [ (unlockingVal e, name e) | e <- pvEquipment pv, not (unlocked e) ]
  in case toUnlock of
        [] -> "(none)"
        xs -> intercalate ", " [show v ++ " → " ++ show eq | (v, eq) <- xs]

displayWirePickView :: PlayerView -> Maybe Player -> IO ()
displayWirePickView pv mTarget = do
  let self = pvSelf pv
      pid = playerId self
      ob = pvOutBox pv
      allInfos = pvInfoMarkers pv
      cutWs = pvCutWires pv
      colorMarks = pvColorMarkers pv
      det = pvDetonator pv

  bbClearScreen

  instructionMessage (pvOutBox pv) (ToPlayer pid) "\n---------------------------------------------"
  instructionMessage (pvOutBox pv) (ToPlayer pid) $ "Detonator: " ++ show det
  instructionMessage (pvOutBox pv) (ToPlayer pid) $
    "Color Markers: "
      ++ (if null colorMarks then "(none)" else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks))
  instructionMessage (pvOutBox pv) (ToPlayer pid) $ "Unlocking Values: " ++ unlockingSummary pv
  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "YOUR RACK"
  printRackLabeled (pvOutBox pv) self allInfos
  case mTarget of
    Nothing -> pure ()
    Just target -> do
      instructionMessage ob (ToPlayer pid) "---------------------------------------------"
      instructionMessage ob (ToPlayer pid) "TARGET RACK"
      printRackLabeled ob target allInfos
  instructionMessage (pvOutBox pv) (ToPlayer pid) "   ⬜ = Hidden   💡 = Info   ⚫ = Cut"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------"
  instructionMessage (pvOutBox pv) (ToPlayer pid) "CUT WIRES (All Players)"
  if null cutWs
    then instructionMessage (pvOutBox pv) (ToPlayer pid) "   (No cuts yet.)"
    else mapM_ (printCut pv) cutWs >> instructionMessage (pvOutBox pv) (ToPlayer pid) ""
  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------\n"

displayInfoMarkerView :: PlayerView -> IO ()
displayInfoMarkerView pv = do
  let self = pvSelf pv
      others = pvOthers pv
      allInfos = pvInfoMarkers pv
      colorMarks = pvColorMarkers pv
      det = pvDetonator pv

  bbClearScreen

  instructionMessage (pvOutBox pv) ToAll "\n---------------------------------------------"
  instructionMessage (pvOutBox pv) ToAll $ "Detonator: " ++ show det
  instructionMessage (pvOutBox pv) ToAll $
    "Color Markers: "
      ++ (if null colorMarks then "(none)" else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks))
  instructionMessage (pvOutBox pv) ToAll $ "Unlocking Values: " ++ unlockingSummary pv
  instructionMessage (pvOutBox pv) ToAll "---------------------------------------------"
  instructionMessage (pvOutBox pv) ToAll "YOUR RACK"
  printRackLabeled (pvOutBox pv) self allInfos
  unless (null others) $ do
    instructionMessage (pvOutBox pv) ToAll "---------------------------------------------"
    instructionMessage (pvOutBox pv) ToAll "OTHER PLAYERS"
    instructionMessage (pvOutBox pv) ToAll "---------------------------------------------"
    mapM_ (\oth -> printRackLabeled (pvOutBox pv) oth allInfos) others
  instructionMessage (pvOutBox pv) ToAll "   ⬜ = Hidden   💡 = Info   ⚫ = Cut"
  instructionMessage (pvOutBox pv) ToAll "---------------------------------------------"

announceHumanTurn :: PlayerView -> IO ()
announceHumanTurn pv = do
  let self = pvSelf pv
      multiHuman = any (not . isBot) (pvOthers pv)
  when (not (isBot self) && multiHuman) $ do
    bbClearScreen
    instructionMessage (pvOutBox pv) ToAll "\n============================================="
    instructionMessage (pvOutBox pv) ToAll $ "  Player " ++ show (playerId self) ++ " (" ++ playerName self ++ ") - it's your turn!"
    instructionMessage (pvOutBox pv) ToAll "============================================="
    bbDelay 3000000

censorPlayerWires :: Player -> Player
censorPlayerWires p = p { playerWires = map hide (playerWires p) }
  where
    hide w
      | color w == Grey = w
      | otherwise = w { color = Hidden, value = -1 }

printRack :: OutBox -> Player -> [InfoMarker] -> IO ()
printRack ob pl infos = do
  let pid = playerId pl
      pName = playerName pl
      pwires = playerWires pl
      infosForPlayer =
        [ (idx, (val, col))
        | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos,
          owner == pid
        ]
  instructionMessage ob (ToPlayer pid) $ "Player " ++ show pid ++ " (" ++ pName ++ "):"
  instructionMessage ob (ToPlayer pid) $ "   " ++ concatMap (formatWireOverview infosForPlayer) pwires ++ "\n"

printRackLabeled :: OutBox -> Player -> [InfoMarker] -> IO ()
printRackLabeled ob pl infos = do
  let pid = playerId pl
      pName = playerName pl
      pwires = playerWires pl
      infosForPlayer =
        [ (idx, (val, col))
        | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos,
          owner == pid
        ]
  instructionMessage ob (ToPlayer pid) $ "Player " ++ show pid ++ " (" ++ pName ++ "):"
  instructionMessage ob (ToPlayer pid) $ "   " ++ concatMap (formatWireLabeled infosForPlayer) pwires ++ "\n"

formatWireOverview :: [(Int, (Int, WireColor))] -> Wire -> String
formatWireOverview infos w =
  let maybeInfo = lookup (wireIndex w) infos
      (emoji, valStr) = case (color w, maybeInfo) of
        (Grey, _) -> ("⚫", show (value w))
        (Hidden, Just (_, Yellow)) -> ("💡", "🟡")
        (Hidden, Just (v, _)) -> ("💡", show v)
        (c, Just (v, _)) -> (colorEmoji c, show v ++ "💡")
        (Hidden, Nothing) -> ("⬜", "?")
        (c, Nothing) -> (colorEmoji c, show (value w))
      wireBox = "[" ++ emoji ++ "(" ++ valStr ++ ")" ++ "]"
   in wireBox ++ " "

formatWireLabeled :: [(Int, (Int, WireColor))] -> Wire -> String
formatWireLabeled infos w =
  let maybeInfo = lookup (wireIndex w) infos
      (emoji, valStr) = case (color w, maybeInfo) of
        (Grey, _) -> ("⚫", show (value w))
        (Hidden, Just (_, Yellow)) -> ("💡", "🟡")
        (Hidden, Just (v, _)) -> ("💡", show v)
        (c, Just (v, _)) -> (colorEmoji c, show v ++ "💡")
        (Hidden, Nothing) -> ("⬜", "?")
        (c, Nothing) -> (colorEmoji c, show (value w))
      lbl = indexToLabel (wireIndex w)
      wireBox = "[" ++ lbl ++ ":" ++ emoji ++ "(" ++ valStr ++ ")" ++ "]"
   in wireBox ++ " "

printCut :: PlayerView -> MarkedWire -> IO ()
printCut pv MarkedWire {player = _, originalIndex = _, wire = w} =
  inlineMessage (pvOutBox pv) ToAll $ colorEmoji (color w) ++ " (" ++ show (value w) ++ ") "

colorEmoji :: WireColor -> String
colorEmoji Blue = "🔵"
colorEmoji Yellow = "🟡"
colorEmoji Red = "🔴"
colorEmoji Grey = "⚫"
colorEmoji Hidden = "⬜"

displayTurnResult :: GameState -> Outcome -> IO ()
displayTurnResult _ ActionDone = pure ()
displayTurnResult gs outcome = do
  instructionMessage (outBox gs) ToAll $ case outcome of
    CutSuccess -> "Successful cut!"
    _ -> "Oops! Wrong wire..."
  bbDelay 3000000

displayBotDecision :: GameState -> Maybe PublicAction -> [PublicEquipmentUse] -> IO ()
displayBotDecision gs act equip = do
  case equip of
    [] -> pure ()
    _  -> instructionMessage (outBox gs) ToAll $ "Equipment : " ++ intercalate ", " (map showEquipUse equip)
  instructionMessage (outBox gs) ToAll $ "Action    : " ++ maybe "(equipment only)" showBotAction act
  bbDelay 1500000

showBotAction :: PublicAction -> String
showBotAction (PubSoloCut2 (a, b)) =
  "Solo Cut 2 - wires " ++ indexToLabel a ++ ", " ++ indexToLabel b
showBotAction (PubSoloCut4 (a, b, c, d)) =
  "Solo Cut 4 - wires " ++ intercalate ", " (map indexToLabel [a, b, c, d])
showBotAction (PubRemoveRed idx) =
  "Remove Red - wire " ++ indexToLabel idx
showBotAction (PubDualCut pid targetIdx w) =
  "Dual Cut - called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ "'s [" ++ indexToLabel targetIdx ++ "]"
showBotAction (PubPlaceInfoMarker idx) =
  "Place Info Marker - wire " ++ indexToLabel idx

showEquipUse :: PublicEquipmentUse -> String
showEquipUse PubUseStabilizer =
  "Stabilizer"
showEquipUse (PubUseSuperDetector pid w) =
  "SuperDetector called " ++ showCallOut w ++ " → Player " ++ show pid
showEquipUse (PubUseTripleDetector pid (a, b, c) w) =
  "TripleDetector called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ " [" ++ intercalate ", " (map indexToLabel [a, b, c]) ++ "]"
showEquipUse (PubUseDoubleDetector pid (a, b) w) =
  "DoubleDetector called " ++ showCallOut w
    ++ " → Player " ++ show pid ++ " [" ++ indexToLabel a ++ ", " ++ indexToLabel b ++ "]"
showEquipUse (PubUseDoubleRadiator pid tgt (w1, w2)) =
  "DoubleRadiator called " ++ showCallOut w1 ++ ", " ++ showCallOut w2
    ++ " → Player " ++ show pid ++ " [" ++ indexToLabel tgt ++ "]"

showCallOut :: Wire -> String
showCallOut w = case color w of
  Yellow -> colorEmoji Yellow
  _      -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")"
