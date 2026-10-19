module GameLogic.Display (displayPlayerView, displayWirePickView, displayInfoMarkerView, displayTurnResult, displayBotDecision, announceHumanTurn, indexToLabel) where

import Control.Monad (unless, when)
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (insert)
import Types
import Config (bbDelay, bbClearScreen)
import Output
import OutBox

indexToLabel :: Int -> String
indexToLabel i = [toEnum (fromEnum 'a' + i)]

displayPlayerView :: PlayerView -> GameM ()
displayPlayerView pv = do
  let self = pvSelf pv
      pid = playerId self
      cutWs = pvCutWires pv
      allInfos = pvInfoMarkers pv
      others = pvOthers pv
      botStatus = isBot self
      det = pvDetonator pv

      header = "\n============================================="
            ++ "\nPLAYER " ++ show pid ++ " isBot: " ++ show botStatus ++ " - Detonator: " ++ show det
            ++ "\n============================================="

      cutText = "\nCUT WIRES (All Players)\n"
            ++ (if null cutWs then "   (No cuts yet.)" else concatMap printCut cutWs)

      equipText = if botStatus 
                    then "\n---------------------------------------------"
                      ++ "\nEQUIPMENT AVAILABLE" ++ "\n   " ++ equipmentSummary pv
                    else ""

      hasHumanOnlooker = any (not . isBot) others
      displaySelf
        | botStatus && hasHumanOnlooker = censorPlayerWires self
        | otherwise = self

  liftIO bbClearScreen
  printRack self allInfos

  unless (null others) $ do
    mapM_ (\oth -> printRack oth allInfos) others
    
  modify (\gs -> gs { actionMsg = header ++ cutText ++ equipText })
  syncState


equipmentSummary :: PlayerView -> String
equipmentSummary pv =
  let availEq = [ show (name e) | e <- pvEquipment pv, unlocked e, not (used e) ]
      dd = [ "DoubleDetector" | doubleDetector (pvSelf pv) ]
      items = availEq ++ dd
  in if null items then "(none)" else intercalate ", " items

-- unlockingSummary :: PlayerView -> String
-- unlockingSummary pv =
--   let toUnlock = [ (unlockingVal e, name e) | e <- pvEquipment pv, not (unlocked e) ]
--   in case toUnlock of
--         [] -> "(none)"
--         xs -> intercalate ", " [show v ++ " → " ++ show eq | (v, eq) <- xs]

displayWirePickView :: PlayerView -> Maybe Player -> GameM ()
displayWirePickView pv mTarget = do
  let allInfos = pvInfoMarkers pv
      self = pvSelf pv
  liftIO bbClearScreen

--  instructionMessage (pvOutBox pv) (ToPlayer pid) $ "Detonator: " ++ show det
--  printYRWires gs
--  instructionMessage (pvOutBox pv) (ToPlayer pid) $ "Unlocking Values: " ++ unlockingSummary pv

--  instructionMessage (pvOutBox pv) (ToPlayer pid) "YOUR RACK"
  printRackLabeled self allInfos
  case mTarget of
    Nothing -> pure ()
    Just target -> do
--      instructionMessage ob (ToPlayer pid) "TARGET RACK"
      printRackLabeled target allInfos

--  instructionMessage (pvOutBox pv) (ToPlayer pid) "CUT WIRES (All Players)"
--  if null cutWs
--    then instructionMessage (pvOutBox pv) (ToPlayer pid) "   (No cuts yet.)"
--    else let cutText = concatMap printCut cutWs >> instructionMessage (pvOutBox pv) (ToPlayer pid) ""
--  instructionMessage (pvOutBox pv) (ToPlayer pid) "---------------------------------------------\n"

displayInfoMarkerView :: PlayerView -> GameM ()
displayInfoMarkerView pv = do
  let self = pvSelf pv
      others = pvOthers pv
      allInfos = pvInfoMarkers pv
  liftIO bbClearScreen

--  instructionMessage (pvOutBox pv) ToAll $ "Detonator: " ++ show det
--  instructionMessage (pvOutBox pv) ToAll "YOUR RACK"
  printRackLabeled self allInfos
  unless (null others) $ do
--    instructionMessage (pvOutBox pv) ToAll "---------------------------------------------"
--    instructionMessage (pvOutBox pv) ToAll "OTHER PLAYERS"
--    instructionMessage (pvOutBox pv) ToAll "---------------------------------------------"
    mapM_ (\oth -> printRackLabeled oth allInfos) others

announceHumanTurn :: PlayerView -> GameM ()
announceHumanTurn pv = do
  let self = pvSelf pv
      multiHuman = any (not . isBot) (pvOthers pv)
  when (not (isBot self) && multiHuman) $ do
-- neccessary?    
    liftIO bbClearScreen
    let msg = "Player " ++ show (playerId self) ++ " (" ++ playerName self ++ ") - it's your turn!"
    modify (\gs -> gs { announceTurn = (playerId self, msg) })
    syncState
    liftIO (bbDelay 3000000)

censorPlayerWires :: Player -> Player
censorPlayerWires p = p { playerWires = map hide (playerWires p) }
  where
    hide w
      | color w == Grey = w
      | otherwise = w { color = Hidden, value = -1 }

printRack :: Player -> [InfoMarker] -> GameM ()
printRack pl infos = do
  let pid = playerId pl
      pName = playerName pl
      pwires = playerWires pl
      infosForPlayer =
        [ (idx, (val, col))
        | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos,
          owner == pid
        ]
--  instructionMessage ob (ToPlayer pid) $ "Player " ++ show pid ++ " (" ++ pName ++ "):"
      msg = " " ++ concatMap (formatWireOverview infosForPlayer) pwires ++ "\n"
  modify (\gs -> gs { rackOverview = insert pid msg (rackOverview gs) })
  syncState

printRackLabeled :: Player -> [InfoMarker] -> GameM ()
printRackLabeled pl infos = do
  let pid = playerId pl
      pName = playerName pl
      pwires = playerWires pl
      infosForPlayer =
        [ (idx, (val, col))
        | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos,
          owner == pid
        ]
      msg = " " ++ concatMap (formatWireLabeled infosForPlayer) pwires ++ "\n"
  modify (\gs -> gs { rackLabled = insert pid msg (rackLabled gs) })
  syncState

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

printCut :: MarkedWire -> String
printCut MarkedWire {player = _, originalIndex = _, wire = w} = colorEmoji (color w) ++ " (" ++ show (value w) ++ ") "


displayTurnResult :: Outcome -> GameM ()
displayTurnResult ActionDone = pure ()
displayTurnResult outcome = do
  let msg = case outcome of
              CutSuccess -> "Successful cut!"
              _ -> "Oops! Wrong wire..."
  modify (\gs -> gs { resultMsg = msg })
  syncState
  liftIO (bbDelay 3000000)

displayBotDecision :: Maybe PublicAction -> [PublicEquipmentUse] -> GameM ()
displayBotDecision act equip = do
  -- update usable equipment
--  case equip of
--    [] -> pure ()
--    _  -> instructionMessage (outBox gs) ToAll $ "Equipment : " ++ intercalate ", " (map showEquipUse equip)
  let msg = "Action    : " ++ maybe "(equipment only)" showBotAction act
  modify (\gs -> gs { actionMsg = msg })
  syncState
  liftIO (bbDelay 1500000)

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
