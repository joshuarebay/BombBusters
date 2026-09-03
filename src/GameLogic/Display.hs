module GameLogic.Display (displayPlayerView, displayWirePickView, displayInfoMarkerView, displayTurnResult, displayBotDecision, indexToLabel) where

import Control.Monad (unless, when)
import Data.List (intercalate)
import Types
import Config (bbDelay, bbClearScreen)

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

  bbClearScreen

  putStrLn "\n============================================="
  putStrLn $ "PLAYER " ++ show pid ++ " isBot: " ++ show botStatus ++ " — Detonator: " ++ show det
  putStrLn "============================================="
  putStrLn $
    "Color Markers: "
      ++ (if null colorMarks then "(none)" else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks))
  putStrLn $ "Unlocking Values: " ++ unlockingSummary pv
  putStrLn "---------------------------------------------"
  putStrLn "YOUR RACK"
  putStrLn "---------------------------------------------"
  printRack self allInfos
  putStrLn "   ⬜ = Hidden   💡 = Info   ⚫ = Cut"

  unless (null others) $ do
    putStrLn "---------------------------------------------"
    putStrLn "OTHER PLAYERS"
    putStrLn "---------------------------------------------"
    mapM_ (\p -> printRack p allInfos) others

  putStrLn "---------------------------------------------"
  putStrLn "CUT WIRES (All Players)"
  putStrLn "---------------------------------------------"
  if null cutWs
    then putStrLn "   (No cuts yet.)"
    else mapM_ printCut cutWs >> putStrLn ""

  when botStatus $ do
    putStrLn "---------------------------------------------"
    putStrLn "EQUIPMENT AVAILABLE"
    putStrLn "---------------------------------------------"
    putStrLn $ "   " ++ equipmentSummary pv

  putStrLn "\n=============================================\n"

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
      allInfos = pvInfoMarkers pv
      cutWs = pvCutWires pv
      colorMarks = pvColorMarkers pv
      det = pvDetonator pv

  bbClearScreen

  putStrLn "\n---------------------------------------------"
  putStrLn $ "Detonator: " ++ show det
  putStrLn $
    "Color Markers: "
      ++ (if null colorMarks then "(none)" else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks))
  putStrLn $ "Unlocking Values: " ++ unlockingSummary pv
  putStrLn "---------------------------------------------"
  putStrLn "YOUR RACK"
  printRackLabeled self allInfos
  case mTarget of
    Nothing -> pure ()
    Just target -> do
      putStrLn "---------------------------------------------"
      putStrLn "TARGET RACK"
      printRackLabeled target allInfos
  putStrLn "   ⬜ = Hidden   💡 = Info   ⚫ = Cut"
  putStrLn "---------------------------------------------"
  putStrLn "CUT WIRES (All Players)"
  if null cutWs
    then putStrLn "   (No cuts yet.)"
    else mapM_ printCut cutWs >> putStrLn ""
  putStrLn "---------------------------------------------\n"

displayInfoMarkerView :: PlayerView -> IO ()
displayInfoMarkerView pv = do
  let self = pvSelf pv
      others = pvOthers pv
      allInfos = pvInfoMarkers pv
      colorMarks = pvColorMarkers pv
      det = pvDetonator pv

  bbClearScreen

  putStrLn "\n---------------------------------------------"
  putStrLn $ "Detonator: " ++ show det
  putStrLn $
    "Color Markers: "
      ++ (if null colorMarks then "(none)" else unwords (map (\w -> colorEmoji (color w) ++ "(" ++ show (value w) ++ ")") colorMarks))
  putStrLn $ "Unlocking Values: " ++ unlockingSummary pv
  putStrLn "---------------------------------------------"
  putStrLn "YOUR RACK"
  printRackLabeled self allInfos
  unless (null others) $ do
    putStrLn "---------------------------------------------"
    putStrLn "OTHER PLAYERS"
    putStrLn "---------------------------------------------"
    mapM_ (`printRackLabeled` allInfos) others
  putStrLn "   ⬜ = Hidden   💡 = Info   ⚫ = Cut"
  putStrLn "---------------------------------------------"

printRack :: Player -> [InfoMarker] -> IO ()
printRack pl infos = do
  let pid = playerId pl
      pName = playerName pl
      pwires = playerWires pl
      infosForPlayer =
        [ (idx, (val, col))
        | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos,
          owner == pid
        ]
  putStrLn $ "Player " ++ show pid ++ " (" ++ pName ++ "):"
  putStrLn $ "   " ++ concatMap (formatWireOverview infosForPlayer) pwires ++ "\n"

printRackLabeled :: Player -> [InfoMarker] -> IO ()
printRackLabeled pl infos = do
  let pid = playerId pl
      pName = playerName pl
      pwires = playerWires pl
      infosForPlayer =
        [ (idx, (val, col))
        | InfoMarker {markerOwner = owner, markerIndex = idx, markerValue = val, markerColor = col} <- infos,
          owner == pid
        ]
  putStrLn $ "Player " ++ show pid ++ " (" ++ pName ++ "):"
  putStrLn $ "   " ++ concatMap (formatWireLabeled infosForPlayer) pwires ++ "\n"

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

printCut :: MarkedWire -> IO ()
printCut MarkedWire {player = _, originalIndex = _, wire = w} =
  putStr $ colorEmoji (color w) ++ " (" ++ show (value w) ++ ") "

colorEmoji :: WireColor -> String
colorEmoji Blue = "🔵"
colorEmoji Yellow = "🟡"
colorEmoji Red = "🔴"
colorEmoji Grey = "⚫"
colorEmoji Hidden = "⬜"

displayTurnResult :: Outcome -> IO ()
displayTurnResult ActionDone = pure ()
displayTurnResult outcome = do
  putStrLn $ case outcome of
    CutSuccess -> "Successful cut!"
    _ -> "Oops! Wrong wire..."
  bbDelay 3000000

displayBotDecision :: Maybe PublicAction -> [PublicEquipmentUse] -> IO ()
displayBotDecision act equip = do
  case equip of
    [] -> pure ()
    _  -> putStrLn $ "Equipment : " ++ intercalate ", " (map showEquipUse equip)
  putStrLn $ "Action    : " ++ maybe "(equipment only)" showBotAction act
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
