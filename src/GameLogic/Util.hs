module GameLogic.Util where

import Data.List
import Data.Maybe
import Text.Read
import Types

getPlayerView :: PlayerId -> GameState -> PlayerView
getPlayerView pid gs =
    let self = maybeFindPlayer pid (players gs)
        censoredWires = map (\w -> if (color w == Grey) then w else w {
            color = Hidden, 
            value = -1
            })
        censorPlayer p = p { playerWires = censoredWires (playerWires p) }
   in case self of
        Just s ->
          PlayerView
            { pvSelf = s,
              pvOthers = map censorPlayer $ filter ((/= pid) . playerId) (players gs),
              pvColorMarkers = colorMarkers gs,
              pvDetonator = detonator gs,
              pvEquipment = equipment gs,
              pvCutWires = cutWires gs,
              pvInfoMarkers = infoMarkers gs,
              pvRoundCount = roundCount gs,
              pvTurnOrder = turnOrder gs,
              pvTurnHistory = turnHistory gs,
              pvUnlockingValues = unlockingValues gs
            }
        Nothing -> error $ "player not found pid: " ++ show pid

updateGameStateFromView :: PlayerView -> GameState -> GameState
updateGameStateFromView pv gs =
  let pid = playerId (pvSelf pv)
      mergedSelf =
        case maybeFindPlayer pid (players gs) of
          Just existing -> (pvSelf pv) {doubleDetector = doubleDetector existing}
          Nothing -> pvSelf pv
      updatedPlayers = map (\p -> if playerId p == pid then mergedSelf else p) (players gs)
      updatedWires = concatMap playerWires updatedPlayers
   in gs
        { wires = updatedWires,
          players = updatedPlayers,
          -- equipment/colorMarkers intentionally NOT copied from pv: those live
          -- in gs and are updated via markEquipmentUsed / unlockEquipment. The
          -- pv carries a stale snapshot (captured before the current turn's
          -- marks) and copying it here would silently revert "used" flags when
          -- a deferred resolver fires after a WaitingForChoice transition.
          cutWires = nub (cutWires gs ++ pvCutWires pv),
          infoMarkers = nub (infoMarkers gs ++ pvInfoMarkers pv),
          detonator = pvDetonator pv
        }

maybeFindPlayer :: PlayerId -> [Player] -> Maybe Player
maybeFindPlayer pidTarget = find ((== pidTarget) . playerId)

findPlayer :: PlayerId -> [Player] -> Player
findPlayer pidTarget pList =
  let maybePlayer = find ((== pidTarget) . playerId) pList
   in case maybePlayer of
        Just p -> p
        Nothing -> error $ "Player with id " ++ show pidTarget ++ " not found."

readInt :: String -> IO Int
readInt prompt = do
  putStrLn prompt
  putStr "> "
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just n -> return n
    Nothing -> do
      putStrLn "Invalid number. Please enter a valid integer."
      readInt prompt

readLabel :: String -> IO Int
readLabel prompt = do
  putStrLn prompt
  putStr "> "
  line <- getLine
  case line of
    [c] | c >= 'a' && c <= 'z' -> return (fromEnum c - fromEnum 'a')
    _ -> do
      putStrLn "Invalid input. Please enter a single lowercase letter (e.g. a, b, c)."
      readLabel prompt

cutAtPosition :: Int -> [Wire] -> [Wire]
cutAtPosition pos = map (\w -> if wireIndex w == pos then w {color = Grey} else w)

extractCutWires :: PlayerId -> Int -> [Wire] -> [MarkedWire]
extractCutWires pid pos ws = [MarkedWire pid (wireIndex w) w | w <- ws, wireIndex w == pos]

isEquipmentUnlocked :: [MarkedWire] -> Equipment -> Bool
isEquipmentUnlocked cuts e = countBlueCutsOfValue (unlockingVal e) cuts >= 2

countBlueCutsOfValue :: Int -> [MarkedWire] -> Int
countBlueCutsOfValue v cuts =
  length [ () | mw <- cuts
              , let w = wire mw
              , color w == Blue
              , value w == v ]

unlockedEquipment :: PlayerView -> [Equipment]
unlockedEquipment pv =
  filter (isEquipmentUnlocked (pvCutWires pv)) (pvEquipment pv)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      (y : _) -> Just y
      [] -> Nothing

equipmentUseToGlobal :: EquipmentUse -> Maybe EquipmentName
equipmentUseToGlobal UseStabilizer = Just Stabilizer
equipmentUseToGlobal UseSuperDetector {} = Just SuperDetector
equipmentUseToGlobal UseTripleDetector {} = Just TripleDetector
equipmentUseToGlobal UseDoubleRadiator {} = Just DoubleRadiator
equipmentUseToGlobal UseDoubleDetector {} = Nothing

markEquipmentUsed :: [EquipmentUse] -> GameState -> GameState
markEquipmentUsed usedEq gs =
  gs {equipment = map mark (equipment gs)}
  where
    usedNames = mapMaybe equipmentUseToGlobal usedEq

    mark e
      | name e `elem` usedNames = e {used = True}
      | otherwise = e

markDoubleDetectorUsed :: PlayerId -> [EquipmentUse] -> GameState -> GameState
markDoubleDetectorUsed userId uses gs =
  if any isDD uses
    then gs {players = map disable (players gs)}
    else gs
  where
    isDD UseDoubleDetector {} = True
    isDD _ = False

    disable p
      | playerId p == userId = p {doubleDetector = False}
      | otherwise = p

unlockEquipment :: GameState -> GameState
unlockEquipment gs =
  let cuts = cutWires gs
      unlockIfMatches e
        | isEquipmentUnlocked cuts e = e {unlocked = True}
        | otherwise = e
   in gs {equipment = map unlockIfMatches (equipment gs)}

advanceTurn :: GameState -> GameState
advanceTurn gs =
  case turnOrder gs of
    [] -> gs
    (x : xs) ->
      let newTurnOrder = xs ++ [x]
          newCurrent = case xs of
            (y : _) -> y
            []      -> x
       in gs
            { turnOrder = newTurnOrder,
              currentPlayer = newCurrent,
              roundCount = roundCount gs + 1
            }

evaluateGameEnd :: GameState -> GameState
evaluateGameEnd gs
  | detonated = gs {status = Detonated}
  | all ((== Grey) . color) (wires gs) = gs {status = Defused}
  | otherwise = gs
  where
    detonated = detonator gs <= 0

checkActivePlayers :: GameState -> GameState
checkActivePlayers gs =
  let activePlayers =
        filter (any ((/= Grey) . color) . playerWires) (players gs)
      activeIds = map playerId activePlayers
      curr = currentPlayer gs
      newTurnOrder = filter (`elem` activeIds) (turnOrder gs)
      newCurrent = case activeIds of
        []      -> curr
        (a : _)
          | curr `elem` activeIds -> curr
          | otherwise             -> a
   in gs
        { players = activePlayers,
          turnOrder = newTurnOrder,
          currentPlayer = newCurrent
        }

selectBots :: [PlayerId] -> GameState -> GameState 
selectBots botIds gs = gs { players = map (\p -> if (playerId p) `elem` botIds then p {isBot = True} else p) $ players gs }

shortenList :: Int -> List a -> List a
shortenList 0 _ = []
shortenList _ [] = []
shortenList n (x:xs) = x : shortenList (n-1) xs

mergeOutcome :: Outcome -> Outcome -> Outcome
mergeOutcome ActionDone o = o
mergeOutcome o _ = o

appendTurnLog :: TurnLog -> GameState -> GameState
appendTurnLog tlog gs = gs {turnHistory = turnHistory gs ++ [tlog]}

-- Strip board-game-private information when emitting an action to the log /
-- display. Caller's own slot index is removed from DualCut. Called-out
-- wires have their `wireIndex` field zeroed because that field is the
-- caller's private slot position; only value/color is publicly spoken.
toPublicAction :: Action -> PublicAction
toPublicAction (DualCut pid (_, tIdx) w) = PubDualCut pid tIdx (anonymise w)
toPublicAction (SoloCut2 ab)             = PubSoloCut2 ab
toPublicAction (SoloCut4 abcd)           = PubSoloCut4 abcd
toPublicAction (RemoveRed i)             = PubRemoveRed i
toPublicAction (PlaceInfoMarker i)       = PubPlaceInfoMarker i

toPublicEquipmentUse :: EquipmentUse -> PublicEquipmentUse
toPublicEquipmentUse UseStabilizer = PubUseStabilizer
toPublicEquipmentUse (UseSuperDetector pid _ w) =
    PubUseSuperDetector pid (anonymise w)
toPublicEquipmentUse (UseTripleDetector pid _ tps w) =
    PubUseTripleDetector pid tps (anonymise w)
toPublicEquipmentUse (UseDoubleDetector pid _ tps w) =
    PubUseDoubleDetector pid tps (anonymise w)
toPublicEquipmentUse (UseDoubleRadiator pid _ tIdx (w1, w2)) =
    PubUseDoubleRadiator pid tIdx (anonymise w1, anonymise w2)

-- Strip everything that isn't board-game-public from a called-out wire.
-- For yellow the value isn't spoken (the caller just says "yellow") so
-- we scrub it too; for blue the caller speaks the value, so it stays.
anonymise :: Wire -> Wire
anonymise w = case color w of
    Yellow -> w { wireIndex = -1, value = -1 }
    _ -> w { wireIndex = -1 }
