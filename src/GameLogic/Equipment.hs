module GameLogic.Equipment where

import Control.Monad.State
import Data.List
import GameLogic.Actions
import GameLogic.Util
import Types

handleEquipment :: Modifier -> PlayerView -> [EquipmentUse] -> GameM Outcome
handleEquipment _ _ [] = pure ActionDone
handleEquipment f pv (x : xs) = do
  o <- handleOneEquipment f pv x
  rest <- handleEquipment f pv xs
  pure (mergeOutcome o rest)

handleOneEquipment :: Modifier -> PlayerView -> EquipmentUse -> GameM Outcome
handleOneEquipment _ _ UseStabilizer = pure ActionDone
handleOneEquipment f pv (UseSuperDetector pid selfIdx _) = do
  gs <- get
  let outcome = if null (superMatch pv pid selfIdx gs) then CutMismatch else CutSuccess
  modify (f (superDetectorUpdate pv pid selfIdx))
  pure outcome
handleOneEquipment f pv (UseTripleDetector pid selfIdx (pos1, pos2, pos3) _) = do
  gs <- get
  let outcome = if null (multiMatch [pos1, pos2, pos3] pv pid selfIdx gs) then CutMismatch else CutSuccess
  modify (f (multiDetectorUpdate pv pid selfIdx [pos1, pos2, pos3]))
  pure outcome
handleOneEquipment f pv (UseDoubleDetector pid selfIdx (pos1, pos2) _) = do
  gs <- get
  let outcome = if null (multiMatch [pos1, pos2] pv pid selfIdx gs) then CutMismatch else CutSuccess
  modify (f (multiDetectorUpdate pv pid selfIdx [pos1, pos2]))
  pure outcome
handleOneEquipment f pv (UseDoubleRadiator pid (i1, i2) pos _) = do
  gs <- get
  let outcome = doubleRadiatorOutcome pv pid (i1, i2) pos gs
  modify (f (doubleRadiatorUpdate pv pid (i1, i2) pos))
  pure outcome

doubleRadiatorOutcome :: PlayerView -> PlayerId -> (Int, Int) -> Int -> GameState -> Outcome
doubleRadiatorOutcome pv pid2 (i1, i2) pos2 gs =
  case maybeFindPlayer pid2 (players gs) of
    Nothing -> CutMismatch
    Just p2 ->
      let wiresP1 = playerWires (pvSelf pv)
          w1 = wiresP1 !! i1
          w2 = wiresP1 !! i2
          wQ = playerWires p2 !! pos2
          matchesWire wSelf wOther = case color wSelf of
            Yellow -> color wOther == Yellow
            Blue -> color wOther == Blue && value wSelf == value wOther
            _ -> False
       in if matchesWire w1 wQ || matchesWire w2 wQ then CutSuccess else CutMismatch

-- stabilizer is special, as it is used in combination with another action,
-- solved by making it a wrapping function that prevents game ending functionality like turning the dial or 
-- exploding on red 
handleStabilizer :: [EquipmentUse] -> Modifier
handleStabilizer eqs
  | UseStabilizer `elem` eqs = stabilizerModifier
  | otherwise = id

stabilizerModifier :: Modifier
stabilizerModifier update gs =
  let detBefore = detonator gs
      gs' = update gs
   in gs' {detonator = detBefore}

superDetectorUpdate :: PlayerView -> PlayerId -> Int -> GameState -> GameState
superDetectorUpdate = detectorUpdate superMatch allWireIndices
  where
    allWireIndices p = [i | (i, w) <- zip [0 ..] (playerWires p), color w /= Red, color w /= Grey]

superMatch :: MatchStrategy
superMatch pv pid selfIdx gs =
  let p1 = pvSelf pv
      p2m = maybeFindPlayer pid (players gs)
      wP1 = playerWires p1 !! selfIdx
   in case (color wP1, p2m) of
        (Blue, Just p2) ->
          findIndices
            (\w -> color w == Blue && value w == value wP1)
            (playerWires p2)
        _ -> []

multiDetectorUpdate :: PlayerView -> PlayerId -> Int -> [Int] -> GameState -> GameState
multiDetectorUpdate pv pid selfIdx positions = detectorUpdate (multiMatch positions) (const positions) pv pid selfIdx

multiMatch :: [Int] -> MatchStrategy
multiMatch positions pv pid selfIdx gs =
  let p1 = pvSelf pv
      p2m = maybeFindPlayer pid (players gs)
      wP1 = playerWires p1 !! selfIdx
   in case (color wP1, p2m) of
        (Blue, Just p2) ->
          let ws = playerWires p2
           in [i | i <- positions, Just w <- [safeIndex ws i], color w == Blue, value w == value wP1]
        _ -> []

doubleRadiatorUpdate :: PlayerView -> PlayerId -> (Int, Int) -> Int -> GameState -> GameState
doubleRadiatorUpdate pv1 pid2 (i1, i2) pos2 gs
  | color wQ == Red =
      gs {detonator = 0}
  | matches = updateGameStateFromView pv1Match $ updateGameStateFromView pv2Match gs
  | otherwise =
      updateGameStateFromView pv2NoMatch gs
  where
    p1 = pvSelf pv1
    pid1 = playerId p1
    pv2 = getPlayerView pid2 gs
    p2 = pvSelf pv2
    wiresP1 = playerWires p1
    wiresP2 = playerWires p2
    w1 = wiresP1 !! i1
    w2 = wiresP1 !! i2
    wQ = wiresP2 !! pos2

    matchingWirePos
      | matchesWire w1 wQ = i1  
      | otherwise = i2 -- safe, because one wire must match when calling this function 

    matches = matchesWire w1 wQ || matchesWire w2 wQ
    matchesWire wSelf wOther =
      case color wSelf of
        Yellow -> color wOther == Yellow
        Blue -> color wOther == Blue && value wSelf == value wOther
        _ -> False
    wiresP1Match = cutAtPosition matchingWirePos wiresP1
    wiresP2Match = cutAtPosition pos2 wiresP2

    cutInfoP1 = extractCutWires pid1 matchingWirePos wiresP1
    cutInfoP2 = extractCutWires pid2 pos2 wiresP2

    infoMarkersP2 = 
      [InfoMarker 
        { markerOwner = pid2, 
          markerIndex = pos2, 
          markerValue = value w, 
          markerColor = color w
        } | w <- map wire cutInfoP2]
    p1Match =
      p1 {playerWires = wiresP1Match}
    p2Match =
      p2 {playerWires = wiresP2Match}
    pv1Match =
      pv1
        { pvSelf = p1Match,
          pvCutWires = pvCutWires pv1 ++ cutInfoP1
        }
    pv2Match =
      pv2
        { pvSelf = p2Match,
          pvCutWires = pvCutWires pv2 ++ cutInfoP2
        }
    pv2NoMatch =
      pv2
        { pvDetonator = pvDetonator pv2 - 1,
          pvInfoMarkers = pvInfoMarkers pv2 ++ infoMarkersP2
        }

detectorUpdate :: MatchStrategy -> (Player -> [Int]) -> PlayerView -> PlayerId -> Int -> GameState -> GameState
detectorUpdate matchFn noMatchOptions pv pid selfIdx gs =
  case matches of
    [] ->
      let gs' = updateGameStateFromView pv2Penalty gs
          markedIdxs = [markerIndex m | m <- infoMarkers gs, markerOwner m == pid]
          validOpts = filter (\i -> i `notElem` markedIdxs && maybe True (\w -> color w `notElem` [Red, Grey]) (safeIndex (playerWires p2) i)) (noMatchOptions p2)
        in if null validOpts
            then gs' { status = InProgress }
            else gs' {
                status = WaitingForChoice $
                    ChooseWire {
                        chooser = pid,
                        options = validOpts,
                        resolver = \pos2 s -> placeInfoMarkerUpdate (getPlayerView pid s) pos2 (s { status = InProgress }),
                        choiceKind = PickInfoMarkerSlot
                    }
            }
    [pos2] ->
      resolveEquipment pv pid selfIdx pos2 gs
    opts ->
      gs
        { status =
            WaitingForChoice $
              ChooseWire
                { chooser = pid,
                  options = opts,
                  resolver =
                    \pos2 ->
                      resolveEquipment pv pid selfIdx pos2
                        . (\s -> s {status = InProgress}),
                  choiceKind = PickWireToCut
                }
        }
  where
    p2 = findPlayer pid (players gs)
    matches = matchFn pv pid selfIdx gs
    pv2Penalty = pv {pvDetonator = pvDetonator pv - 1}

resolveEquipment :: PlayerView -> PlayerId -> Int -> Int -> GameState -> GameState
resolveEquipment pv pid2 selfIdx pos2 gs =
  case maybeFindPlayer pid2 (players gs) of
    Nothing -> gs
    Just p2 ->
      updateGameStateFromView pv1Match . updateGameStateFromView pv2Match $ gs
      where
        p1 = pvSelf pv
        pid1 = playerId p1

        wiresP1 = playerWires p1
        wiresP2 = playerWires p2

        wiresP1Match = cutAtPosition selfIdx wiresP1
        wiresP2Match = cutAtPosition pos2 wiresP2

        cutInfoP1 = extractCutWires pid1 selfIdx wiresP1
        cutInfoP2 = extractCutWires pid2 pos2 wiresP2

        p1Match = p1 {playerWires = wiresP1Match}
        p2Match = p2 {playerWires = wiresP2Match}

        pv1Match =
          pv
            { pvSelf = p1Match,
              pvCutWires = pvCutWires pv ++ cutInfoP1
            }

        pv2 = getPlayerView pid2 gs
        pv2Match =
          pv2
            { pvSelf = p2Match,
              pvCutWires = pvCutWires pv2 ++ cutInfoP2
            }
