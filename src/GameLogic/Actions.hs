module GameLogic.Actions (handleAction, placeInfoMarkerUpdate) where

import Control.Monad.State
import GameLogic.Util
import Types

-- pure action handlers, rely on valid input 

handleAction :: PlayerView -> Action -> Modifier -> GameM Outcome
handleAction pv action f = case action of
  SoloCut2 pos -> modify (f (soloCutUpdate pv pos)) >> pure CutSuccess
  SoloCut4 (a, b, c, d) -> modify (f (soloCutUpdate4 pv (a, b, c, d))) >> pure CutSuccess
  DualCut pid2 pos _ -> do
    gs <- get
    let outcome = dualCutOutcome pv pid2 pos gs
    modify (f (dualCutUpdate pv pid2 pos))
    pure outcome
  RemoveRed pos -> modify (f (removeRedUpdate pv pos)) >> pure ActionDone
  PlaceInfoMarker pos -> modify (placeInfoMarkerUpdate pv pos) >> pure ActionDone

dualCutOutcome :: PlayerView -> PlayerId -> CutPosition -> GameState -> Outcome
dualCutOutcome pv pid2 (pos1, pos2) gs =
  case maybeFindPlayer pid2 (players gs) of
    Nothing -> CutMismatch
    Just p2 ->
      let w1 = playerWires (pvSelf pv) !! pos1
          w2 = playerWires p2 !! pos2
          matches = case color w1 of
            Yellow -> color w2 == Yellow
            Blue -> color w2 == Blue && value w1 == value w2
            _ -> False
       in if matches then CutSuccess else CutMismatch

soloCutUpdate4 :: PlayerView -> (Int, Int, Int, Int) -> GameState -> GameState
soloCutUpdate4 pv (a, b, c, d) gs =
  let gs' = soloCutUpdate pv (a, b) gs
      pv' = getPlayerView (playerId (pvSelf pv)) gs'
   in soloCutUpdate pv' (c, d) gs'

soloCutUpdate :: PlayerView -> CutPosition -> GameState -> GameState
soloCutUpdate pv (a, b) gs =
  let p = pvSelf pv
      pid = playerId p
      ws = playerWires p
      newWires = cutAtPosition a (cutAtPosition b ws)
      cutWs =
        extractCutWires pid a ws ++ extractCutWires pid b ws
      p' = p {playerWires = newWires}
      pv' =
        pv
          { pvSelf = p',
            pvCutWires = pvCutWires pv ++ cutWs
          }
   in updateGameStateFromView pv' gs

dualCutUpdate :: PlayerView -> PlayerId -> CutPosition -> GameState -> GameState
dualCutUpdate pv1 pid2 (pos1, pos2) gs
  | color w2 == Red =
      gs {detonator = 0}
  | matches =
      updateGameStateFromView pv1Match $ updateGameStateFromView pv2Match gs
  | otherwise =
      updateGameStateFromView pv2NoMatch gs
  where
    p1 = pvSelf pv1
    pid1 = playerId p1
    pv2 = getPlayerView pid2 gs
    p2 = pvSelf pv2
    wiresP1 = playerWires p1
    wiresP2 = playerWires p2
    w1 = wiresP1 !! pos1
    w2 = wiresP2 !! pos2

    matches = case color w1 of
      Yellow -> color w2 == Yellow
      Blue -> color w2 == Blue && value w1 == value w2
      _ -> False

    wiresP1Match = cutAtPosition pos1 wiresP1
    wiresP2Match = cutAtPosition pos2 wiresP2

    cutInfoP1 = extractCutWires pid1 pos1 wiresP1
    cutInfoP2 = extractCutWires pid2 pos2 wiresP2

    infoMarkersP2 = 
      [InfoMarker 
        { markerOwner = pid2, 
          markerIndex = pos2, 
          markerValue = value w, 
          markerColor = color w
        } | w <- map wire cutInfoP2]

    p1Match =
      p1
        { playerWires = wiresP1Match
        }
    p2Match =
      p2
        { playerWires = wiresP2Match
        }
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

removeRedUpdate :: PlayerView -> Int -> GameState -> GameState
removeRedUpdate pv pos gs =
  let p = pvSelf pv
      pid = playerId p
      ws = playerWires p
      newWires = cutAtPosition pos ws
      cutInfo = extractCutWires pid pos ws
      p' = p {playerWires = newWires}
      pv' =
        pv
          { pvSelf = p',
            pvCutWires = pvCutWires pv ++ cutInfo
          }
   in updateGameStateFromView pv' gs

placeInfoMarkerUpdate :: PlayerView -> Int -> GameState -> GameState
placeInfoMarkerUpdate pv pos gs =
  let p = pvSelf pv
      pid = playerId p
      val = value $ playerWires p !! pos
      col = color $ playerWires p !! pos
      newMarker = InfoMarker {markerOwner = pid, markerIndex = pos, markerValue = val, markerColor = col}
      pv' = pv { pvInfoMarkers = pvInfoMarkers pv ++ [newMarker] }
   in updateGameStateFromView pv' gs
