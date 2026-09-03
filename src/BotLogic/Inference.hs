module BotLogic.Inference
    ( SoftFact(..)
    , inferSoftFacts
    , worldWeight
    , inferHandConstraints
    ) where

import Types
import BotLogic.BotTypes 
import Config (epistemicConfidence)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)

newtype SoftFact = SoftFact { factHolds :: WorldAssignment -> Bool }

-- Posterior weight of a world under all soft facts. Direct Bayesian form
worldWeight :: [SoftFact] -> WorldAssignment -> Double
worldWeight facts world =
    epistemicConfidence ^ nSat * (1 - epistemicConfidence) ^ nVio
  where
    nVio = length [() | f <- facts, not (factHolds f world)]
    nSat = length facts - nVio

-- Walk the turn history and emit a soft fact for every publicly-detectable
-- "obvious move" the player failed to take, using per-turn replayed state.
-- A second family of facts comes from the first-round info-marker placements
-- (see bulkFacts): when a player voluntarily marks a Blue 1 or Blue 12, we
-- assume the wires on the bulk side of the marker are NOT Yellow or Red --
-- the player wouldn't have bothered placing a "bulk reveal" marker if one of
-- the enclosed wires broke the run. Same epistemicConfidence applies via
-- worldWeight, it never enters the SMT as a hard constraint.
inferSoftFacts
    :: [TurnLog] -- chronological history
    -> [InfoMarker] -- current public info markers
    -> [MarkedWire] -- current public cut wires
    -> Int -- number of players (forced first-round placements skipped)
    -> [SoftFact]
inferSoftFacts history allMarkers allCuts numPlayers =
    concatMap turnPairFacts eligible ++ bulkFacts
  where
    eligible =
        [ (turn, st)
        | (turn, st) <- preStatesByTurn allMarkers allCuts history
        , logRound turn >= numPlayers
        ]
    turnPairFacts (turn, st) =
        turnFacts (rsMarkers st) (rsCuts st) turn

    firstRoundMarkers =
        [ m
        | turn <- history
        , logRound turn < numPlayers
        , Just (PubPlaceInfoMarker i) <- [logAction turn]
        , m <- lookupMarker allMarkers (logPlayer turn) i
        ]

    bulkFacts = concatMap markerToBulkFact firstRoundMarkers

    markerToBulkFact m
        | markerColor m == Blue && markerValue m == 1 =
            [bulkAllBlueFact (markerOwner m) (< markerIndex m)]
        | markerColor m == Blue && markerValue m == 12 =
            [bulkAllBlueFact (markerOwner m) (> markerIndex m)]
        | otherwise = []

    bulkAllBlueFact pid inBulkRange =
        SoftFact $ \w -> case Map.lookup pid w of
            Nothing    -> True
            Just slots ->
                all (\(idx, (_, c)) -> not (inBulkRange idx) || c == Blue)
                    (Map.toList slots)


-- Pair each turn with the public state that existed at its start
preStatesByTurn :: [InfoMarker] -> [MarkedWire] -> [TurnLog] -> [(TurnLog, ReplayState)]
preStatesByTurn allMarkers allCuts history =
    zip history (init priorPlusFinal)
  where
    initial = ReplayState [] []
    advance st turn = ReplayState
        { rsMarkers = rsMarkers st ++ markersAddedBy allMarkers turn
        , rsCuts = rsCuts st ++ cutsAddedBy allCuts turn
        }
    -- the i-th element is the state right before the i-th turn.
    priorPlusFinal = scanl advance initial history

markersAddedBy :: [InfoMarker] -> TurnLog -> [InfoMarker]
markersAddedBy allMarkers turn =
    case logAction turn of
        Just (PubPlaceInfoMarker i) ->
            lookupMarker allMarkers actor i
        Just (PubDualCut tpid tIdx _) | logOutcome turn == CutMismatch ->
            lookupMarker allMarkers tpid tIdx
        _ -> []
  where
    actor = logPlayer turn

lookupMarker :: [InfoMarker] -> PlayerId -> Int -> [InfoMarker]
lookupMarker ms pid idx =
    take 1 [m | m <- ms, markerOwner m == pid, markerIndex m == idx]

cutsAddedBy :: [MarkedWire] -> TurnLog -> [MarkedWire]
cutsAddedBy allCuts turn =
    actionCuts ++ equipmentCuts
  where
    actor = logPlayer turn
    outcome = logOutcome turn

    matchCuts pid idxs =
        [c | c <- allCuts, player c == pid, originalIndex c `elem` idxs]

    actionCuts = case logAction turn of
        Just (PubSoloCut2 (a, b)) -> matchCuts actor [a, b]
        Just (PubSoloCut4 (a, b, c, d)) -> matchCuts actor [a, b, c, d]
        Just (PubDualCut tpid tIdx _) | outcome == CutSuccess ->
            matchCuts tpid [tIdx]
        Just (PubRemoveRed i) -> matchCuts actor [i]
        _ -> []

    equipmentCuts = concatMap eqCuts (logEquipment turn)

    -- Same trade-off as for DualCut: caller-side cut indices are private,
    -- so we only attribute the target-side cut 
    eqCuts PubUseStabilizer = []
    eqCuts (PubUseSuperDetector _ _) = []
    eqCuts (PubUseTripleDetector _ _ _) = []
    eqCuts (PubUseDoubleDetector _ _ _) = []
    eqCuts (PubUseDoubleRadiator tpid tIdx _)
        | outcome == CutSuccess = matchCuts tpid [tIdx]
        | otherwise = []

-- we assume that if player didnt choose certain action, its because they couldnt (with epistemicConfidence)
turnFacts :: [InfoMarker] -> [MarkedWire] -> TurnLog -> [SoftFact]
turnFacts markers cuts turn
    | wasCertain markers (logAction turn) = []
    | otherwise = infoMarkerFacts ++ soloCut2Facts ++ soloCut4Facts ++ [removeRedFact]
  where
    actor = logPlayer turn

    infoMarkerFacts =
        [ SoftFact (\w -> countMatching actor (\v' c' -> v' == v && c' == c) w == 0)
        | InfoMarker mo _ v c <- markers
        , mo /= actor
        ]

    soloCut2Facts = yellowFact : blueFacts
      where
        yellowFact = SoftFact
            (\w -> countMatching actor (\_ c -> c == Yellow) w < 2)
        blueFacts =
            [ SoftFact (\w -> countMatching actor (\v' c -> v' == v && c == Blue) w < 2)
            | v <- bluesAlreadyCut
            ]
        bluesAlreadyCut =
            Map.keys . Map.fromListWith (+) $
            [ (value (wire mw), 1 :: Int)
            | mw <- cuts, color (wire mw) == Blue
            ]

    soloCut4Facts =
        [ SoftFact (\w -> countMatching actor (\v' c' -> v' == v && c' == Blue) w < 4)
        | v <- [1 .. 12]
        ]

    removeRedFact = SoftFact
        (\w -> case Map.lookup actor w of
            Nothing -> True
            Just slots -> any (\(_, c) -> c /= Red) (Map.elems slots))

wasCertain :: [InfoMarker] -> Maybe PublicAction -> Bool
wasCertain _ Nothing = False
wasCertain markers (Just act) = case act of
    PubRemoveRed _ -> True
    PubSoloCut4 _ -> True
    PubSoloCut2 _ -> True
    PubDualCut tpid tIdx _ ->
        any (\m -> markerOwner m == tpid && markerIndex m == tIdx) markers
    PubPlaceInfoMarker _ -> True -- never reached 

countMatching :: PlayerId -> (Int -> WireColor -> Bool) -> WorldAssignment -> Int
countMatching pid p world = case Map.lookup pid world of
    Nothing -> 0
    Just slots -> length [() | (v, c) <- Map.elems slots, p v c]

-- Hard constraints from failed cuts

inferHandConstraints :: [TurnLog] -> [MarkedWire] -> [InfoMarker] -> [HandConstraint]
inferHandConstraints history cuts markers =
    Set.toList . Set.fromList . filter stillHeld $ concatMap forTurn history
  where
    stillHeld (HasBlue pid v) = not $ any
        (\mw -> player mw == pid
             && color (wire mw) == Blue
             && value (wire mw) == v) cuts
    stillHeld (HasYellow pid) = not $ any
        (\mw -> player mw == pid && color (wire mw) == Yellow) cuts
    -- Exclusion + colour-fixed constraints never retire: slot identity is
    -- a permanent fact regardless of later cuts.
    stillHeld (SlotsNotBlue {}) = True
    stillHeld (HandNotBlue {}) = True
    stillHeld (SlotIsRed {}) = True

    forTurn turn =
        let actor = logPlayer turn
            outcome = logOutcome turn
            equip = logEquipment turn
            stab = PubUseStabilizer `elem` equip
            fromAction = case logAction turn of
                Just (PubDualCut tpid tIdx w) | outcome == CutMismatch ->
                    mkHC actor w ++ mkRedFromStab stab tpid tIdx
                _ -> []
            fromEquip = concatMap (forEq actor outcome stab) equip
        in fromAction ++ fromEquip

    forEq _ _ _ PubUseStabilizer = []
    forEq actor outcome _ (PubUseSuperDetector tpid w)
        | outcome == CutMismatch = mkHC actor w ++ mkHandNot tpid w
        | otherwise = []
    forEq actor outcome _ (PubUseTripleDetector tpid (p1, p2, p3) w)
        | outcome == CutMismatch = mkHC actor w ++ mkSlotsNot tpid [p1, p2, p3] w
        | otherwise = []
    forEq actor outcome _ (PubUseDoubleDetector tpid (p1, p2) w)
        | outcome == CutMismatch = mkHC actor w ++ mkSlotsNot tpid [p1, p2] w
        | otherwise = []
    forEq actor outcome stab (PubUseDoubleRadiator tpid tIdx (w1, w2))
        | outcome == CutMismatch =
            mkHC actor w1 ++ mkHC actor w2 ++ mkRedFromStab stab tpid tIdx
        | otherwise = case remainingDoubleRadiatorWire cuts tpid tIdx (w1, w2) of
            Just w -> mkHC actor w
            Nothing -> []

    mkHC pid w = case color w of
        Blue -> [HasBlue pid (value w)]
        Yellow -> [HasYellow pid]
        _ -> []

    -- Detectors can only legally call Blue, so non-Blue calls produce no
    -- exclusion (mirrors how RemoveRed-style non-Blue calls collapse in mkHC).
    mkSlotsNot tpid slots w = case color w of
        Blue -> [SlotsNotBlue tpid slots (value w)]
        _ -> []
    mkHandNot tpid w = case color w of
        Blue -> [HandNotBlue tpid (value w)]
        _ -> []

    -- The SlotIsRed inference only kicks in for failed call-outs that were
    -- Stabilizer-protected. No-marker-on-queried-slot is the Red signature,
    -- if a marker IS on (tpid, tIdx) the failure was a non-Red wrong call.
    mkRedFromStab False _ _ = []
    mkRedFromStab True tpid tIdx
        | hasMarkerAt tpid tIdx = []
        | otherwise = [SlotIsRed tpid tIdx]

    hasMarkerAt pid idx =
        any (\m -> markerOwner m == pid && markerIndex m == idx) markers

-- For a successful DoubleRadiator, identify the called-out wire that did not
-- match (the still-on-hand one). We do this by inspecting the cut record at
-- the target slot: its value/color tells us which of (w1, w2) matched.
remainingDoubleRadiatorWire :: [MarkedWire] -> PlayerId -> Int -> (Wire, Wire) -> Maybe Wire
remainingDoubleRadiatorWire cuts tpid tIdx (w1, w2) =
    case find (\m -> player m == tpid && originalIndex m == tIdx) cuts of
        Nothing -> Nothing
        Just t ->
            let tw = wire t
            in if calledMatches w1 tw then Just w2 else Just w1
  where
    calledMatches called tw = case color called of
        Yellow -> color tw == Yellow
        Blue -> color tw == Blue && value called == value tw
        _ -> False
