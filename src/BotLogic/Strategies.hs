module BotLogic.Strategies where

import Types
import BotLogic.BotTypes
import Config (maxDetonateProb, stabilizerThreshold)
import qualified Data.Map as Map
import Data.List (sortBy)
import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import Data.Ord (comparing, Down(..))


-- Certain actions, checked before SMT, no world enumeration needed.

certainAction :: BotKnowledge -> PlayerId -> Maybe Action
certainAction bk selfPid = (checkInfoMarkerCut Yellow) <|> checkSoloCut4 <|> checkSoloCut2 <|> checkInfoMarkerCut (Blue) <|> checkRemoveRed 
  where
    selfSlots = Map.findWithDefault Map.empty selfPid (playerSlots bk)
    knownWires = [(i, w) | (i, KnownValue w) <- Map.toAscList selfSlots, color w /= Grey]
    otherPids = [pid | pid <- Map.keys (playerSlots bk), pid /= selfPid]
    cutsOfColor c =
                [ DualCut targetPid (myIdx, targetIdx) myWire
                | (myIdx, KnownValue myWire) <- Map.toAscList selfSlots
                , color myWire == c
                , targetPid <- otherPids
                , (targetIdx, KnownValue targetWire) <-
                      Map.toAscList (Map.findWithDefault Map.empty targetPid (playerSlots bk))
                , wireMatches myWire (value targetWire) (color targetWire)
                ]

    -- group wire indices by (value, color)
    byKey = Map.fromListWith (++) [((value w, color w), [i]) | (i, w) <- knownWires]

    checkSoloCut4 = listToMaybe [ SoloCut4 (a, b, c, d) | (_, (a:b:c:d:_)) <- Map.toList byKey ]

    checkSoloCut2 = checkYellowPair <|> checkBluePair
      where
        yellowIdxs = [i | (i, w) <- knownWires, color w == Yellow]
        checkYellowPair = case yellowIdxs of
            a:b:_ -> Just (SoloCut2 (a, b))
            _ -> Nothing
        checkBluePair = listToMaybe
            [ SoloCut2 (a, b)
            | ((v, Blue), [a, b]) <- Map.toList byKey
            , Map.findWithDefault 0 v (cutBlueCounts bk) == 2
            ]

    -- cut wire marked by infomarker if possible 
    checkInfoMarkerCut c =
        case sortBy (comparing (moveSortKey bk selfPid []))
                    [ (Just a, [], 1.0) | a <- cutsOfColor c ] of
            ((Just a, _, _):_) -> Just a
            _ -> Nothing

    checkRemoveRed =
        let reds = [i | (i, w) <- knownWires, color w == Red]
            nonRed = [i | (i, w) <- knownWires, color w /= Red]
        in case reds of
            (r : _) | null nonRed -> Just (RemoveRed r)
            _ -> Nothing

-- Probability helpers, all take WeightedWorld lists so soft-fact inferences

-- Weighted ratio: numerator-mass / total-mass under a predicate.
ratio :: (WorldAssignment -> Bool) -> [WeightedWorld] -> Double
ratio _ [] = 0
ratio p ws
    | totalW <= 0 = 0
    | otherwise = hitW / totalW
  where
    totalW = sum (map snd ws)
    hitW = sum [weight | (world, weight) <- ws, p world]

successProb :: Wire -> PlayerId -> Int -> [WeightedWorld] -> Double
successProb myWire targetPid targetIdx = ratio matches
  where
    matches w = case Map.lookup targetPid w >>= Map.lookup targetIdx of
        Just (v, c) -> wireMatches myWire v c
        Nothing -> False

detonateProb :: PlayerId -> Int -> [WeightedWorld] -> Double
detonateProb targetPid targetIdx = ratio isRed
  where
    isRed w = case Map.lookup targetPid w >>= Map.lookup targetIdx of
        Just (_, Red) -> True
        _ -> False

wireMatches :: Wire -> Int -> WireColor -> Bool
wireMatches myWire targetVal targetColor = case color myWire of
    Blue -> targetColor == Blue && value myWire == targetVal
    Yellow -> targetColor == Yellow
    _ -> False

-- P that any wire in targetPid's hand matches myWire (for SuperDetector)
anyMatchProb :: Wire -> PlayerId -> [WeightedWorld] -> Double
anyMatchProb myWire targetPid ws
    | color myWire /= Blue = 0
    | otherwise = ratio anyMatch ws
  where
    anyMatch w = case Map.lookup targetPid w of
        Nothing -> False
        Just slots -> any (\(v, c) -> c == Blue && v == value myWire) (Map.elems slots)

-- P that at least one of the given positions matches myWire.
multiMatchProb :: Wire -> PlayerId -> [Int] -> [WeightedWorld] -> Double
multiMatchProb myWire targetPid positions = ratio anyMatch
  where
    anyMatch w = case Map.lookup targetPid w of
        Nothing -> False
        Just slots -> any (\pos -> case Map.lookup pos slots of
                                Just (v, c) -> wireMatches myWire v c
                                Nothing -> False) positions

-- P that pos matches w1 OR w2 (for DoubleRadiator).
doubleRadiatorProb :: Wire -> Wire -> PlayerId -> Int -> [WeightedWorld] -> Double
doubleRadiatorProb w1 w2 targetPid targetIdx = ratio matches
  where
    matches w = case Map.lookup targetPid w >>= Map.lookup targetIdx of
        Just (v, c) -> wireMatches w1 v c || wireMatches w2 v c
        Nothing -> False

-- Top N slot indices of a target player by match probability with myWire.
bestPositions :: Int -> BotKnowledge -> Wire -> PlayerId -> [WeightedWorld] -> [Int]
bestPositions n bk myWire targetPid worlds =
    take n . map fst . sortBy (comparing (Down . snd)) $
    [ (wireIdx, successProb myWire targetPid wireIdx worlds)
    | (wireIdx, slot) <- Map.toList (Map.findWithDefault Map.empty targetPid (playerSlots bk))
    , not (isCut slot)
    ]

isCut :: SlotKnowledge -> Bool
isCut (KnownCut _) = True
isCut _ = False

-- Move candidate generation, called after world enumeration.

allMoveCandidates :: BotKnowledge -> PlayerId -> [WeightedWorld] -> [BotMove]
allMoveCandidates bk selfPid worlds =
    dualCutMoves ++ equipmentMoves ++ doubleDetectorMoves
  where
    selfSlots = Map.findWithDefault Map.empty selfPid (playerSlots bk)
    otherPids = [ pid | pid <- Map.keys (playerSlots bk), pid /= selfPid ]

    blueWires =
        [ (wireIdx, w)
        | (wireIdx, KnownValue w) <- Map.toAscList selfSlots
        , color w == Blue
        ]

    -- DoubleRadiator accepts yellow as well as blue, and the same wire
    -- can be named twice to call out only a single value.
    callableWires =
        [ (wireIdx, w)
        | (wireIdx, KnownValue w) <- Map.toAscList selfSlots
        , color w == Blue || color w == Yellow
        ]

    -- if no safe move exists, the bot picks the least-risky high-pSuc one and (below) burns Stabilizer
    dualCutMoves =
        [ (Just (DualCut targetPid (myIdx, targetIdx) myWire), [], pSuc)
        | (myIdx, KnownValue myWire) <- Map.toAscList selfSlots
        , color myWire `notElem` [Grey, Red]
        , targetPid <- otherPids
        , (targetIdx, targetSlot) <- Map.toList (Map.findWithDefault Map.empty targetPid (playerSlots bk))
        , not (isCut targetSlot)
        , let pSuc = successProb myWire targetPid targetIdx worlds
        , pSuc > 0
        ]

    -- Equipment-based moves (global equipment list, excluding Stabilizer).
    equipmentMoves = concatMap tryEquipment (availableEquipment bk)

    tryEquipment eq = case name eq of
        SuperDetector ->
            [ (Nothing, [UseSuperDetector targetPid wireIdx myWire], pSuc)
            | (wireIdx, myWire) <- blueWires
            , targetPid <- otherPids
            , let pSuc = anyMatchProb myWire targetPid worlds
            , pSuc > 0
            ]

        TripleDetector ->
            [ (Nothing, [UseTripleDetector targetPid wireIdx (p1, p2, p3) myWire], pSuc)
            | (wireIdx, myWire) <- blueWires
            , targetPid <- otherPids
            , [p1, p2, p3] <- [bestPositions 3 bk myWire targetPid worlds]
            , let pSuc = multiMatchProb myWire targetPid [p1, p2, p3] worlds
            , pSuc > 0
            ]

        DoubleRadiator ->
            [ (Nothing, [UseDoubleRadiator targetPid (i1, i2) targetIdx (w1, w2)], pSuc)
            | (i1, w1) <- callableWires
            , (i2, w2) <- callableWires
            , i1 <= i2
            , targetPid <- otherPids
            , (targetIdx, targetSlot) <- Map.toList (Map.findWithDefault Map.empty targetPid (playerSlots bk))
            , not (isCut targetSlot)
            , let pSuc = doubleRadiatorProb w1 w2 targetPid targetIdx worlds
            , pSuc > 0
            ]

        _ -> []

    -- Per-player DoubleDetector
    doubleDetectorMoves
        | not (hasDoubleDetector bk) = []
        | otherwise =
            [ (Nothing, [UseDoubleDetector targetPid wireIdx (p1, p2) myWire], pSuc)
            | (wireIdx, myWire) <- blueWires
            , targetPid <- otherPids
            , [p1, p2] <- [bestPositions 2 bk myWire targetPid worlds]
            , let pSuc = multiMatchProb myWire targetPid [p1, p2] worlds
            , pSuc > 0
            ]

-- tie breaker to avoid cutting own info marker if possible
cutsOwnInfoMarker :: BotKnowledge -> BotMove -> Bool
cutsOwnInfoMarker bk (act, eqs, _) = actHits || any eqHits eqs
  where
    marked = selfMarkedSlots bk
    actHits = case act of
        Just (DualCut _ (myIdx, _) _) -> myIdx `elem` marked
        Just (SoloCut2 (a, b)) -> any (`elem` marked) [a, b]
        Just (SoloCut4 (a, b, c, d)) -> any (`elem` marked) [a, b, c, d]
        Just (RemoveRed i) -> i `elem` marked
        _                                 -> False
    -- DoubleRadiator cuts the matching called wire; we can't predict which
    -- of i1/i2 that will be, so we treat either landing on a marker as a hit.
    eqHits (UseSuperDetector _ myIdx _) = myIdx `elem` marked
    eqHits (UseTripleDetector _ myIdx _ _) = myIdx `elem` marked
    eqHits (UseDoubleDetector _ myIdx _ _) = myIdx `elem` marked
    eqHits (UseDoubleRadiator _ (i1, i2) _ _) = i1 `elem` marked || i2 `elem` marked
    eqHits _ = False

-- Tie-break helpers used by chooseBotAction's sortKey.

botCutInfo :: BotMove -> Maybe (Int, Int)
botCutInfo (Just (DualCut _ (myIdx, _) myWire), _, _) = Just (myIdx, value myWire)
botCutInfo (_, [UseSuperDetector _ myIdx myWire], _) = Just (myIdx, value myWire)
botCutInfo (_, [UseTripleDetector _ myIdx _ myWire], _) = Just (myIdx, value myWire)
botCutInfo (_, [UseDoubleDetector _ myIdx _ myWire], _) = Just (myIdx, value myWire)
botCutInfo _ = Nothing

-- setup for solocut 
enables3KindSetup :: BotKnowledge -> PlayerId -> BotMove -> Bool
enables3KindSetup bk selfPid move = case botCutInfo move of
    Just (_, v) -> liveBlueCount bk selfPid v == 3
    Nothing -> False

liveBlueCount :: BotKnowledge -> PlayerId -> Int -> Int
liveBlueCount bk selfPid v = length
    [ ()
    | (_, KnownValue w) <- Map.toList (Map.findWithDefault Map.empty selfPid (playerSlots bk))
    , color w == Blue
    , value w == v
    ]

-- checks if we can enclose wires inside "walls", which are cut wires where all instances have been cut.
-- If for example we have wires [1][2][2][2][walls 3], it would make the most sense to cut
-- the leftmost [2], so we enclose the others inside the "walls".
-- When the gap between wall value and wire value still has some uncut value
-- that could fit between them, cutting adjacent to the wall is informative.
-- When no value can fit between (all such values exhausted), the adjacency
-- bonus is wasted, so we reward the outermost cut instead.
wallFitness :: BotKnowledge -> PlayerId -> BotMove -> Int
wallFitness bk selfPid move = case botCutInfo move of
    Nothing -> 0
    Just (myIdx, v) ->
        let slots = Map.findWithDefault Map.empty selfPid (playerSlots bk)
            handLen = Map.size slots
            leftP = closestLeftWall  bk selfPid myIdx
            rightP = closestRightWall bk selfPid handLen myIdx
            leftV = wallValOf bk selfPid handLen leftP
            rightV = wallValOf bk selfPid handLen rightP
            leftDist = myIdx - leftP
            rightDist = rightP - myIdx
            leftFit = blueFitExists bk leftV v
            rightFit = blueFitExists bk rightV v
        in sideScore handLen leftDist (abs (v - leftV)) leftFit
           + sideScore handLen rightDist (abs (v - rightV)) rightFit
  where
    sideScore handLen distance diff fitExists
        | diff == 1 = distance
        | diff == 2 && fitExists && distance == 1 = handLen
        | diff >= 2 && not fitExists = distance
        | otherwise = 0

-- Whether any blue value strictly between the two endpoints still has
-- uncut copies in the deck (cut count < 4).
blueFitExists :: BotKnowledge -> Int -> Int -> Bool
blueFitExists bk a b =
    any (\v -> Map.findWithDefault 0 v (cutBlueCounts bk) < 4)
        [min a b + 1 .. max a b - 1]

closestLeftWall :: BotKnowledge -> PlayerId -> Int -> Int
closestLeftWall bk selfPid newPos =
    case [ p | p <- [newPos-1, newPos-2 .. 0], isOwnWall bk selfPid p ] of
        (p:_) -> p
        [] -> -1

closestRightWall :: BotKnowledge -> PlayerId -> Int -> Int -> Int
closestRightWall bk selfPid handLen newPos =
    case [ p | p <- [newPos+1 .. handLen-1], isOwnWall bk selfPid p ] of
        (p:_) -> p
        [] -> handLen

isOwnWall :: BotKnowledge -> PlayerId -> Int -> Bool
isOwnWall bk selfPid pos =
    case Map.lookup pos (Map.findWithDefault Map.empty selfPid (playerSlots bk)) of
        Just (KnownCut w)
            | color w == Blue -> Map.findWithDefault 0 (value w) (cutBlueCounts bk) >= 4
            | color w == Yellow -> length (revealedYellow bk) == 2
            | color w == Red -> True
            | otherwise -> False
        _ -> False

wallValOf :: BotKnowledge -> PlayerId -> Int -> Int -> Int
wallValOf bk selfPid handLen pos
    | pos == -1 = 0
    | pos == handLen = 13
    | otherwise = case Map.lookup pos (Map.findWithDefault Map.empty selfPid (playerSlots bk)) of
        Just (KnownCut w) -> value w
        _ -> 0

-- unlocking equipment with cut wire 
unlocksLocked :: BotKnowledge -> BotMove -> Bool
unlocksLocked bk move = case botCutInfo move of
    Just (_, v) -> v `elem` lockedUnlockValues bk
    Nothing -> False

-- computes wether action could cut a red wire 
moveIsSafe :: [WeightedWorld] -> BotMove -> Bool
moveIsSafe worlds (act, eqs, _) = case act of
    Just (DualCut targetPid (_, targetIdx) _) ->
        detonateProb targetPid targetIdx worlds <= maxDetonateProb
    _ -> case eqs of
        [UseDoubleRadiator targetPid _ targetIdx _] ->
            detonateProb targetPid targetIdx worlds <= maxDetonateProb
        _ -> True

-- sorting actions and applying tie breakers 
moveSortKey :: BotKnowledge -> PlayerId -> [WeightedWorld] -> BotMove
            -> (Bool, Down Double, Bool, Bool, Down Int, Bool)
moveSortKey bk selfPid worlds move@(_, _, p) =
    ( not (moveIsSafe worlds move)
    , Down p
    , cutsOwnInfoMarker bk move
    , not (enables3KindSetup bk selfPid move)
    , Down (wallFitness bk selfPid move)
    , not (unlocksLocked bk move)
    )

-- Top-level decision (called only when no certain action is available).

chooseBotAction :: BotKnowledge -> PlayerId -> [WeightedWorld]
                -> (Maybe Action, [EquipmentUse])
chooseBotAction bk selfPid worlds =
    case sortBy (comparing (moveSortKey bk selfPid worlds))
                (allMoveCandidates bk selfPid worlds) of
        [] -> (Nothing, [])
        move@(act, equip, pSuc):_ ->
            -- use stabilizer if move is either uncertain or unsafe;
            -- applies to DualCut and to any cutting equipment move
            let extraEquip = [ UseStabilizer
                             | stabilizerAvailable bk
                             , pSuc < stabilizerThreshold || not (moveIsSafe worlds move)
                             ]
            in (act, extraEquip ++ equip)

stabilizerAvailable :: BotKnowledge -> Bool
stabilizerAvailable bk = any ((== Stabilizer) . name) (availableEquipment bk)
