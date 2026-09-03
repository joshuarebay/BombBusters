module BotLogic.Interface where

import Types
import BotLogic.KnowledgeBase
import BotLogic.ConstraintSolver
import BotLogic.Strategies
import BotLogic.Inference (inferSoftFacts, worldWeight)
import Control.Applicative ((<|>))
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)

botAction :: PlayerView -> IO (Maybe Action, [EquipmentUse])
botAction pv = do
    let bk = distillKnowledge pv
        selfPid = playerId (pvSelf pv)
    case certainAction bk selfPid of
        Just act -> return (Just act, [])
        Nothing -> do
            worlds <- enumerateWorlds bk
            let facts = inferSoftFacts
                            (pvTurnHistory pv)
                            (pvInfoMarkers pv)
                            (pvCutWires pv)
                            (length (pvTurnOrder pv))
                weighted = [ (w, worldWeight facts w) | w <- worlds ]
            return (chooseBotAction bk selfPid weighted)

-- initial infomarker placement
botPlaceMarker :: PlayerView -> Action
botPlaceMarker pv =
    let allBlues = filter ((== Blue) . color) handWires
        takenVals = map markerValue (pvInfoMarkers pv)
        unlockVals = map fst (pvUnlockingValues pv)
        blues = case filter (\w -> blueCount (value w) /= 4) allBlues of
            [] -> allBlues
            xs -> xs
        pick = fromMaybe (middleFallback blues takenVals)
             $ innerEdgePick blues takenVals
             <|> threeOfAKindPick blues
             <|> unlockingPick blues takenVals unlockVals
    in PlaceInfoMarker (wireIndex pick)
  where
    handWires = playerWires (pvSelf pv)
    blueCount v = length [ w | w <- handWires, color w == Blue, value w == v ]

    innerEdgePick ws taken =
        let ones = [w | w <- ws, 1 `notElem` taken, value w == 1]
            twelves = [w | w <- ws, 12 `notElem` taken, value w == 12]
            innerOne = case reverse ones of w : _ -> Just w; [] -> Nothing
            innerTwelve = case twelves of w : _ -> Just w; [] -> Nothing
        in case (length ones >= 2, length twelves >= 2) of
            (True, True)   -> if length ones >= length twelves
                                then innerOne
                                else innerTwelve
            (True, False) -> innerOne
            (False, True) -> innerTwelve
            (False, False) -> Nothing

    threeOfAKindPick ws =
        listToMaybe [ w | w <- ws, blueCount (value w) == 3 ]

    unlockingPick ws taken unlocks =
        listToMaybe [ w | w <- ws
                        , value w `elem` unlocks
                        , value w `notElem` taken ]

    middleFallback ws taken =
        let middle = ws !! (length ws `div` 2)
            fresh = filter (\w -> value w `notElem` taken) ws
        in if value middle `notElem` taken
             then middle
           else if not (null fresh)
             then fresh !! (length fresh `div` 2)
           else middle

botPendingChoice :: PlayerView -> ChoiceKind -> [Int] -> Int
botPendingChoice _ _ [] = 0
botPendingChoice pv PickWireToCut opts@(first:_) =
    case preferUnmarked cleanOpts of
        [] -> first
        [x] -> x
        xs@(_:_:_) -> chooseFromMany xs
  where
    handWires = playerWires (pvSelf pv)
    handLen = length handWires
    selfPid = playerId (pvSelf pv)
    validIdx i = i >= 0 && i < handLen
    cleanOpts = sort (filter validIdx opts)

    -- Avoid cutting the bot's own info-markered wires when alternatives exist.
    selfMarkers =
        [ markerIndex m | m <- pvInfoMarkers pv, markerOwner m == selfPid ]
    preferUnmarked xs = case filter (`notElem` selfMarkers) xs of
        [] -> xs
        ys -> ys

    ownCutAt pos =
        listToMaybe
            [ wire mw
            | mw <- pvCutWires pv
            , player mw == selfPid
            , originalIndex mw == pos
            ]

    totalInDeck c = case c of
        Blue -> 4
        Yellow -> 2
        Red -> 1
        _ -> 0
    cutOfKind v c =
        length [ mw | mw <- pvCutWires pv
                    , value (wire mw) == v
                    , color (wire mw) == c ]
    exhausted v c = cutOfKind v c >= totalInDeck c

    isWall pos
        | pos == -1 || pos == handLen = True
        | not (validIdx pos) = False
        | otherwise = case ownCutAt pos of
            Just w -> exhausted (value w) (color w)
            Nothing -> False
    -- Like isWall but excludes the rack-edge sentinels, so adjacency and
    -- "bracketed middle" rules only fire next to genuine cut walls.
    isRealWall pos
        | pos == -1 || pos == handLen = False
        | not (validIdx pos) = False
        | otherwise = case ownCutAt pos of
            Just w -> exhausted (value w) (color w)
            Nothing -> False
    wallValueAt pos
        | pos == -1 = 0
        | pos == handLen = 13
        | otherwise = maybe 0 value (ownCutAt pos)

    -- Whether any blue value strictly between the two endpoints still has
    -- uncut copies in the deck.
    fitExistsBetween a b =
        any (\v -> not (exhausted v Blue)) [min a b + 1 .. max a b - 1]

    chooseFromMany [] = first
    chooseFromMany xs@(leftmost : _) =
        let rightmost = last xs
            optValue = value (handWires !! leftmost)
            leftWallVal = wallValueAt (leftmost - 1)
            rightWallVal = wallValueAt (rightmost + 1)
            leftIsWall = isWall (leftmost - 1)
            rightIsWall = isWall (rightmost + 1)
            leftReal = isRealWall (leftmost - 1)
            rightReal = isRealWall (rightmost + 1)
            leftDiff = abs (optValue - leftWallVal)
            rightDiff = abs (optValue - rightWallVal)
            leftFit = fitExistsBetween leftWallVal optValue
            rightFit = fitExistsBetween rightWallVal optValue
            middleOpt = xs !! (length xs `div` 2)
        in if leftReal && rightReal && length xs == 3
                then middleOpt
            else if leftReal && leftDiff >= 2 && leftFit then leftmost
            else if rightReal && rightDiff >= 2 && rightFit then rightmost
            else if leftReal && leftDiff >= 2 && not leftFit then rightmost
            else if rightReal && rightDiff >= 2 && not rightFit then leftmost
            else if leftIsWall && leftDiff == 1 then rightmost
            else if rightIsWall && rightDiff == 1 then leftmost
            else first
botPendingChoice pv PickInfoMarkerSlot opts@(first:_) =
    fromMaybe first
        $   firstYellow
        <|> threeOfAKind
        <|> unlockingPicked
        <|> listToMaybe opts4
  where
    ws = playerWires (pvSelf pv)
    handLen = length ws
    selfPid = playerId (pvSelf pv)
    selfMarkers =
        [ markerIndex m | m <- pvInfoMarkers pv, markerOwner m == selfPid ]
    validIdx i = i >= 0 && i < handLen
    cleanOpts = filter validIdx opts
    valueOf i = value (ws !! i)

    firstYellow =
        listToMaybe [ i | i <- cleanOpts, color (ws !! i) == Yellow ]
    isBoundary pos =
        validIdx pos &&
        (color (ws !! pos) == Grey || pos `elem` selfMarkers)
    leftBoundOf i = case [ j | j <- [i-1, i-2 .. 0], isBoundary j ] of
        (j:_) -> j
        [] -> -1
    rightBoundOf i = case [ j | j <- [i+1 .. handLen-1], isBoundary j ] of
        (j:_) -> j
        [] -> handLen
    inBoundedGapLE n i =
        let l = leftBoundOf i
            r = rightBoundOf i
        in l >= 0 && r < handLen && r - l - 1 <= n

    opts3 = case filter (not . inBoundedGapLE 2) cleanOpts of
        [] -> case filter (not . inBoundedGapLE 1) cleanOpts of
            [] -> cleanOpts
            xs -> xs
        xs -> xs

    blueCount v = length [ w | w <- ws, color w == Blue, value w == v ]

    opts4 = case filter (\i -> blueCount (valueOf i) /= 4) opts3 of
        [] -> opts3
        xs -> xs

    threeOfAKind =
        listToMaybe [ i | i <- opts4, blueCount (valueOf i) == 3 ]

    lockedUnlockVals =
        [ unlockingVal e | e <- pvEquipment pv, not (unlocked e) ]
    unlockingPicked =
        listToMaybe [ i | i <- opts4, valueOf i `elem` lockedUnlockVals ]
