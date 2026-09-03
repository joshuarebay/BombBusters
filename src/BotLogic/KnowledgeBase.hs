module BotLogic.KnowledgeBase where

import Types
import BotLogic.BotTypes
import BotLogic.Inference (inferHandConstraints)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Control.Applicative ((<|>))

fullBlueDeck :: Map (Int, WireColor) Int
fullBlueDeck = Map.fromList [((v, Blue), 4) | v <- [1..12]]

distillKnowledge :: PlayerView -> BotKnowledge
distillKnowledge pv =
    let
        myWires = playerWires (pvSelf pv)
        cutWires' = map wire (pvCutWires pv)
        selfPid' = playerId (pvSelf pv)
        cutPositions = [(player mw, originalIndex mw) | mw <- pvCutWires pv]
        infoWires  = [ Wire v c i | InfoMarker owner i v c <- pvInfoMarkers pv
                     , owner /= selfPid'
                     , (owner, i) `notElem` cutPositions ]

        allSeenWires = myWires ++ cutWires' ++ infoWires
        seenBlue = filter ((== Blue) . color) allSeenWires
        seenYellow = filter ((== Yellow) . color) allSeenWires
        seenRed' = any ((== Red) . color) (myWires ++ cutWires')

        remCounts = foldl' (\m w -> Map.insertWith (+) (value w, color w) (-1) m) fullBlueDeck seenBlue

        redMarkers = [ value w | w <- pvColorMarkers pv, color w == Red ]
        yellowMarkers = [ value w | w <- pvColorMarkers pv, color w == Yellow ]

        ctx = SlotContext
            { scRemainingBlue = remCounts
            , scYellowPool = yellowMarkers
            , scRevealedYellow = map value seenYellow
            , scUnseenYellow = 2 - length seenYellow
            , scRedPool = redMarkers
            , scSeenRed = seenRed'
            }

        selfSlots = Map.fromList
            [ (wireIndex w, selfSlot (pvCutWires pv) selfPid' w) | w <- myWires ]
        otherSlots = Map.fromList
            [ (playerId p, mapToSlots p (pvInfoMarkers pv) (pvCutWires pv) ctx) | p <- pvOthers pv ]
        allSlots = Map.insert (playerId (pvSelf pv)) selfSlots otherSlots

        availEq = filter (\e -> unlocked e && not (used e)) (pvEquipment pv)
        lockedUnlockVals = [unlockingVal e | e <- pvEquipment pv, not (unlocked e)]
        hasDblDet = doubleDetector (pvSelf pv)

        cutBlueCnts = Map.fromListWith (+)
            [ (value w, 1) | w <- cutWires', color w == Blue ]

        selfMarked = [ markerIndex m
                     | m <- pvInfoMarkers pv
                     , markerOwner m == selfPid'
                     , (markerOwner m, markerIndex m) `notElem` cutPositions
                     ]

        handCons = inferHandConstraints (pvTurnHistory pv) (pvCutWires pv) (pvInfoMarkers pv)

    in BotKnowledge
        { remainingCounts = remCounts
        , redPool = redMarkers
        , yellowPool = yellowMarkers
        , playerSlots = allSlots
        , revealedYellow = map value seenYellow
        , currentDetonator = pvDetonator pv
        , availableEquipment = availEq
        , hasDoubleDetector = hasDblDet
        , cutBlueCounts = cutBlueCnts
        , selfMarkedSlots = selfMarked
        , handConstraints = handCons
        , seenRed = seenRed'
        , lockedUnlockValues = lockedUnlockVals
        }

selfSlot :: [MarkedWire] -> PlayerId -> Wire -> SlotKnowledge
selfSlot cuts pid w
    | color w == Grey = case cutAt cuts pid (wireIndex w) of
        Just mw -> KnownCut (wire mw)
        Nothing -> PossibleValue []
    | otherwise = KnownValue w

cutAt :: [MarkedWire] -> PlayerId -> Int -> Maybe MarkedWire
cutAt cuts pid i = find (\mw -> player mw == pid && originalIndex mw == i) cuts

mapToSlots :: Player -> [InfoMarker] -> [MarkedWire] -> SlotContext -> Map Int SlotKnowledge
mapToSlots p markers cuts ctx =
    applyBounds $ Map.fromList
        [ (wireIndex w, slotFor w) | w <- playerWires p ]
  where
    slotFor w
        | color w == Grey = case cutAt cuts (playerId p) (wireIndex w) of
            Just mw -> KnownCut (wire mw)
            Nothing -> PossibleValue []
        | otherwise = baseSlot p markers ctx w

baseSlot :: Player -> [InfoMarker] -> SlotContext -> Wire -> SlotKnowledge
baseSlot p markers ctx w =
    case find (\m -> markerOwner m == playerId p && markerIndex m == wireIndex w) markers of
        Just m  -> KnownValue (Wire (markerValue m) (markerColor m) (wireIndex w))
        Nothing -> PossibleValue (allCandidates ctx)

allCandidates :: SlotContext -> [Wire]
allCandidates ctx =
    [ Wire v Blue 0 | ((v, Blue), cnt) <- Map.toList (scRemainingBlue ctx), cnt > 0 ]
    ++
    [ Wire v Yellow 0 | v <- scYellowPool ctx \\ scRevealedYellow ctx, scUnseenYellow ctx > 0 ]
    ++
    [ Wire v Red 0 | v <- scRedPool ctx, not (scSeenRed ctx) ]

-- Narrow each PossibleValue slot using the nearest KnownValue/KnownCut bounds in wireIndex order
applyBounds :: Map Int SlotKnowledge -> Map Int SlotKnowledge
applyBounds slots = Map.fromList (zipWith3 withBounds lowers ascPairs uppers)
  where
    ascPairs = Map.toAscList slots
    sks = map snd ascPairs
    fixed (KnownValue w) = Just w
    fixed (KnownCut w) = Just w
    fixed _ = Nothing
    lowers = init $ scanl (\acc sk -> fixed sk <|> acc) Nothing sks
    uppers = drop 1 $ scanr (\sk acc -> fixed sk <|> acc) Nothing sks

    withBounds lo (wireIdx, sk) hi = (wireIdx, narrow lo sk hi)

    narrow lo (PossibleValue cs) hi = PossibleValue [ w | w <- cs, above lo w, below hi w ]
    narrow _ sk _ = sk

    above Nothing _ = True
    above (Just l) w = (value w, color w) >= (value l, color l)

    below Nothing _ = True
    below (Just h) w = (value w, color w) <= (value h, color h)

findColorValue :: WireColor -> [Wire] -> Maybe Int
findColorValue c ws = value <$> find ((== c) . color) ws
