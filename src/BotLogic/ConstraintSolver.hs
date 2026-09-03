module BotLogic.ConstraintSolver where

import Data.SBV
import Data.SBV.Control
import Types
import BotLogic.BotTypes
import Config (maxModels)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Control.Monad (forM, forM_, when, unless)

-- Wire color encoding
toColorCode :: WireColor -> Integer
toColorCode Blue = 0
toColorCode Yellow = 1
toColorCode Red = 2
toColorCode _ = 0

fromColorCode :: Integer -> WireColor
fromColorCode 0 = Blue
fromColorCode 1 = Yellow
fromColorCode _ = Red


unknownSlotDefs :: BotKnowledge -> [(PlayerId, Int, [Wire])]
unknownSlotDefs bk =
    [ (pid, wireIdx, candidates)
    | (pid, slots) <- Map.toList (playerSlots bk)
    , (wireIdx, PossibleValue candidates) <- Map.toList slots
    , not (null candidates)
    ]

-- Enumeration: one z3 process, incremental checkSat + blocking clauses.

enumerateWorlds :: BotKnowledge -> IO [WorldAssignment]
enumerateWorlds bk = runSMTWith z3 $ do
        symPairs <- buildConstraintsWithVars bk
        query $ collectModels maxModels bk symPairs

-- Build symbolic variables + constraints, returning vars for the Query phase.
buildConstraintsWithVars :: BotKnowledge -> Symbolic [(SlotId, SlotSym)]
buildConstraintsWithVars bk = do
    let defs = unknownSlotDefs bk
    pairs <- forM defs $ \(pid, wireIdx, candidates) -> do
        slotSym <- mkSlotSym (pid, wireIdx, candidates)
        return ((pid, wireIdx), slotSym)
    let syms = map snd pairs
        symMap = pairs
    applyCardinality bk syms
    applyHandSorting bk symMap
    applyHandConstraints bk symMap
    return pairs

-- Enumerate up to n models by looping checkSat and adding blocking clauses.
collectModels :: Int -> BotKnowledge -> [(SlotId, SlotSym)] -> Query [WorldAssignment]
collectModels maxN bk symPairs = go maxN []
  where
    go 0 acc = return acc
    go n acc = do
        res <- checkSat
        case res of
            Sat -> do
                vals <- forM symPairs $ \(slotId, (sv, sc)) -> do
                    v <- getValue sv
                    c <- getValue sc
                    return (slotId, v, c)
                let world = buildWorldQ bk vals
                -- Block this exact assignment so the next checkSat finds a different one.
                constrain $ sOr [ sv ./= literal v .|| sc ./= literal c | ((_, (sv, sc)), (_, v, c)) <- zip symPairs vals ]
                go (n - 1) (world : acc)
            _ -> return acc

-- Build a WorldAssignment from getValue results.
buildWorldQ :: BotKnowledge -> [(SlotId, Integer, Integer)] -> WorldAssignment
buildWorldQ bk slotVals = Map.fromList
    [ ( pid
      , Map.fromList (catMaybes [ wireAt pid wireIdx sk
                                | (wireIdx, sk) <- Map.toList slots ])
      )
    | (pid, slots) <- Map.toList (playerSlots bk)
    ]
  where
    symLookup = Map.fromList [(slotId, (v, c)) | (slotId, v, c) <- slotVals]

    wireAt _ wireIdx (KnownValue w)
        | color w == Grey = Nothing
        | otherwise = Just (wireIdx, (value w, color w))
    wireAt _ _ (KnownCut _) = Nothing
    wireAt pid wireIdx (PossibleValue cs)
        | null cs = Nothing
        | otherwise = case Map.lookup (pid, wireIdx) symLookup of
            Just (v, c) -> Just (wireIdx, (fromIntegral v, fromColorCode c))
            Nothing -> Nothing

-- Constraint construction (Symbolic monad)

mkSlotSym :: (PlayerId, Int, [Wire]) -> Symbolic SlotSym
mkSlotSym (pid, wireIdx, candidates) = do
    let prefix = "p" ++ show pid ++ "s" ++ show wireIdx
    v <- free (prefix ++ "v") :: Symbolic SInteger
    c <- free (prefix ++ "c") :: Symbolic SInteger
    constrain $ sOr
        [ v .== literal (fromIntegral (value w) :: Integer)
          .&& c .== literal (toColorCode (color w))
        | w <- candidates
        ]
    return (v, c)

applyCardinality :: BotKnowledge -> [SlotSym] -> Symbolic ()
applyCardinality bk symSlots = do
    forM_ [ (v, n) | ((v, Blue), n) <- Map.toList (remainingCounts bk) ] $ \(v, n) ->
        constrain $ blueSum v .== literal (fromIntegral (max 0 n) :: Integer)

    let unseenYellow = max 0 (2 - length (revealedYellow bk))
    when (not (null (yellowPool bk)) || unseenYellow == 0) $ do
        constrain $ colorSum 1 .== literal (fromIntegral unseenYellow :: Integer)
        forM_ (yellowPool bk) $ \v ->
            constrain $ yellowValueSum v .<= literal (1 :: Integer)
    when (seenRed bk || not (null (redPool bk))) $
        constrain $ colorSum 2 .== literal (if seenRed bk then (0 :: Integer) else 1)
  where
    blueSum v = sum
        [ ite (sv .== literal (fromIntegral v :: Integer) .&& sc .== literal (0 :: Integer))
              (1 :: SInteger) 0
        | (sv, sc) <- symSlots
        ]
    colorSum code = sum
        [ ite (sc .== literal (code :: Integer)) (1 :: SInteger) 0
        | (_, sc) <- symSlots
        ]
    yellowValueSum v = sum
        [ ite (sv .== literal (fromIntegral v :: Integer) .&& sc .== literal (1 :: Integer))
              (1 :: SInteger) 0
        | (sv, sc) <- symSlots
        ]

applyHandSorting :: BotKnowledge -> [(SlotId, SlotSym)] -> Symbolic ()
applyHandSorting bk symMap =
    forM_ (Map.toList (playerSlots bk)) $ \(pid, slots) -> do
        let encoded = catMaybes [ encodeSlot pid wireIdx sk | (wireIdx, sk) <- Map.toAscList slots ]
        forM_ (zip encoded (drop 1 encoded)) (uncurry constrainLt)
  where
    encodeSlot _ _ (PossibleValue []) = Nothing
    encodeSlot _ _ (KnownValue w) = Just (Left w)
    encodeSlot _ _ (KnownCut w)   = Just (Left w)
    encodeSlot pid wireIdx (PossibleValue _) = Right <$> lookup (pid, wireIdx) symMap

    constrainLt (Left w1) (Left w2) =
        constrain $ literal ( (value w1, toColorCode (color w1)) <= (value w2, toColorCode (color w2)))
    constrainLt (Left w1) (Right (sv2, sc2)) = constrain $ v1 .< sv2 .|| (v1 .== sv2 .&& c1 .<= sc2)
      where
        v1 = literal (fromIntegral (value w1) :: Integer)
        c1 = literal (toColorCode (color w1))
    constrainLt (Right (sv1, sc1)) (Left w2) = constrain $ sv1 .< v2 .|| (sv1 .== v2 .&& sc1 .<= c2)
      where
        v2 = literal (fromIntegral (value w2) :: Integer)
        c2 = literal (toColorCode (color w2))
    constrainLt (Right (sv1, sc1)) (Right (sv2, sc2)) = constrain $ sv1 .< sv2 .|| (sv1 .== sv2 .&& sc1 .<= sc2)

-- derive from failed cuts as disjunction, "player must have value that he called out somewhere"
applyHandConstraints :: BotKnowledge -> [(SlotId, SlotSym)] -> Symbolic ()
applyHandConstraints bk symMap =
    forM_ (handConstraints bk) $ \hc -> case hc of
        HasBlue pid v -> emitConstraint pid (matchBlue v) (eqBlue v)
        HasYellow pid -> emitConstraint pid matchYellow eqYellow
        SlotsNotBlue pid slots v -> emitExclusion pid v (`elem` slots)
        HandNotBlue pid v -> emitExclusion pid v (const True)
        SlotIsRed pid idx -> emitSlotIsRed pid idx
  where
    matchBlue v (KnownValue w) = color w == Blue && value w == v
    matchBlue v (KnownCut w) = color w == Blue && value w == v
    matchBlue _ _ = False

    matchYellow (KnownValue w) = color w == Yellow
    matchYellow (KnownCut w) = color w == Yellow
    matchYellow _ = False

    eqBlue v (sv, sc) =
        sv .== literal (fromIntegral v :: Integer)
        .&& sc .== literal (toColorCode Blue)

    eqYellow (_, sc) = sc .== literal (toColorCode Yellow)

    neqBlue v (sv, sc) =
        sv ./= literal (fromIntegral v :: Integer)
        .|| sc ./= literal (toColorCode Blue)

    playerSyms pid = [ s | ((p, _), s) <- symMap, p == pid ]

    emitConstraint pid match eq =
        let slots = Map.findWithDefault Map.empty pid (playerSlots bk)
            playerKnown = not (Map.null slots)
            alreadyHeld = any match (Map.elems slots)
            syms = playerSyms pid
        in when (playerKnown && not alreadyHeld && not (null syms)) $
              constrain $ sOr (map eq syms)

    -- pin slot to be red (after edge case where stablizer was used on cut of red wire and no infomarker was placed)
    emitSlotIsRed pid idx =
        let pidSlots = Map.findWithDefault Map.empty pid (playerSlots bk)
            isNonRed (KnownValue w) = color w /= Red
            isNonRed (KnownCut w) = color w /= Red
            isNonRed _ = False
            conflictsWithKnown = case Map.lookup idx pidSlots of
                Just sk -> isNonRed sk
                Nothing -> False
        in unless conflictsWithKnown $
            case lookup (pid, idx) symMap of
                Just (_, sc) ->
                    constrain $ sc .== literal (toColorCode Red)
                Nothing -> pure ()

    -- Exclusions as conjunction
    emitExclusion pid v selIdx =
        let pidSlots = Map.findWithDefault Map.empty pid (playerSlots bk)
            isBlueV (KnownValue w) = color w == Blue && value w == v
            isBlueV (KnownCut w) = color w == Blue && value w == v
            isBlueV _ = False
            conflictsWithKnown =
                any (\(idx, sk) -> selIdx idx && isBlueV sk)
                    (Map.toList pidSlots)
        in unless conflictsWithKnown $
            forM_ symMap $ \((p, idx), slotSym) ->
                when (p == pid && selIdx idx) $
                    constrain (neqBlue v slotSym)

