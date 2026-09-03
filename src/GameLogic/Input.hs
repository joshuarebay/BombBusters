module GameLogic.Input
  ( humanAction,
    handlePlaceInfoMarker,
    askPlayerToChooseWire,
    handleChooseBots,
    -- exposed for tests
    checkInputInitInfo,
    checkInputSC2,
    checkInputSC4,
    checkInputDC,
    checkInputRR,
    checkInputTD,
    checkInputDR,
    checkInputDD,
  )
where

import Data.List
import GameLogic.Display
import GameLogic.Util
import Types
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO, try)
import Control.Monad (when)
import Text.Read (readMaybe)

-- input handlers, this is where invalid input should be filtered out before envoking the pure handlers 

data GoBack = GoBack deriving Show
instance Exception GoBack

isBack :: String -> Bool
isBack s = s == "\ESC" || s == "back"

readIntB :: String -> IO Int
readIntB prompt = do
  putStrLn $ prompt ++ "  (ESC + Enter to go back)"
  putStr "> "
  input <- getLine
  if isBack input
    then throwIO GoBack
    else case readMaybe input :: Maybe Int of
      Just n -> return n
      Nothing -> do
        putStrLn "Invalid number. Please enter a valid integer."
        readIntB prompt

readLabelB :: String -> IO Int
readLabelB prompt = do
  putStrLn $ prompt ++ "  (ESC + Enter to go back)"
  putStr "> "
  line <- getLine
  if isBack line
    then throwIO GoBack
    else case line of
      [c] | c >= 'a' && c <= 'z' -> return (fromEnum c - fromEnum 'a')
      _ -> do
        putStrLn "Invalid input. Please enter a single lowercase letter."
        readLabelB prompt

promptFor :: ChoiceKind -> String
promptFor PickWireToCut = "Choose which matching wire to cut."
promptFor PickInfoMarkerSlot = "Place an Info Marker on a wire."

askPlayerToChooseWire :: PlayerView -> [Int] -> ChoiceKind -> IO Int
askPlayerToChooseWire pv opts kind = do
  displayWirePickView pv Nothing
  putStrLn $ "Player " ++ show (playerId $ pvSelf pv) ++ " - " ++ promptFor kind
  putStrLn $ "Options: " ++ unwords (map indexToLabel opts)
  readChoice
  where
    readChoice = do
      putStr "> "
      line <- getLine
      case line of
        [c] | c >= 'a' && c <= 'z' ->
          let n = fromEnum c - fromEnum 'a'
           in if n `elem` opts
                then pure n
                else do
                  putStrLn "Invalid choice. Please pick from the listed options."
                  threadDelay 1000000 >> readChoice
        _ -> do
          putStrLn "Invalid input. Please enter a single lowercase letter (e.g. a, b, c)."
          readChoice

humanAction :: PlayerView -> IO (Maybe Action, [EquipmentUse])
humanAction pv = do
  result <- try (humanActionAttempt pv)
  case result of
    Left GoBack -> humanAction pv
    Right r    -> pure r

humanActionAttempt :: PlayerView -> IO (Maybe Action, [EquipmentUse])
humanActionAttempt pv = do
  choice <- chooseActionOrEquipment pv
  case choice of
    Left action -> pure (Just action, [])
    Right [] -> humanActionAttempt pv
    Right eq -> do
      eqUse <- mapM (chooseEquipmentUse pv) eq
      if all isStabilizer eq
        then do
          act <- chooseAction pv
          pure (Just act, eqUse)
        else pure (Nothing, eqUse)

chooseEquipmentUse :: PlayerView -> Selectable -> IO EquipmentUse
chooseEquipmentUse pv s = case s of
  SelectEquipment e ->
    case name e of
      Stabilizer -> pure UseStabilizer
      SuperDetector -> handleSuperDetectorInput pv
      TripleDetector -> handleTripleDetectorInput pv
      DoubleRadiator -> handleDoubleRadiatorInput pv
  SelectDoubleDetector -> handleDoubleDetectorInput pv

-- Number of uncut wires still on a player's rack.
uncutOn :: Player -> Int
uncutOn p = length [ w | w <- playerWires p, color w /= Grey ]

-- Number of uncut wires still on each other player's rack.
otherUncutCounts :: PlayerView -> [Int]
otherUncutCounts pv = map uncutOn (pvOthers pv)

maxOtherUncut :: PlayerView -> Int
maxOtherUncut pv = maximum (0 : otherUncutCounts pv)

-- A multi-slot detector requires at least one other player with enough
-- distinct uncut positions to fill its query. SuperDetector queries the
-- whole rack, so any uncut wire suffices.
canUseEquipment :: PlayerView -> EquipmentName -> Bool
canUseEquipment pv nm = case nm of
  TripleDetector -> maxOtherUncut pv >= 3
  SuperDetector  -> maxOtherUncut pv >= 1
  Stabilizer     -> True
  DoubleRadiator -> maxOtherUncut pv >= 1

availableEquipment :: PlayerView -> [Selectable]
availableEquipment pv =
  let eqs = map SelectEquipment $
              filter (\e -> not (used e) && canUseEquipment pv (name e))
                     (unlockedEquipment pv)
      dd = [ SelectDoubleDetector
           | doubleDetector (pvSelf pv), maxOtherUncut pv >= 2
           ]
   in eqs ++ dd

chooseEquipment :: PlayerView -> IO [Selectable]
chooseEquipment pv = do
  displayPlayerView pv
  let available = availableEquipment pv
  if null available
    then do
      putStrLn "No equipment available."
      pure []
    else do
      putStrLn $ "Player " ++ show (playerId (pvSelf pv)) ++ " - choose equipment:"
      putStrLn "---------------------------------------------"
      selectLoop available []

selectLoop :: [Selectable] -> [Selectable] -> IO [Selectable]
selectLoop remaining selected = do
  let allowed = allowedRemaining remaining selected
  if null allowed
    then pure selected
    else do
      let indexed = zip [1 .. length allowed] allowed
      mapM_
        (\(i, s) -> putStrLn (show i ++ " | " ++ showSelectable s))
        indexed
      input <- readIntB "Enter number:"
      case input of
        n
          | n >= 1 && n <= length allowed -> do
              let eq = allowed !! (n - 1)
                  remaining' = filter (/= eq) remaining
              selectLoop remaining' (eq : selected)
        _ -> do
          putStrLn "Invalid input, try again."
          threadDelay 1000000 >> selectLoop remaining selected

showSelectable :: Selectable -> String
showSelectable (SelectEquipment e) =
  show (name e) ++ " | " ++ description e
showSelectable SelectDoubleDetector =
  "DoubleDetector | Personal detector (can combine with Stabilizer)"

allowedRemaining :: [Selectable] -> [Selectable] -> [Selectable]
allowedRemaining remaining selected
  | null selected = remaining
  | any isExclusive selected =
      filter isStabilizer remaining
  | otherwise = remaining

isStabilizer :: Selectable -> Bool
isStabilizer (SelectEquipment e) = name e == Stabilizer
isStabilizer _ = False

isExclusive :: Selectable -> Bool
isExclusive (SelectEquipment e) = name e /= Stabilizer
isExclusive SelectDoubleDetector = True

canSoloCut :: PlayerView -> Bool
canSoloCut pv =
  let ws = filter (\w -> color w /= Grey) (playerWires (pvSelf pv))
      countMatching c v = length (filter (\w -> color w == c && value w == v) ws)
      keys = nub [ (color w, value w) | w <- ws ]
      sc4 = any (\(c, v) -> countMatching c v >= 4) keys
      yellowPair = length (filter ((== Yellow) . color) ws) >= 2
      cutHist = pvCutWires pv
      hasHistory c v = any (\mw -> color (wire mw) == c && value (wire mw) == v) cutHist
      sc2Match = any
        (\(c, v) -> c /= Yellow && c /= Red && countMatching c v >= 2 && hasHistory c v)
        keys
   in sc4 || yellowPair || sc2Match

canDualCut :: PlayerView -> Bool
canDualCut pv =
  let selfHas = any (\w -> color w `elem` [Blue, Yellow]) (playerWires (pvSelf pv))
      otherHas = any (\p -> any (\w -> color w /= Grey) (playerWires p)) (pvOthers pv)
   in selfHas && otherHas

canRemoveRed :: PlayerView -> Bool
canRemoveRed pv =
  let ws = playerWires (pvSelf pv)
   in any ((== Red) . color) ws
      && all (\w -> color w `elem` [Red, Grey]) ws

actionOptions :: PlayerView -> [(String, PlayerView -> IO Action)]
actionOptions pv =
     [ ("Solo Cut",    handleSoloCutInput)   | canSoloCut pv ]
  ++ [ ("Duo Cut",     handleDualCutInput)   | canDualCut pv ]
  ++ [ ("Remove Reds", handleRemoveRedInput) | canRemoveRed pv ]

chooseAction :: PlayerView -> IO Action
chooseAction pv = do
  displayPlayerView pv
  putStrLn $ "Player " ++ show (playerId (pvSelf pv)) ++ " - choose action:"
  let opts = actionOptions pv
      numbered = zip [1 :: Int ..] opts
  if null opts
    then do
      putStrLn "No actions available."
      threadDelay 1000000 >> chooseAction pv
    else do
      mapM_ (\(i, (lbl, _)) -> putStrLn (show i ++ ") " ++ lbl)) numbered
      putStrLn "---------------------------------------------"
      let prompt = "Enter " ++ intercalate ", " (map (show . fst) numbered) ++ ":"
      input <- readIntB prompt
      case lookup input [(i, h) | (i, (_, h)) <- numbered] of
        Just h  -> h pv
        Nothing -> putStrLn "Invalid Input, try again." >> chooseAction pv

chooseActionOrEquipment :: PlayerView -> IO (Either Action [Selectable])
chooseActionOrEquipment pv = do
  displayPlayerView pv
  putStrLn $ "Player " ++ show (playerId (pvSelf pv)) ++ " - choose action:"
  let opts = actionOptions pv
      numbered = zip [1 :: Int ..] opts
      hasEq = not (null (availableEquipment pv))
  if null opts && not hasEq
    then do
      putStrLn "No actions available."
      threadDelay 1000000 >> chooseActionOrEquipment pv
    else do
      mapM_ (\(i, (lbl, _)) -> putStrLn (show i ++ ") " ++ lbl)) numbered
      when hasEq $ putStrLn "e) Choose equipment"
      putStrLn "---------------------------------------------"
      let tokens = map (show . fst) numbered ++ ["e" | hasEq]
      putStrLn $ "Enter " ++ intercalate ", " tokens ++ ":"
      putStr "> "
      line <- getLine
      case line of
        "e" | hasEq -> Right <$> chooseEquipment pv
        _ -> case readMaybe line :: Maybe Int of
          Just i | Just h <- lookup i [(k, h) | (k, (_, h)) <- numbered] ->
            Left <$> h pv
          _ -> do
            putStrLn "Invalid input, try again."
            threadDelay 1000000 >> chooseActionOrEquipment pv

handleSuperDetectorInput :: PlayerView -> IO EquipmentUse
handleSuperDetectorInput pv = do
  otherId <- readIntB "SuperDetector - enter target player id:"
  case maybeFindPlayer otherId (pvOthers pv) of
    Nothing -> do
      putStrLn "Invalid player id."
      handleSuperDetectorInput pv
    Just target -> askWire otherId target
  where
    askWire otherId target = do
      displayWirePickView pv (Just target)
      pos <- readLabelB "Your wire:"
      let (valid, msg) = checkInputSD pv otherId pos
      putStrLn msg
      if valid
        then pure $ UseSuperDetector otherId pos (playerWires (pvSelf pv) !! pos)
        else threadDelay 1000000 >> askWire otherId target

checkInputSD :: PlayerView -> PlayerId -> Int -> (Bool, String)
checkInputSD pv otherId pos =
  let p = pvSelf pv
      pid = playerId p
      ws = playerWires p
      n = length ws
      col = if pos >= 0 && pos < n then color (ws !! pos) else Grey
      validIds = filter (/= pid) (pvTurnOrder pv)
   in validate col otherId validIds pos n
  where
    validate col otherIdCandidate validIds posCandidate nLen
      | otherIdCandidate `notElem` validIds =
          (False, "Invalid Id.")
      | posCandidate < 0 || posCandidate >= nLen =
          (False, "Invalid position: Out of range.")
      | col == Grey =
          (False, "Chosen wire already cut.")
      | col == Red =
          (False, "Chosen wire cannot be red.")
      | col == Yellow =
          (False, "Chosen wire cannot be yellow.")
      | otherwise =
          (True, "Valid Super Detector Input.")

handleTripleDetectorInput :: PlayerView -> IO EquipmentUse
handleTripleDetectorInput pv = do
  otherId <- readIntB "Triple Detector - enter target player id:"
  case maybeFindPlayer otherId (pvOthers pv) of
    Nothing -> do
      putStrLn "Invalid player id."
      handleTripleDetectorInput pv
    Just target
      | uncutOn target < 3 -> do
          putStrLn "Target must have at least 3 uncut wires."
          threadDelay 1000000 >> handleTripleDetectorInput pv
      | otherwise -> askWires otherId target
  where
    askWires otherId target = do
      displayWirePickView pv (Just target)
      selfPos <- readLabelB "Your wire:"
      a <- readLabelB "First target wire:"
      b <- readLabelB "Second target wire:"
      c <- readLabelB "Third target wire:"
      let (valid, msg) = checkInputTD pv otherId selfPos (a, b, c)
      putStrLn msg
      if valid
        then pure $ UseTripleDetector otherId selfPos (a, b, c) (playerWires (pvSelf pv) !! selfPos)
        else threadDelay 1000000 >> askWires otherId target

checkInputTD :: PlayerView -> PlayerId -> Int -> (Int, Int, Int) -> (Bool, String)
checkInputTD pv otherId selfPos (a, b, c) =
  let self = pvSelf pv
      pid = playerId self
      validIds = filter (/= pid) (pvTurnOrder pv)
   in case maybeFindPlayer otherId (pvOthers pv) of
        Nothing -> (False, "Invalid Id.")
        Just otherP ->
          let selfWires = playerWires self
              otherWires = playerWires otherP
              nSelf = length selfWires
              nOther = length otherWires
              selfCol =
                if selfPos >= 0 && selfPos < nSelf
                  then color (selfWires !! selfPos)
                  else Grey
              otherCols i =
                if i >= 0 && i < nOther
                  then color (otherWires !! i)
                  else Grey
           in validate validIds selfPos selfCol (a, b, c) otherCols nSelf nOther
  where
    validate validIds selfPosCandidate selfCol (aCandidate, bCandidate, cCandidate) otherCols nSelf nOther
      | otherId `notElem` validIds =
          (False, "Invalid Id.")
      | selfPosCandidate < 0 || selfPosCandidate >= nSelf =
          (False, "Invalid position: own wire out of range.")
      | selfCol == Grey =
          (False, "Chosen own wire already cut.")
      | selfCol /= Blue =
          (False, "Chosen own wire must be blue.")
      | any (< 0) [aCandidate, bCandidate, cCandidate] || any (>= nOther) [aCandidate, bCandidate, cCandidate] =
          (False, "One or more target wire positions out of range.")
      | Grey `elem` map otherCols [aCandidate, bCandidate, cCandidate] =
          (False, "One or more chosen wires already cut.")
      | length (nub [aCandidate, bCandidate, cCandidate]) < 3 =
          (False, "Chosen wire positions must be distinct.")
      | otherwise =
          (True, "Valid Triple Detector Input.")

handleDoubleRadiatorInput :: PlayerView -> IO EquipmentUse
handleDoubleRadiatorInput pv = do
  otherId <- readIntB "Double Radiator - enter target player id:"
  case maybeFindPlayer otherId (pvOthers pv) of
    Nothing -> do
      putStrLn "Invalid player id."
      handleDoubleRadiatorInput pv
    Just target -> askWires otherId target
  where
    askWires otherId target = do
      displayWirePickView pv (Just target)
      pos1 <- readLabelB "Your first wire:"
      pos2 <- readLabelB "Your second wire:"
      otherPos <- readLabelB "Target wire:"
      let (valid, msg) = checkInputDR pv otherId (pos1, pos2) otherPos
      putStrLn msg
      let ws = playerWires (pvSelf pv)
      if valid
        then pure $ UseDoubleRadiator otherId (pos1, pos2) otherPos (ws !! pos1, ws !! pos2)
        else threadDelay 1000000 >> askWires otherId target

checkInputDR :: PlayerView -> PlayerId -> (Int, Int) -> Int -> (Bool, String)
checkInputDR pv otherId (p1, p2) otherPos =
  let self = pvSelf pv
      pid = playerId self
      validIds = filter (/= pid) (pvTurnOrder pv)
   in case maybeFindPlayer otherId (pvOthers pv) of
        Nothing -> (False, "Invalid Id.")
        Just otherP ->
          let selfWires = playerWires self
              otherWires = playerWires otherP
              nSelf = length selfWires
              nOther = length otherWires
              selfCol i =
                if i >= 0 && i < nSelf
                  then color (selfWires !! i)
                  else Grey
              otherCol i =
                if i >= 0 && i < nOther
                  then color (otherWires !! i)
                  else Grey
           in validate validIds (p1, p2) otherPos selfCol otherCol nSelf nOther
  where
    validate validIds (p1Candidate, p2Candidate) otherPosCandidate selfCol otherCol nSelf nOther
      | otherId `notElem` validIds =
          (False, "Invalid Id.")
      | any (< 0) [p1Candidate, p2Candidate] || any (>= nSelf) [p1Candidate, p2Candidate] =
          (False, "Own wire position out of range.")
      | p1Candidate == p2Candidate =
          (False, "The two call-out wires must be distinct.")
      | any (\p -> selfCol p /= Blue && selfCol p /= Yellow) [p1Candidate, p2Candidate] =
          (False, "Both own wires must be blue or yellow and uncut.")
      | otherPosCandidate < 0 || otherPosCandidate >= nOther =
          (False, "Other wire position out of range.")
      | otherCol otherPosCandidate == Grey =
          (False, "Chosen other wire already cut.")
      | otherwise =
          (True, "Valid Double Radiator Input.")

handleDoubleDetectorInput :: PlayerView -> IO EquipmentUse
handleDoubleDetectorInput pv = do
  otherId <- readIntB "Double Detector - enter target player id:"
  case maybeFindPlayer otherId (pvOthers pv) of
    Nothing -> do
      putStrLn "Invalid player id."
      handleDoubleDetectorInput pv
    Just target
      | uncutOn target < 2 -> do
          putStrLn "Target must have at least 2 uncut wires."
          threadDelay 1000000 >> handleDoubleDetectorInput pv
      | otherwise -> askWires otherId target
  where
    askWires otherId target = do
      displayWirePickView pv (Just target)
      selfPos <- readLabelB "Your wire:"
      a <- readLabelB "First target wire:"
      b <- readLabelB "Second target wire:"
      let (valid, msg) = checkInputDD pv otherId selfPos (a, b)
      putStrLn msg
      if valid
        then pure $ UseDoubleDetector otherId selfPos (a, b) (playerWires (pvSelf pv) !! selfPos)
        else threadDelay 1000000 >> askWires otherId target

checkInputDD :: PlayerView -> PlayerId -> Int -> (Int, Int) -> (Bool, String)
checkInputDD pv otherId selfPos (a, b) =
  let self = pvSelf pv
      pid = playerId self
      validIds = filter (/= pid) (pvTurnOrder pv)
   in case maybeFindPlayer otherId (pvOthers pv) of
        Nothing -> (False, "Invalid Id.")
        Just otherP ->
          let selfWires = playerWires self
              otherWires = playerWires otherP
              nSelf = length selfWires
              nOther = length otherWires
              selfCol =
                if selfPos >= 0 && selfPos < nSelf
                  then color (selfWires !! selfPos)
                  else Grey
              otherCol i =
                if i >= 0 && i < nOther
                  then color (otherWires !! i)
                  else Grey
           in validate validIds selfPos selfCol (a, b) otherCol nSelf nOther
  where
    validate validIds selfPosCandidate selfCol (aCandidate, bCandidate) otherCol nSelf nOther
      | otherId `notElem` validIds =
          (False, "Invalid Id.")
      | selfPosCandidate < 0 || selfPosCandidate >= nSelf =
          (False, "Invalid position: own wire out of range.")
      | selfCol == Grey =
          (False, "Chosen own wire already cut.")
      | selfCol /= Blue =
          (False, "Chosen own wire must be blue.")
      | any (< 0) [aCandidate, bCandidate] || any (>= nOther) [aCandidate, bCandidate] =
          (False, "One or more target wire positions out of range.")
      | Grey `elem` map otherCol [aCandidate, bCandidate] =
          (False, "One or more chosen wires already cut.")
      | aCandidate == bCandidate =
          (False, "Chosen wire positions must be distinct.")
      | otherwise =
          (True, "Valid Double Detector Input.")

handleSoloCutInput :: PlayerView -> IO Action
handleSoloCutInput pv = do
  displayWirePickView pv Nothing
  count <- readIntB "Solo Cut - cut two (2) or four (4) wires? Enter 2 or 4:"
  case count of
    2 -> do
      a <- readLabelB "First wire:"
      b <- readLabelB "Second wire:"
      let (valid, msg) = checkInputSC2 pv (a, b)
      putStrLn msg
      if valid
        then return $ SoloCut2 (a, b)
        else chooseAction pv
    4 -> do
      a <- readLabelB "First wire:"
      b <- readLabelB "Second wire:"
      c <- readLabelB "Third wire:"
      d <- readLabelB "Fourth wire:"
      let (valid, msg) = checkInputSC4 pv (a, b, c, d)
      putStrLn msg
      if valid
        then return $ SoloCut4 (a, b, c, d)
        else threadDelay 1000000 >> chooseAction pv
    _ -> do
      putStrLn "Invalid choice. Please enter 2 or 4."
      chooseAction pv

handlePlaceInfoMarker :: PlayerView -> IO Action
handlePlaceInfoMarker pv = do
  displayInfoMarkerView pv
  let pid = playerId (pvSelf pv)
  pos <- readLabel $ "Player " ++ show pid ++ " - pick a wire for your info marker:"
  let (valid, msg) = checkInputInitInfo pv pos
  putStrLn msg
  if valid
    then return $ PlaceInfoMarker pos
    else threadDelay 1000000 >> handlePlaceInfoMarker pv

handleDualCutInput :: PlayerView -> IO Action
handleDualCutInput pv = do
  otherId <- readIntB "Duo Cut - target player id:"
  case maybeFindPlayer otherId (pvOthers pv) of
    Nothing -> do
      putStrLn "Invalid player id, try again."
      handleDualCutInput pv
    Just target -> askWires otherId target
  where
    askWires otherId target = do
      displayWirePickView pv (Just target)
      a <- readLabelB "Your wire:"
      b <- readLabelB "Target wire:"
      let (valid, msg) = checkInputDC pv otherId (a, b)
      putStrLn msg
      if valid
        then return $ DualCut otherId (a, b) (playerWires (pvSelf pv) !! a)
        else threadDelay 1000000 >> askWires otherId target

handleRemoveRedInput :: PlayerView -> IO Action
handleRemoveRedInput pv = do
  displayWirePickView pv Nothing
  pos <- readLabelB "Remove Red - enter the red wire:"
  let (valid, msg) = checkInputRR pv pos
  putStrLn msg
  if valid
    then return $ RemoveRed pos
    else threadDelay 1000000 >> chooseAction pv

checkInputInitInfo :: PlayerView -> Int -> (Bool, String)
checkInputInitInfo pv pos
  | pos < 0 || pos >= n = (False, "Invalid position: out of range.")
  | col == Grey = (False, "Wire already cut.")
  | col == Yellow = (False, "Cannot place initial info marker for yellow wire.")
  | col == Red = (False, "Cannot place initial info marker for red wire.")
  | otherwise = (True, "Placing wire at position " ++ show pos ++ ".")
  where
    playerWiresSelf = playerWires (pvSelf pv)
    n = length playerWiresSelf
    col = color $ playerWiresSelf !! pos

checkInputSC2 :: PlayerView -> CutPosition -> (Bool, String)
checkInputSC2 pv (a, b)
  | a < 0 || b < 0 || a >= n || b >= n =
      (False, "Invalid position: out of range.")
  | a == b =
      (False, "Chosen wire positions must be distinct.")
  | colA == Grey || colB == Grey =
      (False, "One or both wires already cut.")
  | colA == Red || colB == Red =
      (False, "Cannot cut red wires directly.")
  | oneYellow && not bothYellow =
      (False, "Yellow can only be cut with another yellow wire.")
  | bothYellow =
      (True, "Valid yellow cut.")
  | (valA /= valB) || (colA /= colB) =
      (False, "Wires don't match in value or color.")
  | not validCutHistory =
      (False, "No wires with value " ++ show valA ++ " have been cut yet.")
  | otherwise =
      (True, "Valid cut.")
  where
    playerWiresSelf = playerWires (pvSelf pv)
    n = length playerWiresSelf

    wireA = playerWiresSelf !! a
    wireB = playerWiresSelf !! b

    (valA, colA) = (value wireA, color wireA)
    (valB, colB) = (value wireB, color wireB)

    bothYellow = colA == Yellow && colB == Yellow
    oneYellow = colA == Yellow || colB == Yellow

    cutWiresSelf = pvCutWires pv
    validCutHistory = any (\mw -> value (wire mw) == valA && color (wire mw) == colA) cutWiresSelf

checkInputSC4 :: PlayerView -> (Int, Int, Int, Int) -> (Bool, String)
checkInputSC4 pv (a, b, c, d)
  | any (< 0) indices || any (>= length playerWiresSelf) indices =
      (False, "Invalid indices.")
  | length (nub indices) < 4 =
      (False, "Chosen wire positions must be distinct.")
  | any ((== Grey) . color) chosenWires =
      (False, "Cannot cut already cut wire.")
  | not (allSame wireColors) =
      (False, "Invalid color match.")
  | not (allSame wireValues) =
      (False, "Wire values don't match.")
  | otherwise =
      (True, "Valid Solo Cut.")
  where
    playerWiresSelf = playerWires (pvSelf pv)
    indices = [a, b, c, d]
    chosenWires = map (playerWiresSelf !!) indices
    wireColors = map color chosenWires
    wireValues = map value chosenWires

    allSame (x : xs) = all (== x) xs
    allSame [] = True

checkInputDC :: PlayerView -> PlayerId -> CutPosition -> (Bool, String)
checkInputDC pv otherId (a, b) =
  let playerWiresSelf = playerWires (pvSelf pv)
      selfId = playerId (pvSelf pv)
      cutWiresView = pvCutWires pv
      validIds = filter (/= selfId) (pvTurnOrder pv)
      n = length playerWiresSelf
      colA = if a >= 0 && a < n then color (playerWiresSelf !! a) else Grey
      targetCut = any (\mw -> player mw == otherId && originalIndex mw == b) cutWiresView
   in case maybeFindPlayer otherId (pvOthers pv) of
        Nothing -> (False, "Invalid player id.")
        Just otherPlayer ->
          let nOther = length (playerWires otherPlayer)
           in validate colA targetCut validIds n nOther
  where
    validate colA targetCut validIds n nOther
      | otherId `notElem` validIds =
          (False, "Invalid player id.")
      | a < 0 || a >= n =
          (False, "Invalid position: out of range.")
      | b < 0 || b >= nOther =
          (False, "Invalid position: out of range.")
      | colA == Grey =
          (False, "You cannot cut an already cut wire.")
      | colA == Red =
          (False, "You cannot use a red wire for a dual cut.")
      | targetCut =
          (False, "The target wire is already cut.")
      | colA `elem` [Blue, Yellow] =
          (True, "Valid dual cut.")
      | otherwise =
          (False, "Invalid dual cut.")

checkInputRR :: PlayerView -> Int -> (Bool, String)
checkInputRR pv a
  | a < 0 || a >= n =
      (False, "Invalid position: out of range.")
  | colA /= Red =
      (False, "You can only remove a red wire.")
  | not (all (\w -> color w `elem` [Red, Grey]) playerWiresSelf) =
      (False, "Remove Red only works if *all* remaining wires are red.")
  | otherwise =
      (True, "Remove Red successful.")
  where
    playerWiresSelf = playerWires (pvSelf pv)
    n = length playerWiresSelf
    colA = color (playerWiresSelf !! a)

handleChooseBots :: [PlayerId] -> IO [PlayerId]
handleChooseBots pids = do
  amount <- readInt "Choose amount of bots for the round (0,1,2,3,4)."
  let (valid, msg) = checkInputCB amount
  putStrLn msg
  if valid
    then return $ shortenList amount pids
    else threadDelay 1000000 >> handleChooseBots pids

checkInputCB :: Int -> (Bool, String)
checkInputCB input
  | input < 0 || input > 4 =
      (False, "Out of range.")
  | otherwise =
      (True, "Valid input.")
