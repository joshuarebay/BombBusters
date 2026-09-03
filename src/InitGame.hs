module InitGame (initGameState) where

import Data.List (sortBy)
import Data.Ord (comparing)
import System.Random
import System.Random.Shuffle (shuffle')
import qualified Config
import Types

initWires :: IO ([Wire], Wire, [Wire])
initWires = do
  genVals  <- newStdGen
  genWires <- newStdGen
  let blueVals    = [1 .. 12]
      nonBlueVals = [1 .. 11]  -- yellow and red can only be 1..11 per game rules
      shuffledVals = shuffle' nonBlueVals (length nonBlueVals) genVals

      (redVal, yellowVals) = case shuffledVals of
        (r : y1 : y2 : _) -> (r, [y1, y2])
        _ -> error "initWires: shuffle' returned fewer than 3 values"

      blueWires = [Wire val Blue 0 | val <- concatMap (replicate 4) blueVals]
      yellowWires = [Wire val Yellow 0 | val <- yellowVals]
      redWire = Wire redVal Red 0
      allWires = blueWires ++ yellowWires ++ [redWire]

      shuffledWires = shuffle' allWires (length allWires) genWires

  return (shuffledWires, redWire, yellowWires)

initPlayers :: [Wire] -> [Player]
initPlayers allWires =
  let total = length allWires
      baseCount = total `div` 4
      remainder = total `mod` 4

      distribute pid ws
        | null ws = []
        | otherwise =
            let count = baseCount + if pid <= remainder then 1 else 0
                (pWiresRaw, rest) = splitAt count ws
                sortedPWires = sortBy (\w -> comparing value w <> comparing color w) pWiresRaw
                pWires = zipWith (\i w -> w {wireIndex = i}) [0 ..] sortedPWires
                p =
                  Player
                    { playerId = pid,
                      playerName = "Player " ++ show pid,
                      doubleDetector = True,
                      playerWires = pWires,
                      isBot = False
                    }
             in p : distribute (pid + 1) rest
   in distribute 1 allWires

initColorMarkers :: Wire -> [Wire] -> IO [Wire]
initColorMarkers redWire yellowWires = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  let nonBlueVals = [1 .. 11]  -- yellow and red decoy markers restricted to legal red/yellow range

      yellowVals = map value yellowWires
      redVal = value redWire

      shuffled1 = shuffle' nonBlueVals (length nonBlueVals) gen1
      randomRedVal = case filter (/= redVal) shuffled1 of
        (v : _) -> v
        [] -> error "initColorMarkers: no candidate red value"

      shuffled2 = shuffle' nonBlueVals (length nonBlueVals) gen2
      randomYellowVal = case filter (`notElem` yellowVals) shuffled2 of
        (v : _) -> v
        [] -> error "initColorMarkers: no candidate yellow value"

      randomRedWire = Wire randomRedVal Red 0
      randomYellowWire = Wire randomYellowVal Yellow 0

      markers = [redWire, randomRedWire] ++ yellowWires ++ [randomYellowWire]
  return $ sortBy (\m -> comparing value m <> comparing color m) markers

initTurnOrder :: IO ([PlayerId], PlayerId)
initTurnOrder = do
  gen <- newStdGen
  let allPlayers = [1 .. 4]
      tO = shuffle' allPlayers 4 gen
      startingPlayer = case tO of
        (p : _) -> p
        [] -> error "initTurnOrder: turn order unexpectedly empty"
  return (tO, startingPlayer)

initEquipment :: [Equipment]
initEquipment =
  let superDetector =
        Equipment
          { name = SuperDetector,
            unlocked = False,
            used = False,
            unlockingVal = 5,
            description = "Name one value of your own and point to another players whole rack."
          }

      stabilizer =
        Equipment
          { name = Stabilizer,
            unlocked = False,
            used = False,
            unlockingVal = 9,
            description = "No matter the outcome of your turn, the bomb will not explode. Pair this with every other move."
          }

      tripleDetector =
        Equipment
          { name = TripleDetector,
            unlocked = False,
            used = False,
            unlockingVal = 3,
            description = "Name one value of your own and point to three wires of another player."
          }

      doubleRadiator =
        Equipment
          { name = DoubleRadiator,
            unlocked = False,
            used = False,
            unlockingVal = 10,
            description = "Name two values of your own and point to one wire of another player."
          }
   in [superDetector, stabilizer, tripleDetector, doubleRadiator]

initGameState :: IO GameState
initGameState = do
  -- CRN support: when BB_SEED is set, seed the global StdGen so every
  -- subsequent newStdGen call (inside initWires / initColorMarkers /
  -- initTurnOrder) is derived deterministically. Unset -> stock randomness.
  case Config.bbSeed of
    Just s  -> setStdGen (mkStdGen s)
    Nothing -> pure ()
  (allWiresRaw, redWire, yellowWires) <- initWires
  let ps = initPlayers allWiresRaw
      allWires = concatMap playerWires ps
      eq = initEquipment
  cMs <- initColorMarkers redWire yellowWires
  (tO, startingPlayer) <- initTurnOrder
  return
    GameState
      { wires = allWires,
        players = ps,
        colorMarkers = cMs,
        detonator = 4,
        status = InProgress,
        turnOrder = tO,
        currentPlayer = startingPlayer,
        equipment = eq,
        cutWires = [],
        infoMarkers = [],
        roundCount = 0,
        turnHistory = [],
        unlockingValues = [(unlockingVal e, name e) | e <- eq]
      }
