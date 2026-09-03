module Types where

import Control.Monad.State

data WireColor = Blue | Yellow | Red | Grey | Hidden
  deriving (Show, Eq, Ord)

data Wire = Wire
  { value :: Int,
    color :: WireColor,
    wireIndex :: Int
  }
  deriving (Eq, Show)

data EquipmentUse
  = UseStabilizer
  | UseSuperDetector PlayerId Int Wire
  | UseTripleDetector PlayerId Int (Int, Int, Int) Wire
  | UseDoubleRadiator PlayerId (Int, Int) Int (Wire, Wire)
  | UseDoubleDetector PlayerId Int (Int, Int) Wire
  deriving (Eq, Show)

data EquipmentName
  = SuperDetector
  | Stabilizer
  | TripleDetector
  | DoubleRadiator
  deriving (Eq, Show)

data Equipment = Equipment
  { name :: EquipmentName,
    unlocked :: Bool,
    used :: Bool,
    unlockingVal :: Int,
    description :: String
  }
  deriving (Eq, Show)

type PlayerId = Int

type CutPosition = (Int, Int)

data MarkedWire = MarkedWire
  { player :: PlayerId,
    originalIndex :: Int,
    wire :: Wire
  }
  deriving (Eq, Show)

data InfoMarker = InfoMarker
  { markerOwner :: PlayerId,
    markerIndex :: Int,
    markerValue :: Int,
    markerColor :: WireColor
  }
  deriving (Eq, Show)

data Action
  = DualCut PlayerId CutPosition Wire
  | SoloCut2 CutPosition
  | SoloCut4 (Int, Int, Int, Int)
  | RemoveRed Int
  | PlaceInfoMarker Int
  deriving (Eq, Show)

data PublicAction
  = PubDualCut PlayerId Int Wire           
  | PubSoloCut2 (Int, Int)                
  | PubSoloCut4 (Int, Int, Int, Int)
  | PubRemoveRed Int
  | PubPlaceInfoMarker Int               
  deriving (Eq, Show)

data PublicEquipmentUse
  = PubUseStabilizer
  | PubUseSuperDetector PlayerId Wire                 
  | PubUseTripleDetector PlayerId (Int, Int, Int) Wire
  | PubUseDoubleDetector PlayerId (Int, Int) Wire    
  | PubUseDoubleRadiator PlayerId Int (Wire, Wire)  
  deriving (Eq, Show)

data Player = Player
  { playerId :: PlayerId,
    playerName :: String,
    doubleDetector :: Bool,
    playerWires :: [Wire],
    isBot :: Bool
  }
  deriving (Eq, Show)

data ChoiceKind
  = PickWireToCut       
  | PickInfoMarkerSlot 
  deriving (Eq, Show)

data PendingChoice = ChooseWire
  { chooser :: PlayerId,
    options :: [Int],
    resolver :: Int -> GameState -> GameState,
    choiceKind :: ChoiceKind
  }

data GameStatus
  = InProgress
  | WaitingForChoice PendingChoice
  | Detonated
  | Defused

data GameState = GameState
  { wires :: [Wire],
    players :: [Player],
    colorMarkers :: [Wire],
    detonator :: Int,
    status :: GameStatus,
    turnOrder :: [PlayerId],
    currentPlayer :: PlayerId,
    equipment :: [Equipment],
    cutWires :: [MarkedWire],
    infoMarkers :: [InfoMarker],
    roundCount :: Int,
    turnHistory :: [TurnLog],
    unlockingValues :: [(Int, EquipmentName)]
  }

data PlayerView = PlayerView
  { pvSelf :: Player,
    pvOthers :: [Player],
    pvColorMarkers :: [Wire],
    pvDetonator :: Int,
    pvEquipment :: [Equipment],
    pvCutWires :: [MarkedWire],
    pvInfoMarkers :: [InfoMarker],
    pvRoundCount :: Int,
    pvTurnOrder :: [PlayerId],
    pvTurnHistory :: [TurnLog],
    pvUnlockingValues :: [(Int, EquipmentName)]
  }
  deriving (Eq, Show)

type GameM = StateT GameState IO

type Modifier = (GameState -> GameState) -> (GameState -> GameState)

type MatchStrategy = PlayerView -> PlayerId -> Int -> GameState -> [Int]

data Selectable
  = SelectEquipment Equipment
  | SelectDoubleDetector
  deriving (Eq, Show)

data Outcome
  = CutSuccess
  | CutMismatch
  | ActionDone
  deriving (Eq, Show)

data TurnLog = TurnLog
  { logRound :: Int,
    logPlayer :: PlayerId,
    logAction :: Maybe PublicAction,
    logEquipment :: [PublicEquipmentUse],
    logOutcome :: Outcome
  }
  deriving (Eq, Show)
