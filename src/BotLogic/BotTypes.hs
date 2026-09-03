module BotLogic.BotTypes where

import Types
import Data.Map (Map)
import Data.SBV

data SlotContext = SlotContext
    { scRemainingBlue :: Map (Int, WireColor) Int
    , scYellowPool :: [Int]
    , scRevealedYellow :: [Int]
    , scUnseenYellow :: Int
    , scRedPool :: [Int]
    , scSeenRed :: Bool
    }

data SlotKnowledge
  = KnownValue Wire
  | KnownCut Wire
  | PossibleValue [Wire]
  deriving (Show, Eq)

data HandConstraint
  = HasBlue PlayerId Int            
  | HasYellow PlayerId              
  | SlotsNotBlue PlayerId [Int] Int 
  | HandNotBlue PlayerId Int        
  | SlotIsRed PlayerId Int          
  deriving (Eq, Ord, Show)

data BotKnowledge = BotKnowledge
  { remainingCounts :: Map (Int, WireColor) Int
  , redPool :: [Int]
  , yellowPool :: [Int]
  , playerSlots :: Map PlayerId (Map Int SlotKnowledge)  
  , revealedYellow :: [Int]
  , currentDetonator :: Int
  , availableEquipment :: [Equipment]
  , hasDoubleDetector :: Bool
  , cutBlueCounts :: Map Int Int  
  , selfMarkedSlots :: [Int]     
  , handConstraints :: [HandConstraint]  
  , seenRed :: Bool               
  , lockedUnlockValues :: [Int]  
  } deriving (Show, Eq)

type BotMove = (Maybe Action, [EquipmentUse], Double)

type WeightedWorld = (WorldAssignment, Double)

type WorldAssignment = Map PlayerId (Map Int (Int, WireColor))

type SlotSym = (SInteger, SInteger)
type SlotId = (PlayerId, Int)

data ReplayState = ReplayState
    { rsMarkers :: [InfoMarker]
    , rsCuts :: [MarkedWire]
    }
