module GameLogic.Core where

import Control.Monad.State
import Config (bbDelay)
import GameLogic.Actions
import GameLogic.Display (displayPlayerView, displayBotDecision)
import GameLogic.Equipment
import GameLogic.Input
import GameLogic.Util
import BotLogic.Interface
import Types

playerTurn :: PlayerView -> GameM Outcome
playerTurn pv = do
  let pubOf act eq = (fmap toPublicAction act, map toPublicEquipmentUse eq)
  (act, equip) <- liftIO $ if isBot (pvSelf pv)
    then do
      displayPlayerView pv
      bbDelay 2000000
      putStrLn $ "Bot " ++ show (playerId (pvSelf pv)) ++ " is thinking..."
      result@(a, eq) <- botAction pv
      let (pubA, pubEq) = pubOf a eq
      displayBotDecision pubA pubEq
      return result
    else humanAction pv
  let f = handleStabilizer equip
  eqOutcome <- handleEquipment f pv equip
  actOutcome <- case act of
    Just a -> handleAction pv a f
    Nothing -> pure ActionDone
  let outcome = mergeOutcome eqOutcome actOutcome
  modify $ markDoubleDetectorUsed (playerId (pvSelf pv)) equip . markEquipmentUsed equip
  gs <- get
  let (pubAct, pubEquip) = pubOf act equip
      tlog = TurnLog
        { logRound = roundCount gs,
          logPlayer = playerId (pvSelf pv),
          logAction = pubAct,
          logEquipment = pubEquip,
          logOutcome = outcome
        }
  modify (appendTurnLog tlog)
  pure outcome

-- pending choice is envoked whenever a player whose turn it is not has to choose a wire;
-- either for info marker placement or for cutting some wire (with multiple options)
handlePendingChoice :: PendingChoice -> GameM ()
handlePendingChoice (ChooseWire choosingPlayer opts resolve kind) = do
  gs <- get
  let pv = getPlayerView choosingPlayer gs
  choice <- if isBot (pvSelf pv)
    then pure $ botPendingChoice pv kind opts
    else liftIO $ askPlayerToChooseWire pv opts kind
  modify $ \s -> resolve choice s
