module GameLoop where

import Control.Monad.State
import Data.Time.Clock (getCurrentTime)
import BenchmarkLog (writeGameLog)
import GameLogic.Actions (handleAction)
import GameLogic.Core
import GameLogic.Display (displayPlayerView, displayTurnResult, displayBotDecision)
import GameLogic.Input (handlePlaceInfoMarker, handleChooseBots)
import GameLogic.Util
import BotLogic.Interface
import Types

runGame :: GameState -> IO ()
runGame gs = do
    startT   <- getCurrentTime
    finalGs  <- execStateT startGame gs
    endT     <- getCurrentTime
    writeGameLog startT endT finalGs

startGame :: GameM ()
startGame = do
  gs <- get
  botIds <- liftIO $ handleChooseBots (turnOrder gs)
  modify $ selectBots botIds
  gameLoop

gameLoop :: GameM ()
gameLoop = do
  modify evaluateGameEnd
  gs <- get
  let currStatus = status gs
  case currStatus of
    Detonated -> liftIO $ putStrLn "Bomb detonated, game lost."
    Defused -> liftIO $ putStrLn "Bomb defused, game won."
    WaitingForChoice pc -> handlePendingChoice pc >> gameLoop
    InProgress -> do
      modify checkActivePlayers
      modify unlockEquipment
      gs' <- get
      let pid = currentPlayer gs'
          pv = getPlayerView pid gs'
          isFirstRound = roundCount gs' < length (players gs')
          
      if isFirstRound
        then do
          act <- if isBot (pvSelf pv)
            then do
              liftIO $ displayPlayerView pv
              let a = botPlaceMarker pv
              liftIO $ displayBotDecision (Just (toPublicAction a)) []
              pure a
            else liftIO $ handlePlaceInfoMarker pv
          _ <- handleAction pv act id
          pure ()
        else do
          outcome <- playerTurn pv
          gs'' <- get
          case status gs'' of
            Detonated -> pure ()
            _ -> liftIO $ displayTurnResult outcome

      modify advanceTurn
      gameLoop
