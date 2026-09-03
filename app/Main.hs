module Main where

import GameLoop
import InitGame

main :: IO ()
main = do
  gameState <- initGameState
  runGame gameState
