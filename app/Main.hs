module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import GameLoop
import InitGame

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  gameState <- initGameState
  runGame gameState
