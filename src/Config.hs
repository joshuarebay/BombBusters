-- Runtime configuration knobs exposed via environment variables.
-- Set BB_FAST=1 to silence threadDelay/clearScreen for benchmarking.
-- Set BB_MAX_MODELS / BB_MAX_DET_PROB to tune the bot's SMT cap and risk threshold.
-- Set BB_STAB_THRESHOLD to set the success-probability cutoff below which a
-- chosen cut is considered risky enough to deserve a Stabilizer.
-- Set BB_EPISTEMIC_CONFIDENCE to set the prior that an inferred "missed
-- obvious move" reflects reality (0 < c < 1; default 0.7).
-- Set BB_SEED to an integer to seed the global StdGen deterministically,
-- so a paired run under the same seed produces the same deck / turn
-- order / colour markers. Unset -> newStdGen fresh randomness.
-- Defaults are the best-per-parameter values from the sensitivity sweep
-- (benchmarks/benchmark_figures/run-2026-07-04T16-56-41).
module Config
    ( benchMode
    , bbDelay
    , bbClearScreen
    , maxModels
    , maxDetonateProb
    , stabilizerThreshold
    , epistemicConfidence
    , bbSeed
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified System.Console.ANSI as ANSI

{-# NOINLINE benchMode #-}
benchMode :: Bool
benchMode = unsafePerformIO (isJust <$> lookupEnv "BB_FAST")

bbDelay :: Int -> IO ()
bbDelay n = unless benchMode (threadDelay n)

bbClearScreen :: IO ()
bbClearScreen = unless benchMode ANSI.clearScreen

{-# NOINLINE maxModels #-}
maxModels :: Int
maxModels = unsafePerformIO $ do
    v <- lookupEnv "BB_MAX_MODELS"
    pure (maybe 2048 read v)

{-# NOINLINE maxDetonateProb #-}
maxDetonateProb :: Double
maxDetonateProb = unsafePerformIO $ do
    v <- lookupEnv "BB_MAX_DET_PROB"
    pure (maybe 0.02 read v)

{-# NOINLINE stabilizerThreshold #-}
stabilizerThreshold :: Double
stabilizerThreshold = unsafePerformIO $ do
    v <- lookupEnv "BB_STAB_THRESHOLD"
    pure (maybe 0.6 read v)

{-# NOINLINE epistemicConfidence #-}
epistemicConfidence :: Double
epistemicConfidence = unsafePerformIO $ do
    v <- lookupEnv "BB_EPISTEMIC_CONFIDENCE"
    pure (maybe 0.7 read v)

-- Reads BB_SEED as Int; returns Nothing when unset or unparseable so the
-- caller falls back to non-deterministic newStdGen. Any Int fits (mkStdGen
-- accepts negative values).
{-# NOINLINE bbSeed #-}
bbSeed :: Maybe Int
bbSeed = unsafePerformIO $ do
    v <- lookupEnv "BB_SEED"
    pure (v >>= readMaybe)
