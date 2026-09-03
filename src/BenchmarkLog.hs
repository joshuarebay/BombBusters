{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Appends one JSON line per completed game to BB_LOG_FILE (if set).
-- Called from GameLoop.runGame. Silent no-op when BB_LOG_FILE is unset.
module BenchmarkLog (writeGameLog) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (nub)
import Data.Time.Clock (UTCTime, diffUTCTime, nominalDiffTimeToSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (IOMode (AppendMode), hFlush, hPutStrLn, withFile)

import qualified Config
import Types

writeGameLog :: UTCTime -> UTCTime -> GameState -> IO ()
writeGameLog startT endT gs = do
    mPath <- lookupEnv "BB_LOG_FILE"
    case mPath of
        Nothing -> pure ()
        Just path -> do
            mRunId <- lookupEnv "BB_RUN_ID"
            createDirectoryIfMissing True (takeDirectory path)
            let record = gameRecord mRunId startT endT gs
            withFile path AppendMode $ \h -> do
                BL.hPutStr h (A.encode record)
                hPutStrLn h ""
                hFlush h

gameRecord :: Maybe String -> UTCTime -> UTCTime -> GameState -> A.Value
gameRecord mRunId startT endT gs =
    A.object
        [ "run_id"           .= mRunId
        , "config"           .= configObj
        , "status"           .= statusLabel (status gs)
        , "rounds"           .= roundCount gs
        , "detonator_final"  .= detonator gs
        , "wires_cut"        .= length (cutWires gs)
        , "n_players"        .= length (nub (map logPlayer (turnHistory gs)))
        , "start_time"       .= iso8601Show startT
        , "end_time"         .= iso8601Show endT
        , "duration_ms"      .= durationMs
        , "equipment_unlocked" .= [ name e | e <- equipment gs, unlocked e ]
        , "equipment_used"     .= [ name e | e <- equipment gs, used e ]
        , "turns"            .= turnHistory gs
        ]
  where
    durationMs :: Int
    durationMs = round (1000 * (realToFrac (nominalDiffTimeToSeconds (diffUTCTime endT startT)) :: Double))

    configObj = A.object
        [ "maxDetonateProb"     .= Config.maxDetonateProb
        , "epistemicConfidence" .= Config.epistemicConfidence
        , "stabilizerThreshold" .= Config.stabilizerThreshold
        , "maxModels"           .= Config.maxModels
        , "benchMode"           .= Config.benchMode
        ]

statusLabel :: GameStatus -> String
statusLabel Detonated          = "Detonated"
statusLabel Defused            = "Defused"
statusLabel InProgress         = "InProgress"
statusLabel (WaitingForChoice _) = "WaitingForChoice"

-- JSON instances for the domain types referenced above.

instance A.ToJSON WireColor where
    toJSON Blue   = A.String "Blue"
    toJSON Yellow = A.String "Yellow"
    toJSON Red    = A.String "Red"
    toJSON Grey   = A.String "Grey"
    toJSON Hidden = A.String "Hidden"

instance A.ToJSON Wire where
    toJSON w = A.object
        [ "value"     .= value w
        , "color"     .= color w
        , "wireIndex" .= wireIndex w
        ]

instance A.ToJSON EquipmentName where
    toJSON SuperDetector  = A.String "SuperDetector"
    toJSON Stabilizer     = A.String "Stabilizer"
    toJSON TripleDetector = A.String "TripleDetector"
    toJSON DoubleRadiator = A.String "DoubleRadiator"

instance A.ToJSON Outcome where
    toJSON CutSuccess  = A.String "CutSuccess"
    toJSON CutMismatch = A.String "CutMismatch"
    toJSON ActionDone  = A.String "ActionDone"

instance A.ToJSON PublicAction where
    toJSON (PubDualCut pid tIdx w) = A.object
        [ "tag" .= ("DualCut" :: String)
        , "target" .= pid, "targetIdx" .= tIdx, "wire" .= w
        ]
    toJSON (PubSoloCut2 (a, b)) = A.object
        [ "tag" .= ("SoloCut2" :: String), "positions" .= [a, b] ]
    toJSON (PubSoloCut4 (a, b, c, d)) = A.object
        [ "tag" .= ("SoloCut4" :: String), "positions" .= [a, b, c, d] ]
    toJSON (PubRemoveRed i) = A.object
        [ "tag" .= ("RemoveRed" :: String), "position" .= i ]
    toJSON (PubPlaceInfoMarker i) = A.object
        [ "tag" .= ("PlaceInfoMarker" :: String), "position" .= i ]

instance A.ToJSON PublicEquipmentUse where
    toJSON PubUseStabilizer = A.object [ "tag" .= ("Stabilizer" :: String) ]
    toJSON (PubUseSuperDetector pid w) = A.object
        [ "tag" .= ("SuperDetector" :: String), "target" .= pid, "wire" .= w ]
    toJSON (PubUseTripleDetector pid (p1, p2, p3) w) = A.object
        [ "tag" .= ("TripleDetector" :: String)
        , "target" .= pid, "positions" .= [p1, p2, p3], "wire" .= w
        ]
    toJSON (PubUseDoubleDetector pid (p1, p2) w) = A.object
        [ "tag" .= ("DoubleDetector" :: String)
        , "target" .= pid, "positions" .= [p1, p2], "wire" .= w
        ]
    toJSON (PubUseDoubleRadiator pid tIdx (w1, w2)) = A.object
        [ "tag" .= ("DoubleRadiator" :: String)
        , "target" .= pid, "targetIdx" .= tIdx, "wires" .= [w1, w2]
        ]

instance A.ToJSON TurnLog where
    toJSON tl = A.object
        [ "round"     .= logRound tl
        , "player"    .= logPlayer tl
        , "action"    .= logAction tl
        , "equipment" .= logEquipment tl
        , "outcome"   .= logOutcome tl
        ]
