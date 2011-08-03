{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events
import GHC.RTS.Events.Merge
import GHC.RTS.Events.Sparks

import System.Environment
import Text.Printf
import Data.List
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe
import System.IO
import System.Exit

main = getArgs >>= command

command ["show", file] = do
    log <- readLogOrDie file
    putStrLn $ ppEventLog log

command ["merge", out, file1, file2] = do
    log1 <- readLogOrDie file1
    log2 <- readLogOrDie file2
    let m = mergeEventLogs log1 log2
    writeEventLogToFile out m

command ["sparks-csv", file] = do
    EventLog _ (Data es) <- readLogOrDie file
    putStr . csvSparks . sparkInfo $ es

readLogOrDie file = do
    e <- readEventLogFromFile file
    case e of
        Left s    -> die ("Failed to parse " ++ file ++ ": " ++ s)
        Right log -> return log

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

csv = intercalate ","

csvSparks sp = unlines . map csv $ firstline : map aline sp
 where
    firstline =                      ["started"  , "finished"  , "idle"  , "duration"]
    aline sei = map (show . ($ sei)) [timeStarted, timeFinished, timeIdle, sparkDuration]
