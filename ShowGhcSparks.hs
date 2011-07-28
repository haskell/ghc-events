{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events as Log
import GHC.RTS.Events.Sparks
import System.Environment
import Data.List
import System.IO
import System.Exit

main = do
  [file] <- getArgs

  EventLog _ (Data es) <- do
            e <- readEventLogFromFile file
            case e of
               Left  s   -> die ("Failed to parse " ++ file ++ ": " ++ s)
               Right log -> return log
    
  putStr . csvSparks . sparkInfo $ es

csv :: [String] -> String
csv = intercalate ","

csvSparks sp = unlines . map csv $ firstline : map aline sp
 where
    firstline =                      ["started"  , "finished"  , "idle"  , "duration"]
    aline sei = map (show . ($ sei)) [timeStarted, timeFinished, timeIdle, sparkDuration]

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
