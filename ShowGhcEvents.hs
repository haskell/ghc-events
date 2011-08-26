{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events as Log
import System.Environment
import Text.Printf
import Data.List
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe
import System.IO
import System.Exit

main = do
  [file] <- getArgs

  log <- do e <- readEventLogFromFile file
            case e of
               Left  s   -> die ("Failed to parse " ++ file ++ ": " ++ s)
               Right log -> return log
    
  putStrLn $ ppEventLog log

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
