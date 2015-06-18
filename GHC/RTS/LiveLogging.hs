module GHC.RTS.LiveLogging (
  listen,
  printEventsIncremental,
  pullEvents) where

import GHC.RTS.Events

import Control.Concurrent (threadDelay, forkFinally)
import Control.Monad (forever)
import Debug.Trace (trace)
import Network
import System.IO
import Text.Printf (printf)


port :: Int
port = 44444

pullEvents :: Handle -> IO ()
pullEvents h = do
    eh <- ehOpen h 4096
    -- Using True so that the handle would be queried until the log is complete
    trace "Pulling events" $ printEventsIncremental eh True

listen :: Int -> IO ()
listen port = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (pullEvents handle)
                (\_ -> putStrLn "closed" >> hClose handle)

printEventsIncremental :: EventHandle
                       -> Bool -- Whether to retry on incomplete logs
                       -> IO ()
printEventsIncremental eh dashf = do
    event <- ehReadEvent eh
    case event of
      Item ev -> do
          putStrLn (ppEvent' ev) -- if actual printing is needed
          printEventsIncremental eh dashf
      Incomplete ->
        if dashf
          then print "Log Incomplete. Waiting for more input." >> threadDelay 1000000 >> printEventsIncremental eh dashf
          else putStrLn "Finished (NOT all file was parsed successfully)"
      Complete ->
        putStrLn "Finished (file was parsed successfully)"
      ParseError errMsg ->
        putStrLn $ "Error: " ++ errMsg
