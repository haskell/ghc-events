module GHC.RTS.LiveLogging {-# WARNING "Live logging is VERY experimental." #-} (
  listen,
  printEventsIncremental,
  pullEvents) where

import GHC.RTS.EventsIncremental

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Debug.Trace (trace)
import Network
import System.IO
import Text.Printf (printf)


pullEvents :: Handle -> IO ()
pullEvents h = do
    eh <- ehOpen h 4096
    -- Using True so that the handle would be queried until the log is complete
    trace "Pulling events" $ printEventsIncremental eh True

listen :: Int -> IO ()
listen portno = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portno))
  printf "Listening on port %d\n" portno
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (pullEvents handle)
                (\_ -> putStrLn "closed" >> hClose handle)

