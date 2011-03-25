import GHC.RTS.Events
import System.Exit

{-
TODO: perform a sanity check on the deserialized logs.  Fuzzy matching may be
required.  For now, we simply check whether deserialization is successful.

Steps to produce event log:
    $ ghc --make queens.hs -o queens-ghc-$VERSION -threaded -eventlog
    $ queens-ghc-$VERSION 8 +RTS -N4 -ls

Where queens.hs is http://darcs.haskell.org/nofib/parallel/queens/Main.hs
-}
files :: [FilePath]
files = map ("test/"++)
    [ "queens-ghc-6.12.1.eventlog"
    , "queens-ghc-7.0.2.eventlog" ]

-- returns True on success
testFile :: FilePath -> IO Bool
testFile f = do
    e <- readEventLogFromFile f
    case e of
        Right _ -> putStrLn (f ++ ": success") >> return True
        Left m  -> putStrLn (f ++ ": fail; " ++ m) >> return False


main = do
    successes <- mapM testFile files
    if and successes
        then return ()
        else exitFailure
