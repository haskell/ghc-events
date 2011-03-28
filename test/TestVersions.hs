{-
This test parses sample event logs from each major GHC version and compares
the pretty printed output with reference output.

When tests fail, use a diff tool to compare the output of show-ghc-events with
the reference file.  Resolve the differences in either the library code or the
reference file, and 'darcs record' the changes.

Steps to produce event log and reference output:
    $ ghc --make queens.hs -o queens-ghc-$VERSION -threaded -eventlog
    $ queens-ghc-$VERSION 8 +RTS -N4 -ls
    $ show-ghc-events queens-ghc-$VERSION.eventlog > queens-ghc-$VERSION.eventlog.reference

Where queens.hs is http://darcs.haskell.org/nofib/parallel/queens/Main.hs
-}

import GHC.RTS.Events
import System.Exit

files :: [FilePath]
files = map ("test/"++)
    [ "queens-ghc-6.12.1.eventlog"
    , "queens-ghc-7.0.2.eventlog" ]

-- returns True on success
testFile :: FilePath -> IO Bool
testFile f = do
    e <- readEventLogFromFile f
    let oops s = putStrLn (f ++ ": failure" ++ s) >> return False

    case e of
        Left m -> oops m

        Right newlog -> do
            oldlog <- readFile (f ++ ".reference")
            if oldlog == ppEventLog newlog
                then putStrLn (f ++ ": success") >> return True
                else oops "pretty print output does not match"
main = do
    successes <- mapM testFile files
    if and successes
        then return ()
        else exitFailure
