module Utils where

files :: [FilePath]
files = map ("test/"++)
    [ "queens-ghc-6.12.1.eventlog"
    , "queens-ghc-7.0.2.eventlog"
    , "mandelbrot-mmc-2011-06-14.eventlog"
    , "mdlLogMPI1.eventlog"
    , "pre77stop.eventlog", "782stop.eventlog", "783stop.eventlog" ]


-- Code to help print the differences between a working test and a failing test.
diffLines o n = diff 1 (lines o) (lines n)

diff :: Int -> [String] -> [String] -> String
diff _ [] [] = "Logs match"
diff l [] (n:ns) = "Extra lines in new log at line " ++ show l ++ ":\n" ++
    (unlines (n:ns))
diff l (o:os) [] = "Missing lines in new log at line " ++ show l ++ ":\n" ++
    (unlines (o:os))
diff l (o:os) (n:ns) = if (o == n)
                        then diff (l+1) os ns
                        else "Different output at line " ++ show l ++ ":\n" ++
                            "Original: " ++ o ++ "\n" ++
                            "New:      " ++ n
