module Main where

import CLParser
import Lib
import Scan

main :: IO ()
main = run =<< parseArgs

run :: Opts -> IO ()
run opts = case opts of
    Opts (Scan BLELLOCH n _) -> putStrLn "Run parallel scan without powerlist"
    -- Run parallel prefix sum with powerlist
    Opts (Scan SPSPLPar1 n _) -> putStrLn $ runScan parSps1 n
    Opts (Scan SPSPLPar2 n c) -> putStrLn $ runParScan c n
    -- Opts (Scan SPSPLPar3 n _) -> putStrLn $ runScan parSps3 n
    -- Run sequential prefix sum without powerlist
    Opts (Scan SPS n _) -> putStrLn $ runScan sequentialSPS n
    -- Run sequential prefix sum with powerlist
    Opts (Scan SPSPL n _) -> putStrLn $ runScan sps n