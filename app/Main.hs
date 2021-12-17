module Main where

import CLParser
import Scan
import Sort

main :: IO ()
main = run =<< parseArgs

run :: Opts -> IO ()
run opts = case opts of
    Opts (Scan BLELLOCH n _) -> putStrLn "Run parallel scan without powerlist"
    -- Run parallel prefix sum with powerlist
    Opts (Scan SPSPLPar1 n _) -> putStrLn $ runScan parSps1 n
    Opts (Scan SPSPLPar2 n c) -> putStrLn $ runParScan2 c n
    Opts (Scan SPSPLPar3 n c) -> putStrLn $ runParScan3 c n
    -- Run prefix sum via ldf algo (sequential)
    Opts (Scan LDF n _) -> putStrLn $ runScan ldf n
    -- Run parallel prefix sum via ldf algo
    Opts (Scan LDFPar n c) -> putStrLn $ runParLdf c n
    -- Run sequential prefix sum without powerlist
    Opts (Scan SPS n _) -> putStrLn $ runScan sequentialSPS n
    -- Run sequential prefix sum with powerlist
    Opts (Scan SPSPL n _) -> putStrLn $ runScan sps n
    -- Run parallel prefix sum using unboxed vecpowerlist
    Opts (Scan SPSUBVecPLPar n cs) -> putStrLn $ runParSpsUBVec cs n
    Opts (Scan LDFUBVecPLPar n cs) -> putStrLn $ runParLdfUBVec cs n
    Opts (Scan LDFChunkUBVecPLPar n cs) -> putStrLn $ runParLdfChunkUBVec cs n
    -- Run sort
    Opts (Sort DEFAULT n cs) -> putStrLn $ runDefaultSort cs n
    Opts (Sort BATCHER n cs) -> putStrLn $ runBatcherSort cs n