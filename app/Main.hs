module Main where

import CLParser
import Lib
import Scan

main :: IO ()
main = run =<< parseArgs

run :: Opts -> IO ()
run opts = case opts of
    Opts (Scan x n) Parallel True -> putStrLn "Run parallel scan without powerlist"
    Opts (Scan x n) Parallel False -> putStrLn "Run parallel scan with powerlist"
    -- Run sequential prefix sum without powerlist
    Opts (Scan x n) Sequential True -> putStrLn $ runSequentialSPS n
    Opts (Scan x n) Sequential False -> putStrLn $ runSPS n