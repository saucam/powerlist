module Main where

import CLParser
import Lib

main :: IO ()
main = run =<< parseArgs

run :: Opts -> IO ()
run opts = case opts of
    Opts (Scan x n) Parallel True -> putStrLn "Run parallel scan without powerlist"
    Opts (Scan x n) Parallel False -> putStrLn "Run parallel scan with powerlist"
    Opts (Scan x n) Sequential True -> putStrLn "Run sequential scan without powerlist"
    Opts (Scan x n) Sequential False -> putStrLn "Run sequential scan with powerlist"