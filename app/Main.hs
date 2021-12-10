module Main where

import CLParser
import Lib

main :: IO ()
main = run =<< parseArgs

run :: Opts -> IO ()
run opts = case opts of
    Opts (Scan x n) Parallel -> putStrLn "Run parallel scan"
    Opts (Scan x n) Sequential -> putStrLn "Run sequential scan"