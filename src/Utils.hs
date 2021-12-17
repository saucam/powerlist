module Utils where

{-
  Generates a list from 2^n to 1
-}
generateList :: Int -> [Int]
generateList n = [2^n, 2^n-1..1]
