module Utils where

import qualified Data.Vector.Unboxed as V

{-
  Generates a list from 2^n to 1
-}
generateReverseList :: Int -> [Int]
generateReverseList n = [2 ^ n,2 ^ n - 1 .. 1]

{-
  Generates a list from 2^n to 1
-}
generateList :: Int -> [Int]
generateList n = [1 .. 2 ^ n]

{-
  Generate Unboxed Vector with values from 1 to 2^n
-}
generateUVec :: Int -> V.Vector Int
generateUVec n = V.generate (2 ^ n) (+ 1)

{-
  Generate Unboxed Vector with values from 2^n to 1
-}
generateReverseUVec :: Int -> V.Vector Int
generateReverseUVec n = V.generate (2 ^ n) (\i -> 2 ^ n - i)