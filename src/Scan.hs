module Scan where

import qualified Powerlist as P

-- import qualified Data.Vector.Unboxed as V
-- import Data.Vector.Fusion.Bundle (inplace)

split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

-- generateArray :: Int -> V.Vector Int
-- generateArray uB = V.fromList [1..uB]

-- sequentialSPS :: V.Vector Int -> V.Vector Int
-- sequentialSPS = V.scanl1 (+)

{-
  Generates a list from 1 to 2^n
-}
generateList :: Int -> [Int]
generateList n = [1..2^n]

{-
  Simple sequential SPS is nothing but haskel's scanl1
-}
sequentialSPS :: (a -> a -> a) -> [a] -> [a]
sequentialSPS = scanl1

runSequentialSPS :: Int -> String
runSequentialSPS inp = show $ sequentialSPS (+) $ generateList inp

{-
  Simple sequential SPS using powerlist, works for lists with power of 2 length
-}
sps :: Num a => (a -> a -> a) -> P.PowerList a -> P.PowerList a
sps _ [] = []
sps _ [x] = [x]
sps op l = P.zip (sps op u) (sps op v)
  where (u, v) = P.unzip $ zipWith op (P.rsh (0, l)) l

runSPS :: Int -> String
runSPS inp = show $ sps (+) $ generateList inp
