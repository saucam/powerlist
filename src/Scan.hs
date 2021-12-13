module Scan where

import qualified Powerlist as P
import Control.Parallel.Strategies
    ( parBuffer,
      parList,
      parListChunk,
      rdeepseq,
      r0,
      rpar,
      rseq,
      runEval,
      using,
      rparWith,
      withStrategy,
      Eval,
      Strategy )
import Control.DeepSeq ( force, NFData )

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

--------------------------------------------------------------------------------
-- Sequential SPS is nothing but haskel's scanl1
--------------------------------------------------------------------------------
sequentialSPS :: (a -> a -> a) -> [a] -> [a]
sequentialSPS = scanl1

--------------------------------------------------------------------------------
-- Sequential SPS using powerlist, works for lists with power of 2 length
--------------------------------------------------------------------------------
sps :: Num a => (a -> a -> a) -> P.PowerList a -> P.PowerList a
sps _ [] = []
sps _ [x] = [x]
sps op l = P.zip (sps op u) (sps op v)
  where (u, v) = P.unzip $ zipWith op (P.rsh (0, l)) l

--------------------------------------------------------------------------------
-- Parallel SPS Version1 using powerlist, works for lists with power of 2 length
--------------------------------------------------------------------------------
parSps1 :: Num a => (a -> a -> a) -> P.PowerList a -> P.PowerList a
parSps1 _ [] = []
parSps1 _ [x] = [x]
parSps1 op l = runEval (do
    (u, v) <- rseq $ P.unzip $ zipWith op (P.rsh (0, l)) l
    u' <- rpar (parSps1 op u)
    v' <- rpar (parSps1 op v)
    return $ P.zip u' v')

runScan :: ((Int -> Int -> Int) -> P.PowerList Int -> P.PowerList Int)-> Int -> String
runScan f inp = show $ sum $ f (+) $ generateList inp

parZipWith :: Strategy c -> Int -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith strategy cs z as bs = Prelude.zipWith z as bs `using` parListChunk cs strategy

parSps2 :: NFData a => Num a => (a -> a -> a) -> Int -> P.PowerList a -> P.PowerList a
parSps2 _ _ [] = []
parSps2 _ _ [x] = [x]
parSps2 op cs l = runEval (do
    (u, v) <- rseq $ P.unzip $ parZipWith rdeepseq cs op (0: l) l
    u' <- rparWith rdeepseq (parSps2 op cs u)
    v' <- rparWith rdeepseq (parSps2 op cs v)
    r0 $ P.zip u' v')
    --r0 $ P.parZip rpar cs u' v')


runParScan :: Int -> Int -> String
runParScan cs inp = show $ sum $ parSps2 (+) cs $ generateList inp

{-
parSps3 _ [] = []
parSps3 _ [x] = [x]
parSps3 op l = runEval (do
    (u, v) <- rseq $ P.unzip l
    s <- rparWith rseq (parSps3 op $ parZipWith rdeepseq op u v)
    return $ P.zip s (parZipWith rdeepseq op s u))
-}
--------------------------------------------------------------------------------
-- Ladner Fischer Algorithm
--------------------------------------------------------------------------------



