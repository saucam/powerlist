module Sort where

import Control.Monad (join)
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
      Strategy, unEval )
import Control.DeepSeq ( force, NFData )

import qualified Data.Vector.Unboxed         as V
import qualified UBVecPowerlist              as P
import Data.List (sort)
import Data.Bits (Bits(xor))

{-
  Generates a list from 1 to 2^n
-}
generateList :: Int -> [Int]
generateList n = [2^n, 2^n-1..1]

runDefaultSort :: Int -> Int -> String
runDefaultSort cs inp = show $ last $ defaultSort $ generateList inp

runBatcherSort :: Int -> Int -> String
runBatcherSort cs inp = show $ V.last $ batcherMergeSort inp $ V.generate (2^inp) (\i -> 2^inp - i)

defaultSort :: Ord a => [a] -> [a]
defaultSort = sort

batcherMergeSort :: (NFData a, Ord a, V.Unbox a) => Int -> P.PowerList a -> P.PowerList a
batcherMergeSort _ l | V.length l <= 1 = l
batcherMergeSort d l | d > 4 = runEval(do
    p <- rpar (V.ifilter (\i a -> even i) l)
    q <- rpar (V.ifilter (\i a -> odd i) l)
    sortp <- rparWith rdeepseq (batcherMergeSort d p)
    sortq <- rparWith rdeepseq (batcherMergeSort d q)
    batcherMerge (rparWith rdeepseq) sortp sortq)
batcherMergeSort d l = V.fromList $ sort $ V.toList l

batcherMerge :: (Ord a, V.Unbox a) => Strategy (P.PowerList a) -> P.PowerList a -> P.PowerList a -> Eval (P.PowerList a)
batcherMerge strategy x y | V.length x == 1 = strategy $ V.fromList [hx `min` hy, hx `max` hy]
    where hx = V.head x
          hy = V.head y
batcherMerge strategy x y = do
    r <- rpar (V.ifilter (\i a -> even i) x)
    s <- rpar (V.ifilter (\i a -> odd i) x)
    u <- rpar (V.ifilter (\i a -> even i) y)
    v <- rpar (V.ifilter (\i a -> odd i) y)
    rv <- batcherMerge (rparWith rdeepseq) r v
    su <- batcherMerge (rparWith rdeepseq) s u
    rparWith rdeepseq $ P.minMaxZip rv su


