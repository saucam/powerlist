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
import Data.List (sort)
import Utils ( generateReverseList )

import qualified Data.Vector.Unboxed         as V
import qualified UBVecPowerlist              as P
import qualified Data.Vector.Unboxed.Mutable as M

runDefaultSort :: Int -> Int -> String
runDefaultSort cs inp = show $ last $ defaultSort $ generateReverseList inp

runBatcherSort :: Int -> Int -> String
runBatcherSort cs inp = show $ V.last $ parBatcherMergeSort inp $ V.generate (2^inp) (\i -> 2^inp - i)

defaultSort :: Ord a => [a] -> [a]
defaultSort = sort

--------------------------------------------------------------------------------
-- Sequential Impl
--------------------------------------------------------------------------------
batcherMergeSort :: (Ord a, V.Unbox a) => P.PowerList a -> P.PowerList a
batcherMergeSort l | V.length l <= 1 = l
batcherMergeSort l = sortp `batcherMerge` sortq
    where sortp = batcherMergeSort p
          sortq = batcherMergeSort q
          p     = P.filterOdd l
          q     = P.filterEven l

batcherMerge :: (Ord a, V.Unbox a) => P.PowerList a -> P.PowerList a -> P.PowerList a
batcherMerge x y | V.length x == 1 = V.fromList [hx `min` hy, hx `max` hy]
    where hx = V.head x
          hy = V.head y
batcherMerge x y = P.minMaxZip rv su
    where rv = r `batcherMerge` v
          su = s `batcherMerge` s
          r  = P.filterOdd x
          v  = P.filterEven y
          s  = P.filterEven x
          u  = P.filterOdd y

--------------------------------------------------------------------------------
-- Parallel Impl
--------------------------------------------------------------------------------
parBatcherMergeSort :: (NFData a, Ord a, V.Unbox a) => Int -> P.PowerList a -> P.PowerList a
parBatcherMergeSort _ l | V.length l <= 1 = l
parBatcherMergeSort d  l | d > 5 = runEval(do
    p <- rseq $ P.filterOdd l
    q <- rseq $ P.filterEven l
    sortp <- rparWith rdeepseq (parBatcherMergeSort (d-1) p)
    sortq <- rparWith rdeepseq (parBatcherMergeSort (d-1) q)
    parBatcherMerge d sortp sortq)
parBatcherMergeSort d l = V.fromList $ defaultSort $ V.toList l

parBatcherMerge :: (Ord a, V.Unbox a) => Int -> P.PowerList a -> P.PowerList a -> Eval (P.PowerList a)
--batcherMerge strategy d cs x y | V.length x == 1 = rseq $ V.fromList [hx `min` hy, hx `max` hy]
--    where hx = V.head x
--          hy = V.head y
parBatcherMerge d x y | d > 6 = do
    r <- rseq $ P.filterOdd x
    s <- rseq $ P.filterEven x
    u <- rseq $ P.filterOdd  y
    v <- rseq $ P.filterEven y
    rv <- parBatcherMerge (d-1) r v
    su <- parBatcherMerge (d-1) s u
    rparWith rdeepseq $ P.minMaxZip rv su
parBatcherMerge d x y = r0 (merge x y)

merge :: (Ord a, V.Unbox a) => P.PowerList a -> P.PowerList a -> P.PowerList a
merge a b = V.create $ do
    v <- M.new nm
    go 0 0 v
    return v
        where
            n = V.length a
            m = V.length b
            nm = n + m
            go i j v
                | (i + j) < nm = do
                    let ai =  a V.! i
                    let bj = b V.! j
                    if (j == m) || (i < n && ai <= bj) then do
                        M.unsafeWrite v (i+j) ai
                        go (i+1) j v
                    else do
                        M.unsafeWrite v (i+j) bj
                        go i (j+1) v
                | otherwise = return ()
