module Sort where

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies
  ( Eval
  , rdeepseq
  , rpar
  , rparWith
  , rseq
  , runEval
  )
import Data.List (sort)
import Utils (generateReverseList, generateReverseUVec)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified UBVecPowerlist as P

runDefaultSort :: Int -> Int -> String
runDefaultSort _ inp = show $ last $ defaultSort $ generateReverseList inp

runBatcherSort :: Int -> Int -> String
runBatcherSort _ inp =
  show $ V.last $ parBatcherMergeSort inp $ generateReverseUVec inp

defaultSort :: Ord a => [a] -> [a]
defaultSort = sort

--------------------------------------------------------------------------------
-- Sequential Impl for demonstration
--------------------------------------------------------------------------------
batcherMergeSort :: (Ord a, V.Unbox a) => P.PowerList a -> P.PowerList a
batcherMergeSort l
  | V.length l <= 1 = l
batcherMergeSort l = sortp `batcherMerge` sortq
  where
    sortp = batcherMergeSort p
    sortq = batcherMergeSort q
    p = P.filterOdd l
    q = P.filterEven l

batcherMerge ::
     (Ord a, V.Unbox a) => P.PowerList a -> P.PowerList a -> P.PowerList a
batcherMerge x y
  | V.length x == 1 = V.fromList [hx `min` hy, hx `max` hy]
  where
    hx = V.head x
    hy = V.head y
batcherMerge x y = P.minMaxZip rv su
  where
    rv = r `batcherMerge` v
    su = s `batcherMerge` u
    r = P.filterOdd x
    v = P.filterEven y
    s = P.filterEven x
    u = P.filterOdd y

--------------------------------------------------------------------------------
-- Parallel Impl
--------------------------------------------------------------------------------
parBatcherMergeSort ::
     (NFData a, Ord a, V.Unbox a) => Int -> P.PowerList a -> P.PowerList a
parBatcherMergeSort _ l
  | V.length l <= 1 = l
parBatcherMergeSort d l
  | d > 10 =
    runEval
      (do p <- rpar $ P.filterOdd l
          q <- rpar $ P.filterEven l
          _ <- rseq p
          sortp <- rparWith rdeepseq (parBatcherMergeSort (d - 1) p)
          _ <- rseq q
          sortq <- rparWith rdeepseq (parBatcherMergeSort (d - 1) q)
          parBatcherMerge d sortp sortq)
parBatcherMergeSort _ l = V.fromList $ defaultSort $ V.toList l

parBatcherMerge ::
     (Ord a, V.Unbox a)
  => Int
  -> P.PowerList a
  -> P.PowerList a
  -> Eval (P.PowerList a)
--batcherMerge strategy d cs x y | V.length x == 1 = rseq $ V.fromList [hx `min` hy, hx `max` hy]
--    where hx = V.head x
--          hy = V.head y
parBatcherMerge d x y
  | d > 10 = do
    r <- rseq $ P.filterOdd x
    v <- rseq $ P.filterEven y
    rv <- parBatcherMerge (d - 1) r v
    s <- rseq $ P.filterEven x
    u <- rseq $ P.filterOdd y
    su <- parBatcherMerge (d - 1) s u
    rparWith rdeepseq $ P.minMaxZip rv su
parBatcherMerge _ x y = rseq (merge x y)

merge :: (Ord a, V.Unbox a) => P.PowerList a -> P.PowerList a -> P.PowerList a
merge a b =
  V.create $ do
    v <- M.new nm
    go 0 0 v
    return v
  where
    n = V.length a
    m = V.length b
    nm = n + m
    go i j v
      | (i + j) < nm = do
        let ai = a V.! i
        let bj = b V.! j
        if (j == m) || (i < n && ai <= bj)
          then do
            M.unsafeWrite v (i + j) ai
            go (i + 1) j v
          else do
            M.unsafeWrite v (i + j) bj
            go i (j + 1) v
      | otherwise = return ()