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
import qualified Data.Vector.Unboxed.Mutable as M
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
runBatcherSort cs inp = show $ V.last $ batcherMergeSort inp cs $ V.generate (2^inp) (\i -> 2^inp - i)

defaultSort :: Ord a => [a] -> [a]
defaultSort = sort

batcherMergeSort :: (NFData a, Ord a, V.Unbox a) => Int -> Int -> P.PowerList a -> P.PowerList a
batcherMergeSort _ _ l | V.length l <= 1 = l
batcherMergeSort d cs l | d > 5 = runEval(do
    p <- rpar (V.ifilter (\i a -> even i) l)
    q <- rpar (V.ifilter (\i a -> odd i) l)
    sortp <- rparWith rdeepseq (batcherMergeSort (d-1) cs p)
    sortq <- rparWith rdeepseq (batcherMergeSort (d-1) cs q)
    batcherMerge (rparWith rdeepseq) d cs sortp sortq)
batcherMergeSort d cs l = V.fromList $ defaultSort $ V.toList l

batcherMerge :: (Ord a, V.Unbox a) => Strategy (P.PowerList a) -> Int -> Int -> P.PowerList a -> P.PowerList a -> Eval (P.PowerList a)
batcherMerge strategy d cs x y | V.length x == 1 = rseq $ V.fromList [hx `min` hy, hx `max` hy]
    where hx = V.head x
          hy = V.head y
batcherMerge strategy d cs x y | d > 5 = do
    r <- rpar (V.ifilter (\i a -> even i) x)
    s <- rpar (V.ifilter (\i a -> odd i) x)
    u <- rpar (V.ifilter (\i a -> even i) y)
    v <- rpar (V.ifilter (\i a -> odd i) y)
    rv <- batcherMerge strategy (d-1) cs r v
    su <- batcherMerge strategy (d-1) cs s u
    P.parMinMaxZip rpar cs rv su
batcherMerge strategy d cs x y = strategy (merge x y)

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
