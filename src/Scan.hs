module Scan where

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
import Utils ( generateList, generateUVec )

import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector                 as V
import qualified Data.Vector.Split           as S
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Powerlist                   as P
import qualified UBVecPowerlist              as UVP

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
  where (u, v) = P.unzip $ P.zipWith op (P.rsh 0 l) l

--------------------------------------------------------------------------------
-- Parallel SPS Version1 using powerlist, works for lists with power of 2 length
--------------------------------------------------------------------------------
parSps1 :: Num a => (a -> a -> a) -> P.PowerList a -> P.PowerList a
parSps1 _ [] = []
parSps1 _ [x] = [x]
parSps1 op l = runEval (do
    (u, v) <- rseq $ P.unzip $ P.zipWith op (P.rsh 0 l) l
    u' <- rpar (parSps1 op u)
    v' <- rpar (parSps1 op v)
    return $ P.zip u' v')

runScan :: ((Int -> Int -> Int) -> P.PowerList Int -> P.PowerList Int)-> Int -> String
runScan f inp = show $ sum $ f (+) $ generateList inp

odds [] = []
odds [x] = [x]
odds (x:y:xs) = x : odds xs

evens [] = []
evens [x] = []
evens (x:y:xs) = y : evens xs

parSps2 :: NFData a => Num a => (a -> a -> a) -> Int -> P.PowerList a -> P.PowerList a
parSps2 _ _ [] = []
parSps2 _ _ [x] = [x]
parSps2 op cs l = runEval (do
    k <- rseq $ P.parZipWith rdeepseq cs op (0: l) l
    u <- rpar (odds k)
    v <- rpar (evens k)
    --(u, v) <- rseq $ P.unzip k
    u' <- rparWith rdeepseq (parSps2 op cs u)
    v' <- rparWith rdeepseq (parSps2 op cs v)
    r0 $ P.zip u' v')
    --r0 $ P.parZip rpar cs u' v')

{-
  Parallel till certain depth, for arrays of size  <= 2^3, use sequentialSPS
-}
parSps3 :: NFData a => Num a => (a -> a -> a) -> Int -> Int -> P.PowerList a -> P.PowerList a
parSps3 _ _ _ [] = []
parSps3 _ _ _ [x] = [x]
parSps3 op cs d l | d > 4 = runEval (do
    k <- rseq $ P.parZipWith rdeepseq cs op (0: l) l
    u <- rpar (odds k)
    v <- rpar (evens k)
    --(u, v) <- rseq $ P.unzip k
    u' <- rparWith rdeepseq (parSps3 op cs (d-1) u)
    v' <- rparWith rdeepseq (parSps3 op cs (d-1) v)
    r0 $ P.zip u' v')
parSps3 op cs d l = sequentialSPS op l

runParScan2 :: Int -> Int -> String
runParScan2 cs inp = show $ sum $ parSps2 (+) cs $ generateList inp

runParScan3 :: Int -> Int -> String
runParScan3 cs inp = show $ sum $ parSps3 (+) cs inp $ generateList inp

runParLdf :: Int -> Int -> String
runParLdf cs inp = show $ sum $ parLdf (+) cs inp $ generateList inp

runParSpsUBVec :: Int -> Int -> String
runParSpsUBVec cs inp = show $ UV.sum $ parSpsUBVec (+) cs inp $ generateUVec inp

runParLdfUBVec :: Int -> Int -> String
runParLdfUBVec cs inp = show $ UV.sum $ parLdfUBVec (+) cs inp $ generateUVec inp

--runParLdfChunkUBVec :: Int -> Int -> String
--runParLdfChunkUBVec cs inp = show $ UV.sum $ parLdfChunkUBVec (+) cs inp $ generateUVec inp

--------------------------------------------------------------------------------
-- Ladner Fischer Algorithm
--------------------------------------------------------------------------------

ldf :: Num a => (a -> a -> a) -> P.PowerList a -> P.PowerList a
ldf _ []         = []
ldf _ [x]        = [x]
ldf op l         = P.zip (P.zipWith op (P.rsh 0 t) p) t
   where (p, q)  = P.unzip l
         pq      = P.zipWith op p q
         t       = ldf op pq

{-
  A parallel version of LDF
-}
parLdf :: NFData a => Num a => (a -> a -> a) -> Int -> Int -> P.PowerList a -> P.PowerList a
parLdf _ _ _ []        = []
parLdf _ _ _ [x]       = [x]
parLdf op cs d l | d > 4 = runEval (do
  p <- rpar (odds l)
  q <- rpar (evens l)
  pq <- rseq (P.parZipWith rdeepseq cs op p q)
  t <- rparWith rdeepseq (parLdf op cs (d-1) pq)
  k <- r0 (P.parZipWith rdeepseq cs op (0: t) p)
  r0 $ P.zip k t)
parLdf op cs d l = sequentialSPS op l

--------------------------------------------------------------------------------
-- SPS and LDF using powerlist unboxed vector implementation
--------------------------------------------------------------------------------
parSpsUBVec :: (NFData a, UV.Unbox a, Num a) => (a -> a -> a) -> Int -> Int -> UVP.PowerList a -> UVP.PowerList a
parSpsUBVec op cs d l | UV.length l <= 1 = l
parSpsUBVec op cs d l | d > 4 = runEval (do
    k <- rseq $ UVP.shiftAdd l
    u <- rpar (UV.ifilter (\i a -> odd i) k)
    v <- rpar (UV.ifilter (\i a -> even i) k)
    u' <- rpar (parSpsUBVec op cs (d-1) u)
    v' <- rpar (parSpsUBVec op cs (d-1) v)
    r0 $ UVP.zip u' v')
parSpsUBVec op cs d l = UV.scanl1 (+) l

parLdfUBVec :: (NFData a, UV.Unbox a, Num a) => (a -> a -> a) -> Int -> Int -> UVP.PowerList a -> UVP.PowerList a
parLdfUBVec op cs d l | UV.length l <= 1 = l
parLdfUBVec op cs d l | d > 4 = runEval (do
--    p <- rpar (UV.ifilter (\i a -> even i) l)
--    q <- rpar (UV.ifilter (\i a -> odd i) l)
    p <- rseq $ UVP.filterOdd l
    q <- rseq $ UVP.filterEven l
    pq <- UVP.parZipWith (rparWith rdeepseq) op cs p q
    t <- rpar (parLdfUBVec op cs (d-1) pq)
    k <- rseq $ UVP.shiftAdd2 t p
    UVP.parZip (rparWith rdeepseq) cs k t)
parLdfUBVec op cs d l = UV.scanl1 (+) l


{-- Try chunked approach
parLdfChunkUBVec :: (NFData a, UV.Unbox a, Num a) => (a -> a -> a) -> Int -> Int -> UVP.PowerList a -> UVP.PowerList a
parLdfChunkUBVec op cs d l | UV.length l <= 1 = l
parLdfChunkUBVec op cs d l = runEval $ parLdfChunkVec' op chunks  
    where
        n = UV.length l
        chunkSize = 2^cs
        chunks = S.chunksOf chunkSize l
        numChunks = n `div` chunkSize
        parLdfChunkVec' :: (NFData a, UV.Unbox a, Num a) => (a -> a -> a) -> [UVP.PowerList a] -> Eval (UVP.PowerList a)
        parLdfChunkVec' op [] = return UV.empty
        parLdfChunkVec' op chunks = do
            resChunks <- parList rdeepseq (parLdfUBVec op cs d <$> chunks)
            res <- rdeepseq $ UV.concat resChunks
            -- get last element of each block
            lastelems <- parList rdeepseq (UV.last <$> resChunks)
            lastScan <- rpar (UV.fromList $ sequentialSPS op lastelems)
            rseq $ UV.create $ do
                m <- UV.thaw res
                mergeChunks (n-1) (UV.tail $ UV.reverse lastScan) m
                return m
                where mergeChunks i lastScan m
                       | i > 0 = do
                            let ad = UV.head lastScan
                            let start = i
                            go m chunkSize start ad 0
                            mergeChunks (i-chunkSize) (UV.tail lastScan) m
                       | otherwise = return ()
          
go m chunkSize start v id
    | id <= chunkSize = do
      curr <- M.unsafeRead m (start - id)
      M.unsafeWrite m (start - id) (curr + v)
      go m chunkSize start v (id+1)
    | otherwise = return ()
-}
