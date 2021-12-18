module GPUScan where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

type PowerList a = Vector a

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

runGpu = A.toList $ GPU.run $ ldf (+) 20 (use $ fromList (Z:.10) [1..10] :: Vector Int)

ldf :: Int -> PowerList a -> PowerList a
ldf _  l | A.length l <= 1 = l
ldf cs l = runEval (do
    p <- rpar $ UVP.filterOdd l
    q <- rpar $ UVP.filterEven l
    _ <- rseq p
    _ <- rseq q
    pq <- UVP.parZipWith (rparWith rdeepseq) op cs p q
    t <- rpar (parLdfUBVec op cs (d-1) pq)
    k <- rseq $ UVP.shiftAdd2 t p
    UVP.parZip (rparWith rdeepseq) cs k t)
-- parLdfUBVec op _ l = UV.scanl1 (+) l