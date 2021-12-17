module Main where

import Utils
    ( generateList,
      generateUVec,
      generateReverseList,
      generateReverseUVec )

import Criterion.Main ( defaultMainWith, bench, defaultConfig, nf, bgroup, env )
import Criterion.Types ( Config(resamples) )
import Control.DeepSeq (force)
import Scan ( ldf, sps, sequentialSPS, parLdfUBVec, parSpsUBVec )
import Sort ( batcherMergeSort, parBatcherMergeSort, defaultSort )

import qualified Data.Vector.Unboxed         as V

baseConfig :: Config
baseConfig = defaultConfig {
            resamples = 10
        }

setUpEnv :: IO (V.Vector Int, [Int], V.Vector Int, [Int])
setUpEnv = do
    let scanInpUV = force generateUVec 20
    let scanInpL  = force generateList 20
    let sortInpUV = force generateReverseUVec 20
    let sortInpL  = force generateReverseList 20
    return (scanInpUV, scanInpL, sortInpUV, sortInpL)

main :: IO ()
main = defaultMainWith baseConfig [
            env setUpEnv $ \ ~(scanInpUV, scanInpL, sortInpUV, sortInpL) -> bgroup "main" [
                bgroup "scan" [
                  bgroup "par" [
                    bgroup "128" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 128 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 128 20) scanInpUV
                    ],
                    bgroup "256" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 256 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 256 20) scanInpUV
                    ],
                    bgroup "512" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 512 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 512 20) scanInpUV
                    ],
                    bgroup "1024" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 1024 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 1024 20) scanInpUV
                    ],
                    bgroup "2048" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 2048 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 2048 20) scanInpUV
                    ],
                    bgroup "4096" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 4096 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 4096 20) scanInpUV
                    ],
                    bgroup "8192" [
                      bench "LDFUBVecPLPar" $ nf (parLdfUBVec (+) 8192 20) scanInpUV, 
                      bench "SPSUBVecPLPar" $ nf (parSpsUBVec (+) 8192 20) scanInpUV
                    ]
                  ],
                  bgroup "seq" [
                    bench "ldf" $ nf (ldf (+)) scanInpL,
                    bench "sps" $ nf (sps (+)) scanInpL,
                    bench "scanl1" $ nf (sequentialSPS (+)) scanInpL
                  ]
                ],
                bgroup "sort" [
                  bgroup "par" [
                    bench "BATCHER" $ nf (parBatcherMergeSort 20 ) sortInpUV
                  ],
                  bgroup "seq" [
                    bench "BATCHER" $ nf batcherMergeSort sortInpUV,
                    bench "DEFAULT" $ nf defaultSort sortInpL
                  ]
                ]
            ]
         ]