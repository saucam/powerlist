module Scan where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Fusion.Bundle (inplace)

generateArray :: Int -> V.Vector Int
generateArray uB = V.fromList [1..uB]

sequentialSPS :: V.Vector Int -> V.Vector Int
sequentialSPS = V.scanl1 (+)

runSequentialSPS :: Int -> String
runSequentialSPS inp = show $ sequentialSPS $ generateArray inp