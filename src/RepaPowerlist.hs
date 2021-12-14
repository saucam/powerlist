module RepaPowerlist where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Interleave
import Data.Array.Repa.Operators.IndexSpace

-- Using repa unboxed array here as it would be most performant
type PowerList b a = R.Array b R.DIM1 a


tie :: R.Source r1 a => PowerList r1 a -> PowerList r1 a -> PowerList D a
{-# INLINE tie #-}
tie = (Data.Array.Repa.Operators.IndexSpace.++)

zip :: R.Source r1 a => PowerList r1 a -> PowerList r1 a -> PowerList D a
{-# INLINE zip #-}
zip = interleave2

zipWith :: R.Source r1 a => (a -> a -> a) -> PowerList r1 a -> PowerList r1 a -> PowerList D a
{-# INLINE zipWith #-}
zipWith = Data.Array.Repa.Operators.Mapping.zipWith