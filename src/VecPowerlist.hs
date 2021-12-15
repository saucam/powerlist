module VecPowerlist where

import Control.Parallel.Strategies
import Data.Vector.Strategies

import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as M
import qualified Data.Vector.Split           as S

-- Using simple list here as it would be most performant
type PowerList a = V.Vector a

tie :: PowerList a -> PowerList a -> PowerList a
{-# INLINE tie #-}
tie = (V.++)

zip :: PowerList a -> PowerList a -> PowerList a
{-# INLINE zip #-}
zip xs ys = V.generate (V.length xs + V.length ys) (\i -> if even i then xs V.! (i `div` 2) else ys V.! (i `div` 2))
--zip _ _ = error "Non similar powerlists"

parZip :: Strategy (PowerList a) -> Int -> PowerList a -> PowerList a -> PowerList a
{-# INLINE parZip #-}
parZip strategy cs as bs = VecPowerlist.zip as bs `using` (parVector cs strategy)

zipWith :: Num a => (a -> a -> a) -> PowerList a -> PowerList a -> PowerList a
{-# INLINE zipWith #-}
zipWith = V.zipWith

parZipWith :: Num a => Strategy (PowerList a) -> Int -> (a -> a -> a) -> PowerList a -> PowerList a -> PowerList a
{-# INLINE parZipWith #-}
parZipWith strategy cs z as bs = VecPowerlist.zipWith z as bs `using` (parVector cs strategy)

unzip :: PowerList a -> (PowerList a, PowerList a) 
unzip k = (b, c)
  where b = V.ifilter (\i a -> even i) k
        c = V.ifilter (\i a -> odd i) k

-- Right shift and use zero, does not perform well as cons is O(n)
rsh :: a -> PowerList a -> PowerList a
{-# INLINE rsh #-}
rsh zero xs = V.cons zero $ V.init xs

shiftAdd :: Num a => PowerList a -> PowerList a
shiftAdd l = V.create $ do
    m <- V.thaw l
    go (V.length l -1) m
    return m
    where go id l
            | id > 0 = do
              prev <- M.unsafeRead l (id - 1)
              curr <- M.unsafeRead l id 
              M.unsafeWrite l id (prev + curr)
              go (id-1) l
            | otherwise = return ()

shiftAdd2 :: Num a => PowerList a -> PowerList a -> PowerList a
shiftAdd2 r l = V.create $ do
    m <- V.thaw l
    go (V.length l -1) m
    return m
    where go id l
            | id > 0 = do
              curr <- M.unsafeRead l id 
              M.unsafeWrite l id ((r V.! (id - 1)) + curr)
              go (id-1) l
            | otherwise = return ()

addPairs :: Num a => PowerList a -> PowerList a
addPairs l = V.create $ do
  m <- M.new n
  addPairs' l m 0
  return m
  where
    n = V.length l `div` 2
    addPairs' l m i
             | i < n = do
               M.unsafeWrite m i (l V.! (2*i) + (l V.! (2*i + 1)))
               addPairs' l m (i+1)
             | otherwise = return ()  