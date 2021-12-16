{-# LANGUAGE FlexibleContexts #-}
module UBVecPowerlist where

import Control.Parallel.Strategies

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Split           as S

-- Using simple list here as it would be most performant
type PowerList a = V.Vector a

tie :: V.Unbox a => PowerList a -> PowerList a -> PowerList a
{-# INLINE tie #-}
tie = (V.++)

zip ::  (V.Unbox a, Num a) => PowerList a -> PowerList a -> PowerList a
{-# INLINE zip #-}
--zip xs ys = V.generate (V.length xs + V.length ys) (\i -> if even i then xs V.! (i `div` 2) else ys V.! (i `div` 2))
--zip _ _ = error "Non similar powerlists"

zip xs ys = V.create $ do
  m <- M.new n
  write m 0
  return m
  where
    n = V.length xs + V.length ys
    write m i
         | i < n = do
           M.unsafeWrite m i (xs V.! (i `div` 2))
           M.unsafeWrite m (i+1) (ys V.! (i `div` 2))
           write m (i+2)
         | otherwise = return ()

parZip ::  (V.Unbox a, Num a) => Strategy (PowerList a) -> Int -> PowerList a -> PowerList a -> Eval (PowerList a)
{-# INLINE parZip #-}
parZip strategy cs as bs = do
              let inp = Prelude.zip ac bc
              lists <- parList strategy (writePar <$> inp)
              return $ V.concat lists
              where
                 ac = S.chunksOf cs as
                 bc = S.chunksOf cs bs
                 numChunks = V.length as `div` cs
                 writePar (a, b) = UBVecPowerlist.zip a b


zipWith :: (Num a, V.Unbox a) => (a -> a -> a) -> PowerList a -> PowerList a -> PowerList a
{-# INLINE zipWith #-}
zipWith op xs ys = V.create $ do
  m <- V.thaw xs
  write m ys 0
  return m
  where
    k = V.length xs
    write m ys i
         | i < k = do
           curr <- M.unsafeRead m i
           M.unsafeWrite m i (op (ys V.! i) curr)
           write m ys (i+1)
         | otherwise = return ()

parZipWith :: (Num a,  V.Unbox a) => Strategy (PowerList a) -> (a -> a -> a) -> Int -> PowerList a -> PowerList a -> Eval (PowerList a)
{-# INLINE parZipWith #-}
parZipWith strategy op cs as bs = do
              let inp = Prelude.zip ac bc
              lists <- parList strategy (writePar <$> inp)
              return $ V.concat lists
              where
                 ac = S.chunksOf cs as
                 bc = S.chunksOf cs bs
                 numChunks = V.length as `div` cs
                 writePar (a, b) = UBVecPowerlist.zipWith op a b

unzip ::  V.Unbox a => PowerList a -> (PowerList a, PowerList a) 
unzip k = (b, c)
  where b = V.ifilter (\i a -> even i) k
        c = V.ifilter (\i a -> odd i) k

-- Right shift and use zero, does not perform well as cons is O(n)
rsh ::  V.Unbox a => a -> PowerList a -> PowerList a
{-# INLINE rsh #-}
rsh zero xs = V.cons zero $ V.init xs

shiftAdd :: (V.Unbox a, Num a) => PowerList a -> PowerList a
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

shiftAdd2 :: (V.Unbox a, Num a) => PowerList a -> PowerList a -> PowerList a
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

addPairs :: (V.Unbox a, Num a) => PowerList a -> PowerList a
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

minMaxZip ::  (V.Unbox a, Ord a) => PowerList a -> PowerList a -> PowerList a
minMaxZip xs ys = V.create $ do
  m <- M.new n
  write m 0
  return m
  where
    n = V.length xs + V.length ys
    write m i
         | i < n = do
           let p = xs V.! (i `div` 2)
           let q = ys V.! (i `div` 2)
           M.unsafeWrite m i (p `min` q)
           M.unsafeWrite m (i+1) (p `max` q)
           write m (i+2)
         | otherwise = return ()

parMinMaxZip ::  (V.Unbox a, Ord a) => Strategy (PowerList a) -> Int -> PowerList a -> PowerList a -> Eval (PowerList a)
{-# INLINE parMinMaxZip #-}
parMinMaxZip strategy cs as bs = do
              let inp = Prelude.zip ac bc
              lists <- parList strategy (writePar <$> inp)
              return $ V.concat lists
              where
                 ac = S.chunksOf cs as
                 bc = S.chunksOf cs bs
                 numChunks = V.length as `div` cs
                 writePar (a, b) = UBVecPowerlist.minMaxZip a b