module VecPowerlist where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

-- Using simple list here as it would be most performant
type PowerList a = V.Vector a

tie :: PowerList a -> PowerList a -> PowerList a
{-# INLINE tie #-}
tie = (V.++)

zip :: PowerList a -> PowerList a -> PowerList a
{-# INLINE zip #-}
zip xs ys =
  V.generate
    (V.length xs + V.length ys)
    (\i ->
       if even i
         then xs V.! (i `div` 2)
         else ys V.! (i `div` 2))

--zip _ _ = error "Non similar powerlists"
zipWith :: Num a => (a -> a -> a) -> PowerList a -> PowerList a -> PowerList a
{-# INLINE zipWith #-}
zipWith = V.zipWith

unzip :: PowerList a -> (PowerList a, PowerList a)
unzip k = (b, c)
  where
    b = V.ifilter (\i _ -> even i) k
    c = V.ifilter (\i _ -> odd i) k

-- Right shift and use zero, does not perform well as cons is O(n)
rsh :: a -> PowerList a -> PowerList a
{-# INLINE rsh #-}
rsh zero xs = V.cons zero $ V.init xs

shiftAdd :: Num a => PowerList a -> PowerList a
shiftAdd l =
  V.create $ do
    m <- V.thaw l
    go (V.length l - 1) m
    return m
  where
    go i mv
      | i > 0 = do
        prev <- M.unsafeRead mv (i - 1)
        curr <- M.unsafeRead mv i
        M.unsafeWrite mv i (prev + curr)
        go (i - 1) mv
      | otherwise = return ()

shiftAdd2 :: Num a => PowerList a -> PowerList a -> PowerList a
shiftAdd2 r l =
  V.create $ do
    m <- V.thaw l
    go (V.length l - 1) m
    return m
  where
    go i mv
      | i > 0 = do
        curr <- M.unsafeRead mv i
        M.unsafeWrite mv i ((r V.! (i - 1)) + curr)
        go (i - 1) mv
      | otherwise = return ()

addPairs :: Num a => PowerList a -> PowerList a
addPairs l =
  V.create $ do
    m <- M.new n
    addPairs' m 0
    return m
  where
    n = V.length l `div` 2
    addPairs' mv i
      | i < n = do
        M.unsafeWrite mv i (l V.! (2 * i) + (l V.! (2 * i + 1)))
        addPairs' mv (i + 1)
      | otherwise = return ()