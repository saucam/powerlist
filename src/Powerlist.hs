module Powerlist where

import Control.Parallel.Strategies

-- Using simple list here as it would be most performant
type PowerList a = [a]

tie :: PowerList a -> PowerList a -> PowerList a
tie = (++)

zip :: PowerList a -> PowerList a -> PowerList a
zip [] [] = []
zip (x:xs) (y:ys) = x : y : Powerlist.zip xs ys
zip _ _ = error "Non similar powerlists"

parZip :: Strategy a -> Int -> PowerList a -> PowerList a -> PowerList a
parZip strategy cs as bs = Powerlist.zip as bs `using` parListChunk cs strategy

unzip :: PowerList a -> (PowerList a, PowerList a)
--unzip = snd . foldr (\x (b, (xs, ys)) -> (not b, if b then (x:xs, ys) else (xs, x:ys))) (False, ([], []))
unzip = Prelude.unzip . splt
  where splt []       = []
        splt (x:y:xs) = (x, y) : splt xs
        splt _        = error "Malformed powerlist"

-- Right shift and use zero
rsh :: (a, PowerList a) -> PowerList a
rsh (zero, xs) = zero : init xs