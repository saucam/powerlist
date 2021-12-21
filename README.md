# Powerlist

This is a benchmark utility for running and comparing different parallel algorithms with their sequential counterparts.
We primarily aim to show how a new data structure *Powerlist* can help in implementing naturally recursive and parallel versions of common algorithms.

J. Misra [[1]](#1), has introduced powerlist, a new recursive data structure that permits concise and elegant description of many data parallel algorithms like prefix-sum, Batcher’s sorting schemes, FFT etc. Powerlist is proposed
as a recursive data structure that is more suitable for describing parallel algorithms. Similar to a list structure,
the base case of powerlist is a list of one element. Longer power lists are constructed from the elements of 2
powerlists, p and q of the same length using 2 operators described below:

- <img src="https://render.githubusercontent.com/render/math?math=p\ |\ q"> is the powerlist formed by concatenating p and q.
- <img src="https://render.githubusercontent.com/render/math?math=p\ \bowtie\ q"> is the powerlist formed by successively taking alternate items from p and q, starting with p.

Hence the length of a powerlist is a power of 2. Further, both p and q are restricted to contain similar elements.
However it is possible to design algorithms that eliminate this constraint.

The ```Powerlist``` data structure has been implemented using 2 different data structures, ```List``` and ```Data.Vector.Unboxed```.

## Powerlist as List

The List implementation of powerlist is straightforward and allows to implement the required operators easily:

The ```tie``` function is same as ```++``` of List in haskell, but ```zip``` is a bit different which is shown below:

```haskell
-- Using simple list here as it would be most performant
type PowerList a = [a]

tie :: PowerList a -> PowerList a -> PowerList a
{-# INLINE tie #-}
tie = (++)

zip :: PowerList a -> PowerList a -> PowerList a
{-# INLINE zip #-}
zip [] [] = []
zip xs ys = Prelude.zip xs ys >>= \(a, b) -> [a, b]
```

There is an analogous ```unzip``` function that is required for dividing the input powerlist into 2 different powerlists.

```haskell
unzip :: PowerList a -> (PowerList a, PowerList a)
unzip = snd . foldr (\x (b, (xs, ys)) -> (not b, if b then (x:xs, ys) else (xs, x:ys))) (False, ([], []))
```

We also need another function right shift which we implement as shown:

```haskell
-- Right shift and use zero
rsh :: a -> PowerList a -> PowerList a
rsh zero xs = zero : init xs
```
## Powerlist as Unboxed Vector

It was quickly clear from experiments that powerlists implemented as list can only scale so much for algorithms like prefix sum, since the recursive nature of algorithm leads to too many intermediate list creation and GC cycles, that slow down the execution on large inputs.
Hence, we resort to using Unboxed Vectors which are more memory friendly. We also use mutable vectors to further decrease memory usage.

But the implementation of operation in terms of Vectors is a bit more involved:

```haskell
type PowerList a = V.Vector a

tie :: V.Unbox a => PowerList a -> PowerList a -> PowerList a
{-# INLINE tie #-}
tie = (V.++)

zip ::  (V.Unbox a, Num a) => PowerList a -> PowerList a -> PowerList a
{-# INLINE zip #-}
--zip xs ys = V.generate (V.length xs + V.length ys) (\i -> if even i then xs V.! (i `div` 2) else ys V.! (i `div` 2))
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
              inp <- rseq $ Prelude.zip ac bc
              lists <- parList strategy (writePar <$> inp)
              rdeepseq $ V.concat lists
              where
                 ac = S.chunksOf cs as
                 bc = S.chunksOf cs bs
                 writePar (a, b) = UBVecPowerlist.zip a b


zipWith :: (Num a, V.Unbox a) => (a -> a -> a) -> PowerList a -> PowerList a -> PowerList a
zipWith op xs ys = V.create $ do
  m <- V.thaw xs
  write m ys 0
  return m
  where
    k = V.length xs
    write m y i
         | i < k = do
           curr <- M.unsafeRead m i
           M.unsafeWrite m i (op (y V.! i) curr)
           write m y (i+1)
         | otherwise = return ()

parZipWith :: (Num a,  V.Unbox a) => Strategy (PowerList a) -> (a -> a -> a) -> Int -> PowerList a -> PowerList a -> Eval (PowerList a)
parZipWith strategy op cs as bs = do
              inp <- rseq $ Prelude.zip ac bc
              lists <- parList strategy (writePar <$> inp)
              rdeepseq $ V.concat lists
              where
                 ac = S.chunksOf cs as
                 bc = S.chunksOf cs bs
                 writePar (a, b) = UBVecPowerlist.zipWith op a b

unzip ::  V.Unbox a => PowerList a -> (PowerList a, PowerList a)
unzip k = (b, c)
  where b = V.ifilter (\i _ -> even i) k
        c = V.ifilter (\i _ -> odd i) k
```

Operators like ```zipWith``` are highly optimized now, as we end up creating at-most a single copy of the input vectors.

Read on for the list of supported algorithms.

## Algorithms

We support many different algorithms of the following problems for benchmark and comparison.

- Prefix Sum (Scan)
  - ```SPS```                : The sequential prefix sum, which is nothing but scanl1 in haskell.
  - ```SPSPL```              : A sequential prefix sum using powerlist, to demonstrate equivalence.
  - ```SPSPLPar1```          : A parallel implementation of ```SPSPL```, with the Eval Monad, first attempt.
  - ```SPSPLPar2```          : More optimized parallel implementation of ```SPSPL```, with the Eval Monad.
  - ```SPSPLPar3```          : Only evaluate in parallel till certain depth, then fall back to scanl1.
  - ```LDF```                : The sequential Ladner Fischer algorithm implementation using powerlist.
  - ```LDFPar```             : A parallel implementation of ```LDF``` using eval monad.
  - ```SPSUBVecPLPar```      : An implementation of ```SPSPLPar3``` using Unboxed Vectors as powerlists, with more optimizations.
  - ```LDFUBVecPLPar```      : An implementation of ```LDFPar``` using Unboxed Vectors as powerlists, with more optimizations.
  - ```LDFChunkUBVecPLPar``` : An implementation of ```LDFUBVecPLPar``` that creates chunks of input and applies ```LDF``` scheme to each chunk in parallel. This is a hybrid of ```LDF``` and an algorithm due to Bleloch [[2]](#2).

- Sort
  - ```DEFAULT```            : Default sort in haskell.
  - ```BATCHER```            : A parallel implementation of the Batcher merge sort algorithm using powerlists.

Read more about the algorithm details [here](docs/Algorithms.md).

### Building

To build the executables, simply run

```
stack build

```
This builds 2 different executables:

- ```powerlist-exe``` for executing and analyzing each of the different algorithms.
- ```powerlist-benchmark`` for benchmarking each of the algorithm functions by executing them multiple times. This uses [criterion](https://hackage.haskell.org/package/criterion) package, hence all command line options of criterion can be used.

### Running

To run each of the algorithms, use the ```powerlist-exe``` executable, which supports 2 commands ```scan``` and ```sort```:

```
$ stack exec powerlist-exe -- scan
Usage: powerlist-exe scan (-a|--algo ALGONAME) (-s|--size INPSIZE) 
                          [-c|--csize CHUNKSIZE]
  Run Scan Algorithm

Available options:
  -a,--algo ALGONAME       Supported Algos:
                           [SPS,SPSPL,SPSPLPar1,SPSPLPar2,SPSPLPar3,LDF,LDFPar,SPSUBVecPLPar,LDFUBVecPLPar,LDFChunkUBVecPLPar]
  -s,--size INPSIZE        Size of array in terms of powers of 2 on which to run
                           scan
  -c,--csize CHUNKSIZE     Size of chunks for parallelization
  -h,--help                Show this help text
```
and
```
$ stack exec powerlist-exe -- sort
Usage: powerlist-exe sort (-a|--algo ALGONAME) (-s|--size INPSIZE) 
                          [-c|--csize CHUNKSIZE]
  Run Sort Algorithm

Available options:
  -a,--algo ALGONAME       Supported Algos: [DEFAULT,BATCHER]
  -s,--size INPSIZE        Size of array in terms of powers of 2 on which to run
                           sort
  -c,--csize CHUNKSIZE     Size of chunks for parallelization
  -h,--help                Show this help text

```

So for example to run simple prefix sum algorithm using powerlist on array of input size 2^5:

```
stack exec powerlist-exe -- scan --algo SPSPL --size 5
```

To run the parallel version of the same algorithm using powerlist, on 8 cores and generate eventlog file for threadscope analysis:

```
stack exec powerlist-exe -- scan --algo SPSPLPar1 --size 20 +RTS -N8 -ls
```
To run the more optimized parallel version, and supply (optional) chunk size for splitting methods like zip etc:

```
stack exec powerlist-exe -- scan --algo SPSPLPar2 --size 20 --csize 256 +RTS -N8 -ls
```

### Benchmarks

Find the [benchmarks here](docs/Benchmark.md)

## Project Report

View the complete [project report here](docs/project_report.pdf)

## References
<a id="1">[1]</a>
J. Misra, “Powerlist: A structure for parallel recursion,” ACM Trans. Program. Lang. Syst., vol. 16,
p. 1737–1767, nov 1994.


