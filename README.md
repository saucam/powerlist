# Powerlist

This is a benchmark utility for running and comparing different parallel algorithms with their sequential counterparts.
We primarily aim to show how a new data structure *Powerlist* can help in implementing naturally recursive and parallel versions of common algorithms.

J. Misra [[1]](#1), has introduced powerlist, a new recursive data structure that permits concise and elegant description of many data parallel algorithms like prefix-sum, Batcher’s sorting schemes, FFT etc. Powerlist is proposed
as a recursive data structure that is more suitable for describing parallel algorithms. Similar to a list structure,
the base case of powerlist is a list of one element. Longer power lists are constructed from the elements of 2
powerlists, p and q of the same length using 2 operators described below:

- <img src="https://render.githubusercontent.com/render/math?math=p\ |\ q"> is the powerlist formed by concatenating p and q.
- <img src="https://render.githubusercontent.com/render/math?math=p\ \bowtie\ q"> is the powerlist formed by successively taking alternate items from p and q, starting with p.

Further, both p and q are restricted to contain similar elements.

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
- ```powerlist-benchmark``` for benchmarking each of the algorithm functions by executing them multiple times. This uses [criterion](https://hackage.haskell.org/package/criterion) package, hence all command line options of criterion can be used.

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

So for example to run simple prefix sum algorithm using powerlist on array of input size ```2^5```:

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

## References
<a id="1">[1]</a>
J. Misra, “Powerlist: A structure for parallel recursion,” ACM Trans. Program. Lang. Syst., vol. 16,
p. 1737–1767, nov 1994.


