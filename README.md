# Powerlist

This is a benchmark utility for running and comparing different parallel algorithms with the sequential versions.
We primarily aim to show how a new data structure *Powerlist* can help in implementing naturally recursive and parallel versions of common algorithms.
Read on for the list of supported algorithms.

### Algorithms

We support many different algorithms of the following problems for benchmark and comparison.

- Prefix Sum (Scan)

#### Scan algorithms
Following scan algorithms are supported:

- ```SPS``` : The sequential prefix sum, equivalent to scanl in haskell.
- ```SPSPL``` : A sequential prefix sum using powerlist, to demonstrate equivalence.
- ```SPSPLPar1```: A parallel implementation of ```SPSPL```, with the Eval Monad.
- ```SPSPLPar2```: More optimized parallel implementation of ```SPSPL```, with the Eval Monad.

### Running via stack

To run scan algorithm

```
stack run -- scan
```
shows the available options

```
Usage: powerlist-exe scan (-a|--algo K) (-s|--size R)
  Run Scan Algorithm

Available options:
  -a,--algo K              Supported Algos: SPS, LDF
  -s,--size R              Size of array on which to run scan
  -h,--help                Show this help text
```

To run simple prefix sum algorithm on an array of size 2^5 in sequential mode:

```
stack run -- scan --algo SPS --size 5
```

To run the parallel version of the same algorithm using powerlist:

```
stack run -- scan --algo SPSPLPar1 --size 5
```
To run the more optimized parallel version, and supply chunk size for splitting zip

```
stack run -- scan --algo SPSPLPar2 --size 20 --csize 100
```

### Benchmarks

Find the [benchmarks here](docs/Benchmark.md)


