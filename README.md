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

### Algorithms

We support many different algorithms of the following problems for benchmark and comparison.

- Prefix Sum (Scan)
- Sort

#### Scan algorithms
Following scan algorithms are supported:

- ```SPS``` : The sequential prefix sum, equivalent to scanl in haskell.
- ```SPSPL``` : A sequential prefix sum using powerlist, to demonstrate equivalence.
- ```SPSPLPar1```: A parallel implementation of ```SPSPL```, with the Eval Monad.
- ```SPSPLPar2```: More optimized parallel implementation of ```SPSPL```, with the Eval Monad.
- ```SPSPLPar3```: Only evaluate in parallel till certain depth, then fall back to scanl1

#### Sort algorithms
Following sort algorithms are supported:

- ```DEFAULT``` : This is the default sort implementation in haskell
- ```BATCHER``` : This is the batcher merge sort implementation using powerlists.

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

## References
<a id="1">[1]</a>
J. Misra, “Powerlist: A structure for parallel recursion,” ACM Trans. Program. Lang. Syst., vol. 16,
p. 1737–1767, nov 1994.


