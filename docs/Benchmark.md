# Benchmarks
We list the benchmark of various supported algorithms here.
All benchmarks are performed on an 8 core (X2) Intel i9-9900K CPU @ 3.60GHz running Debian 11 (bullseye).
System has 32G of memory, and we specify wherever extra memory was supplied.

The output from the algorithms is the sum of the prefix sum array. This is to make sure everything was computed as expected.

## Scan

## Results Table
|Algo|Description|Array Size|Chunk Size|Num Cores|Time taken (s)|Threadscope Log|
|----|-----------|----------|----------|---------|--------------|---------------|
|SPS|Sequential scan using powerlist|2^20|-|1|5.357|-
|LDFPar|2^20|100|8|0.644|[LDFPar20CS100.eventlog](https://github.com/saucam/powerlist-threadscope/blob/main/LDFPar/LDFPar20CS100.eventlog)|

Check below for more details.

### Sequential Prefix sum
Simple prefix sum performs amazingly well on large arrays

### Sequential Prefix sum using powerlist
The sequential prefix sum using powerlists performs poorly as the array grows. It is expected since we introduce
recursion in an otherwise linear algorithm and generate a lot of intermediate lists which are GC'd.

Benchmarking results over a list of size 2^20:
```
$ stack exec powerlist-bench -- --match glob main/scan/seq/sps +RTS
benchmarking main/scan/seq/sps
time                 5.357 s    (4.897 s .. 5.518 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 5.367 s    (5.354 s .. 5.393 s)
std dev              70.26 ms   (58.53 ms .. 89.41 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Parallel Prefix sum using powerlist v1 (SPSPLPar1) 

### Parallel Prefix sum using powerlist v2 (SPSPLPar2)

### Parallel Prefix sum using powerlist v3 (SPSPLPar3)

### Sequential Prefix sum using Ladner Fischer algorithm (LDF)

### Parallel Prefix sum using Ladner Fischer algorithm (LDFPar)

1. Run for array of length 2^20, using chunk size 100, over 8 cores


```
time stack run -- scan --algo LDFPar --size 20 --csize 100 +RTS -N8 -ls
192154133857304576

real	0m0.644s
user	0m1.368s
sys	0m1.116s
```

![](LDFPar20CS100.png)

Load seems to be distributed evenly. There are GC pauses as expected as many intermediate lists are generated.