# Algorithms

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

  Read on for details.

## Scan

Scan algorithm is used to generate a prefix sum array from the input array, by summing all the elements of the array upto each element.
So for example the prefix sum for input array ```[1, 2, 3, 4]``` is given by ```[1, 3, 6, 10]``` (1 = 1, 3 = (1 + 2), 6 = (1 + 2 + 3), 10 = (1 + 2 + 3 + 4))

### SPS

The simplest sequential algorithm for scan runs through each elements, keeping track of the prefix sum of the previous element and adds the element to it to generate an output array element for each of the input element.
This is nothing but ```scanl1``` with ```(+)``` operator in haskell:

```haskell
Prelude> scanl1 (+) [1, 2, 3, 4]
[1,3,6,10]
```
### SPSPL

Powerlist 


