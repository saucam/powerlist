# powerlist

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

To run the parallel version of the same algorithm:

```
stack run -- scan --algo SPS --size 5 --parallel
```