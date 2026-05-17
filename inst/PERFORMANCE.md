# Performance notes

`sbyops` uses native Fortran/OpenMP cores for the heavy parts of the current column selection operations.

## Recommended environment variables

For large workloads, it can be useful to configure OpenMP before starting R:

```bash
export OMP_PROC_BIND=spread
export OMP_PLACES=cores
export OMP_DYNAMIC=false
```

If other numerical libraries are used in the same R session, avoid oversubscription by limiting their own thread pools when appropriate:

```bash
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export BLIS_NUM_THREADS=1
```

## Modal frequency

`sby_select_modal_frequency()` converts supported columns to compact integer codes in R and counts modal frequencies in Fortran. The native loop is parallelized by column.

## Correlation

`sby_select_correlation()` computes absolute Pearson correlations in Fortran. The first implementation uses a dense correlation matrix and includes an internal safety check before allocation. If too many selected columns would require an unsafe matrix allocation, the function stops before exhausting memory.

A future implementation should add a streaming/block correlation engine for very wide data.
