# sbyops

`sbyops` provides fast, pipe-friendly column selection operations for tabular preprocessing in R.

The current public API is intentionally small:

```r
preprocessed <- data |>
  sby_select_modal_frequency(threshold = 0.95) |>
  sby_select_correlation(threshold = 0.95)
```

## Modal frequency selection

`sby_select_modal_frequency()` removes selected columns whose most frequent value appears in a proportion greater than or equal to `threshold`.

```r
df <- data.frame(
  constant = c(1, 1, 1, 1, 1),
  near_constant = c("x", "x", "x", "x", "y"),
  variable = c(1, 2, 3, 4, 5)
)

sby_select_modal_frequency(df, threshold = 0.8)
```

When no selectors are supplied, all supported columns are evaluated. When selectors are supplied, only those columns are evaluated and all other columns are kept.

## Correlation selection

`sby_select_correlation()` removes highly correlated numeric columns using absolute Pearson correlation.

```r
df <- data.frame(
  x1 = 1:6,
  x2 = 2 * (1:6),
  x3 = c(6, 1, 5, 2, 4, 3),
  group = letters[1:6]
)

sby_select_correlation(df, threshold = 0.99)
```

When no selectors are supplied, all numeric columns are evaluated. Non-numeric columns are kept.

## Native cores

The heavy work is performed by Fortran/OpenMP native cores:

- modal-frequency counting uses encoded integer columns;
- Pearson correlation uses a native double-matrix kernel and handles non-finite values pairwise.
