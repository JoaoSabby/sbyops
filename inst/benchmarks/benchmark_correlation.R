#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(sbyops))

run_case <- function(n_rows, n_cols, threshold = 0.95) {
  set.seed(42)
  x <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
  if (n_cols >= 2) x[, 2] <- x[, 1] * 0.99 + x[, 2] * 0.01
  gc()
  t <- system.time(out <- sby_select_correlation(x, threshold = threshold))
  data.frame(
    n_rows = n_rows,
    n_cols = n_cols,
    elapsed = unname(t[["elapsed"]]),
    removed = n_cols - ncol(out)
  )
}

cases <- list(
  c(10000, 250),
  c(250000, 250),
  c(50000, 1000)
)

res <- do.call(rbind, lapply(cases, function(z) run_case(z[1], z[2])))
print(res)
cat("\nPara benchmark local pesado, executar manualmente cenários 1e6x250 e 4e6x250.\n")
