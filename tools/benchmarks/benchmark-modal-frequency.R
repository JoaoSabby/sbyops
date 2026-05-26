modal_ref <- function(df, threshold){
  keep <- vapply(df, function(x){
    if(!(is.logical(x)||is.integer(x)||is.factor(x)||is.numeric(x)||is.character(x))) return(TRUE)
    enc <- as.integer(factor(x, exclude = NULL))
    (max(tabulate(enc))/length(enc)) < threshold
  }, logical(1))
  df[, keep, drop = FALSE]
}

set.seed(1)
n <- 1e5
p <- 50
df <- data.frame(
  replicate(p/5, sample(c(TRUE,FALSE,NA), n, replace = TRUE, prob = c(0.98,0.01,0.01))),
  replicate(p/5, sample(c(1L,2L,NA_integer_), n, replace = TRUE, prob = c(0.98,0.01,0.01))),
  replicate(p/5, sample(c(1,2,NaN,NA), n, replace = TRUE, prob = c(0.96,0.02,0.01,0.01))),
  replicate(p/5, sample(c("a","b",NA), n, replace = TRUE, prob = c(0.98,0.01,0.01))),
  replicate(p/5, factor(sample(c("x","y",NA), n, replace = TRUE, prob = c(0.98,0.01,0.01))))
)

thr <- 0.95
print(system.time(r1 <- modal_ref(df, thr)))
print(system.time(r2 <- sby_select_modal_frequency(df, threshold = thr)))
stopifnot(identical(names(r1), names(r2)))
