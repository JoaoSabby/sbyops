% Generated manually for the development snapshot.
\name{sby_select_correlation}
\alias{sby_select_correlation}
\title{Select columns by Pearson correlation}
\usage{
sby_select_correlation(.data, ..., threshold, num_treads = NULL)
}
\arguments{
\item{.data}{A data frame, tibble, or numeric-compatible matrix.}
\item{...}{Tidyselect column selectors. If empty, all numeric columns are evaluated.}
\item{threshold}{Numeric scalar between 0 and 1. Columns are removed when absolute Pearson correlation is greater than or equal to this value.}
\item{num_treads}{Optional positive integer scalar thread cap for this call. When supplied, it overrides \code{sby_config_max_threads} without changing the stored package configuration.}
}
\value{
\code{.data} with highly correlated selected columns removed.
}
\description{
Removes highly correlated numeric columns. In each pair at or above \code{threshold}, the function removes the column with the larger mean absolute correlation against the still-active candidate columns.
}
\details{
Engine selection is automatic using configured thresholds. During heavy execution paths (\code{fortran} and \code{blas}), the function applies a temporary thread context derived from \code{num_treads} when supplied, otherwise from \code{sby_config_max_threads}, and restores the original environment and R options on exit.
}
