% Generated manually for the development snapshot.
\name{sby_select_correlation}
\alias{sby_select_correlation}
\title{Select columns by Pearson correlation}
\usage{
sby_select_correlation(.data, ..., threshold)
}
\arguments{
\item{.data}{A data.frame or tibble.}
\item{...}{Tidyselect column selectors. If empty, all numeric columns are evaluated.}
\item{threshold}{Numeric scalar between 0 and 1. Columns are removed when absolute Pearson correlation is greater than or equal to this value.}
}
\value{
\code{.data} with highly correlated selected columns removed.
}
\description{
Removes highly correlated numeric columns. In each pair at or above \code{threshold}, the function removes the column with the larger mean absolute correlation against the still-active candidate columns.
}
