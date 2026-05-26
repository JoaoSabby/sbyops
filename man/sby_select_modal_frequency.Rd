% Generated manually for the development snapshot.
\name{sby_select_modal_frequency}
\alias{sby_select_modal_frequency}
\title{Select columns by modal frequency}
\usage{
sby_select_modal_frequency(.data, ..., threshold)
}
\arguments{
\item{.data}{A data frame, tibble, or matrix.}
\item{...}{Tidyselect column selectors. If empty, all supported columns are evaluated.}
\item{threshold}{Numeric scalar between 0 and 1. Columns are removed when the modal value ratio is greater than or equal to this value.}
}
\value{
\code{.data} with high modal-frequency columns removed.
}
\description{
Removes selected columns whose most frequent value appears in a proportion greater than or equal to \code{threshold}.
}
\details{
The native routine receives an explicit \code{max_threads} argument from \code{sby_config_max_threads} and runs inside a temporary OpenMP thread context that is restored on exit.
}
