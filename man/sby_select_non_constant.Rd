% Generated manually for the development snapshot.
\name{sby_select_non_constant}
\alias{sby_select_non_constant}
\title{Remove constant columns}
\usage{
sby_select_non_constant(.data, ...)
}
\arguments{
\item{.data}{A data frame, tibble, or matrix.}
\item{...}{Tidyselect expressions for data-frame-like inputs. When omitted, all columns are considered.}
}
\value{
An object with the same structural class as \code{.data} without selected constant columns.
}
\description{
Remove selected columns that contain a single repeated value across all observations.
}
\details{
The native C backend evaluates each selected column and supports column-parallel execution when OpenMP is available.
}
\examples{
constantData <- data.frame(a = c(1, 1, 1), b = c(1, 2, 3))
sby_select_non_constant(constantData)
}
\seealso{
\code{\link{sby_select_correlation}}, \code{\link{sby_select_modal_frequency}}
}
