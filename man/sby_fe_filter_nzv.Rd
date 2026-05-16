% Generated manually from roxygen comments until roxygen2 is run.
\name{sby_fe_filter_nzv}
\alias{sby_fe_filter_nzv}
\title{Detect columns with low variability by modal frequency}
\usage{
sby_fe_filter_nzv(data, threshold, n_threads = NULL)
}
\arguments{
\item{data}{A \code{data.frame} or tibble. Each column must be \code{factor},
\code{character}, \code{integer}, \code{logical}, or \code{numeric}.}

\item{threshold}{Numeric scalar between 0 and 1. It is the minimum relative
frequency of the modal value required for a column to be returned.}

\item{n_threads}{Optional positive integer scalar. Number of OpenMP threads
requested by the Fortran core. When \code{NULL}, the OpenMP runtime uses the
environment default, such as \code{OMP_NUM_THREADS}. When supplied, the native
core calls \code{omp_set_num_threads()}; depending on the OpenMP runtime, this
setting may persist for later calls in the same R process.}
}
\value{
An object of class \code{sby_fe_filter_nzv_result}, backed by a tibble when the
\pkg{tibble} package is installed or by a base \code{data.frame} otherwise.

The returned table has one row per filtered column and these columns:
\itemize{
  \item \code{column}: name of the filtered column;
  \item \code{ratio}: relative frequency of the dominant value;
  \item \code{value}: dominant value represented as text;
  \item \code{count}: absolute frequency of the dominant value.
}

The object also stores global metadata in attributes: \code{n_rows},
\code{n_cols}, \code{threshold}, and \code{n_threads}.
}
\description{
\code{sby_fe_filter_nzv()} identifies columns where one content value appears in
a proportion greater than or equal to \code{threshold}. The acronym NZV refers to
near-zero variance; in this package, low variability is operationalized as modal
frequency dominance.
}
\details{
For each column, the function computes:

\deqn{ratio = frequency\_of\_most\_common\_value / number\_of\_rows}

A column is returned when:

\deqn{ratio >= threshold}

\code{NA} values are counted as content. Thus, a column with 95 percent
\code{NA} has \code{NA} as its modal value and is returned when
\code{threshold <= 0.95}.

Numeric columns are compared by exact equality after coding with
\code{factor(..., exclude = NULL)}; no numeric tolerance or rounding is applied.

The R layer validates inputs and converts every column to compact positive
integer codes. The Fortran core receives only those codes and computes the modal
code, modal count, and modal ratio by direct counting. The original modal value
is reconstructed in R after the native call.
}
\examples{
df <- data.frame(
  a = c(0, 0, 0, 0, 1),
  b = c("x", "x", "x", "x", "y"),
  c = c(1, 2, 3, 4, 5),
  d = c(NA, NA, NA, NA, 10)
)

sby_fe_filter_nzv(df, threshold = 0.8)

sby_fe_filter_nzv(df, threshold = 0.95, n_threads = 1)
}
