% Generated manually for the development snapshot.
\name{sby_config}
\alias{sby_config}
\title{Configure sbyops execution thresholds}
\usage{
sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)
}
\arguments{
\item{sby_config_start_fortran}{Integer threshold where automatic execution switches from streaming to Fortran backend.}
\item{sby_config_start_blas}{Integer threshold where BLAS execution is enabled.}
\item{sby_config_max_threads}{Integer positive scalar thread cap used by runtime thread contexts.}
}
\value{
A named list with validated configuration values.
}
\description{
Configure backend thresholds and thread limit for automatic execution planning.
}
\details{
This function stores runtime options consumed by internal dispatch logic. \code{sby_config_max_threads} is the central thread cap used by runtime thread contexts.
}
\examples{
# Apply default configuration values explicitly
sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)
}
