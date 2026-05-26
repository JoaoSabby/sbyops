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
\item{sby_config_start_blas}{Integer threshold where BLAS with OpenMP threading is enabled.}
\item{sby_config_max_threads}{Integer positive scalar thread cap used by native backends.}
}
\value{
A named list with validated configuration values.
}
\description{
Configure backend thresholds and thread settings used by automatic correlation execution planning.
}
\details{
This function stores runtime options consumed by internal dispatch logic. Values are validated as positive integer scalars.
}
\examples{
# Apply default configuration values explicitly
sby_config(
  sby_config_start_fortran = 10000L,
  sby_config_start_blas = 100000L,
  sby_config_max_threads = 2L
)
}
