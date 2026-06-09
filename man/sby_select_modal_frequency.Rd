% Generated manually for the development snapshot.
\name{sby_select_modal_frequency}
\alias{sby_select_modal_frequency}
\title{Select columns by modal frequency}
\usage{
sby_select_modal_frequency(.data, ..., threshold = 0.99)
}
\arguments{
\item{.data}{A data frame-like object.}
\item{...}{Tidyselect expressions for data-frame-like inputs. When omitted, all columns are considered.}
\item{threshold}{Numeric scalar threshold argument accepted by the public API.}
}
\value{
\code{.data} with columns retained according to the current modal-frequency implementation.
}
\description{
Filters selected columns with modal-frequency logic implemented directly in \R{}. Columns outside the tidyselect selection are retained without being evaluated by the modal-frequency algorithm.
}
\details{
This development snapshot keeps the current \code{sby_select_modal_frequency()} implementation as the source of truth and removes the previous native modal-frequency backend from the package.
}
