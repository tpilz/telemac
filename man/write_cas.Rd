% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/steering.R
\name{write_cas}
\alias{write_cas}
\alias{write_cas.data.frame}
\alias{write_cas.list}
\alias{write_cas.t2d_cas}
\alias{write_cas.t2d}
\title{Write steering file (*.cas)}
\usage{
write_cas(x, ...)

\method{write_cas}{data.frame}(x, fname, ...)

\method{write_cas}{list}(x, fname, ...)

\method{write_cas}{t2d_cas}(x, ...)

\method{write_cas}{t2d}(x, ...)
}
\arguments{
\item{x}{Either: An object of class \code{t2d_cas};
a \code{list} with elements as in a \code{t2d_cas} object;
a \code{data.frame} with columns \code{key} and \code{value} giving the
steering parameters;
an object of class \code{t2d} with element \code{cas}.}

\item{...}{Arguments passed to or from other methods.}

\item{fname}{\code{character}, a file name (extension .cas) where the steering
parameters should be written to.}
}
\value{
Returns input \code{x} invisibly.
}
\description{
Writes the steering file for a TELEMAC project.
}
\note{
An existing steering file will be silently overwritten.
}
\examples{
\dontrun{
# creates test.cas in current working directory
cas_tpl <- cas()
cas_tpl <- cas(cas_tpl, fname = "test.cas")
write_cas(cas_tpl)
}
}
