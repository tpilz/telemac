% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results.R
\name{read_results}
\alias{read_results}
\title{Read results from *.slf}
\usage{
read_results(fname, vars = "all", times = NULL, return_datetime = FALSE)
}
\arguments{
\item{fname}{\code{character} File name of the results file to be read.}

\item{vars}{Selection of variables to be read from the file: either \code{"all"}
(default) reading all variables, \code{"none"} giving only the header, a character
vector of variable names, or a numeric vector of positions.}

\item{times}{\code{integer} vector, the timesteps to be read. Passed to \code{\link{read_slf_header}}.}

\item{return_datetime}{\code{logical}, return timesteps as datetime (\code{\link{POSIXct}}) object?}
}
\value{
A \code{list} with \code{header} (see output of \code{\link{read_slf_header}})
and \code{values}, which is a tidy \code{data.frame} where each line represents the value for a
certain mesh point (with coordinates x and y) at a certain simulation timestep (note
that this might be difficult to interpret if you used variable timestep lengths) for a
specific variable.
}
\description{
Reads the results of a TELEMAC simulation from a SELAFIN (*.slf) file.
}
