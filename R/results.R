# internal constructor for a t2d_res object
new_res <- function(data = NULL, log = NULL, fname = "results.slf", lname = "<unspecified>") {
  if (is.null(data)) {
    res_dat <- list(header = NULL, tin = NULL, log = NULL, values = NULL)
  } else {
    stopifnot(is.character(fname) && length(fname) == 1)
    stopifnot(inherits(data, "list"))
    stopifnot(all(c("header", "tin", "values") %in% names(data)))

    if (is.null(data$tin))
      data$tin <- tin.matrix(cbind(data$header$x, data$header$y), ikle = data$header$ikle, ipobo = data$header$ipobo)

    res_dat <- list(
      header = list(title = data$header$title,
                    precision = data$header$precision,
                    nelem = data$header$nelem,
                    npoin = data$header$npoin,
                    ndp = data$header$ndp,
                    varnames = data$header$varnames,
                    varunits = data$header$varunits,
                    date = data$header$date,
                    ntimes = data$header$ntimes),
      tin = data$tin,
      log = log,
      values = data$values)
  }
  structure(
    res_dat,
    file = fname,
    log = lname,
    class = c("t2d_res", "list")
  )
}

# internal validator for a t2d_res object
validate_res <- function(x) {
  stopifnot(inherits(x, c("t2d_res", "list")))
  stopifnot(all(c("header", "tin", "log", "values") %in% names(x)))
  if (!is.null(x$header)) {
    stopifnot(inherits(x$header, "list"))
    stopifnot(all(c("title", "precision", "nelem", "npoin", "ndp", "varnames", "varunits", "date") %in% names(x$header)))
  }
  if (!is.null(x$tin)) {
    stopifnot(inherits(x$tin, "t2d_tin"))
    x$tin <- validate_tin(x$tin)
  }
  if (!is.null(x$values)) {
    stopifnot(inherits(x$values, "data.frame"))
    stopifnot(all(c("x", "y", "timestep", "variable", "value") %in% colnames(x$values)))
  }
  stopifnot(all(c("file", "log") %in% names(attributes(x))))

  x
}

#' Results  object
#'
#' Initialise a results object to handle the results of TELEMAC-2D runs.
#'
#' @param x Either: \code{NULL} (default), in which case a simple template without data will be generated;
#'    a \code{character} string providing the name of an existing results file;
#'    an object of class \code{t2d_res} (modify attributes);
#'    an object of class \code{t2d} (read data after model run).
#' @param fname \code{character}, name for the associated results file (can also be used to replace an existing entry).
#' @param ... Arguments passed to or from other methods, e.g. to \code{\link{read_results}}
#' such as arguments \code{vars}, \code{times}, or \code{return_datetime}.
#' @return An object of class \code{t2d_res} consisting of an attribute \code{file}
#' pointing to a specific results file, an attribute \code{log} pointing to the log
#' of a simulation run, and a \code{list} with elements
#' \describe{
#'   \item{header}{General information including title, precision (of numbers in
#'     the slf file in bytes), the numbers of mesh elements and points, the number
#'     of points per mesh (3 in case of triangles which is the only supported value for now),
#'     the variable names and units, and the simulation start date.}
#'   \item{tin}{An object of class \code{t2d_tin} representing the underlying mesh.}
#'   \item{log}{The log messages (a \code{character} vector).}
#'   \item{values}{A \code{data.frame} where each line represents the value for a
#'     certain mesh point (with coordinates x and y) at a certain simulation timestep (note
#'     that this might be difficult to interpret if you used variable timestep lengths) for a
#'     specific variable.}
#' }
#' @note Also note the associated \code{\link{plot.t2d_res}} method.
#' @export
results <- function(x, ...) UseMethod("results")

#' @name results
#' @export
results.default <- function(x = NULL, fname = "results.slf", ...) {
  validate_res(new_res(NULL, fname = fname))
}

#' @param log File name of a log file from a simulation run, if it exists.
#' @name results
#' @export
results.character <- function(x, fname = NULL, log = NULL, ...) {
  data <- read_results(x, ...)
  logdata <- NULL
  if (is.null(log)) {
    log <- "<unspecified>"
  } else {
    logdata <- readLines(log)
  }
  if (is.null(fname)) fname <- x
  validate_res(new_res(c(data, tin = list(NULL)), logdata, fname, log))
}

#' @name results
#' @export
results.t2d_res <- function(x, fname = NULL, log = NULL, ...) {
  x <- validate_res(x)

  if (is.null(fname)) {
    fname <- attr(x, "file")
  }

  if (!is.null(log)) {
    logdata <- readLines(log)
  } else {
    log <- attr(x, "log")
    logdata <- x$log
  }

  data <- list(header = x$header, tin = x$tin, values = x$values)

  validate_res(new_res(data, logdata, fname, log))
}

#' @name results
#' @export
results.t2d <- function(x, ...) {
  if (attr(x$res, "log") != "<unspecified>")
    logf <- paste(x$wdir, attr(x$res, "log"), sep = "/")
  else
    logf <- NULL
  results.character(paste(x$wdir, attr(x$res, "file"), sep = "/"),
                    fname = attr(x$res, "file"), log = logf, ...)
}


#' @name results
#' @param n Maximum number of steering parameters to print.
#' @export
print.t2d_res <- function(x, ..., n = 10) {
  cat("Object of class t2d_res: TELEMAC simulation results\n")
  cat("Associated results file:", attr(x, "file"), "\n")
  if (is.null(x$header)) {
    cat("No results available yet (otherwise use results() to import them).")
  } else {
    cat("Associated log file:", attr(x, "log"), "\n")
    cat("Simulation title:", x$header$title, "\n")
    cat("The results comprise the following", length(x$header$varnames), "variables (units):\n")
    cat(paste0(x$header$varnames, " (", x$header$varunits, ")"), sep = ", ")
    cat("\n")
    cat("over", x$header$ntimes, "timesteps.\n")
    if (is.null(x$values)) {
      cat("No results imported yet.")
    } else {
      times <- unique(x$values$timestep)
      vars <- unique(x$values$variable)
      if (length(vars) == length(x$header$varnames) && length(times) == x$header$ntimes) {
        cat("All data imported as a tidy data.frame.\n")
      } else {
        if (length(times) <= 2)
          cat("Imported", length(times), "timesteps (",  paste(as.character(times), collapse = ", "), ") and ")
        else if (!inherits(times, "POSIXct") && length(times) <= 10)
          cat("Imported", length(times), "timesteps (", paste(times, collapse = ", "), ") and ")
        else
          cat("Imported", length(times), "timesteps (", as.character(min(times)), ", ...,", as.character(max(times)), ") and ")
        if (length(vars) == 1)
          cat("variable", as.character(vars), "as a tidy data.frame.\n")
        else
          cat("variables", paste(vars, collapse = ", "), "as a tidy data.frame.\n")
      }
      if (n > 0) {
        if (inherits(x$values, "tbl_df"))
          NextMethod()
        else {
          y <- x$values
          if (nrow(y) > n) {
            cat(paste("First", n, "of", nrow(y), "values:\n"))
            y <- y[1:n, , drop = FALSE]
          }
          print.data.frame(y, ...)
        }
      }
    }
  }

  invisible(x)
}


#' Read results from *.slf
#'
#' Reads the results of a TELEMAC simulation from a SELAFIN (*.slf) file.
#'
#' @param fname \code{character} File name of the results file to be read.
#' @param vars Selection of variables to be read from the file: either \code{"all"}
#' (default) reading all variables, \code{"none"} giving only the header, a character
#' vector of variable names, or a numeric vector of positions.
#' @param times \code{integer} vector, the timesteps to be read. Passed to \code{\link{read_slf_header}}.
#' @param return_datetime \code{logical}, return timesteps as datetime (\code{\link{POSIXct}}) object?
#' @return A \code{list} with \code{header} (see output of \code{\link{read_slf_header}})
#' and \code{values}, which is a tidy \code{data.frame} where each line represents the value for a
#' certain mesh point (with coordinates x and y) at a certain simulation timestep (note
#' that this might be difficult to interpret if you used variable timestep lengths) for a
#' specific variable.
#' @export
read_results <- function(fname, vars = "all", times = NULL, return_datetime = FALSE) {
  stopifnot(file.exists(fname))
  check_file(fname, "slf")

  # read header information
  x_head <- read_slf_header(fname)

  # select variables
  if (length(vars) == 1 && vars == "all") {
    vars <- seq_len(x_head$nbv1)
  } else if (is.character(vars)) {
    vars <- sapply(stringr::str_to_lower(vars), function(x) which(stringr::str_to_lower(x_head$varnames) == x), USE.NAMES = F)
  }

  # read variables
  if (length(vars) == 1 && vars == "none") {
    out <- list(header = x_head, values = NULL)
  } else {
    x_var <- read_slf_variable(fname, x_head$seek_head, vars, x_head$nbv1, x_head$precision, x_head$npoin, times)

    if (return_datetime)
      x_var$time <- x_head$date + x_var$time

    # as data.frame
    vals_df <- data.frame(
      variable = rep(rep(stringr::str_to_lower(x_head$varnames)[vars], each = x_head$npoin), dim(x_var$values)[3]),
      timestep = rep(x_var$time, each = prod(dim(x_var$values)[1:2])),
      x = rep(x_head$x, prod(dim(x_var$values)[2:3])), y = rep(x_head$y, prod(dim(x_var$values)[2:3])),
      value = c(x_var$values)
    )

    # output
    out <- list(header = x_head,
                values = vals_df)
  }
  out
}


#' Write results to *.slf
#'
#' Writes data into a SELAFIN (*.slf) file.
#'
#' @param x An object of class \code{t2d_res} or \code{t2d}.
#' @return An updated version of input \code{x}.
#' @note Writes only the available values (\code{x$values}) into the file and adapts
#' the header accordingly in case the header is referring to more variables that are
#' not imported.
#' @export
write_results <- function(x) UseMethod("write_results")

#' @name write_results
#' @export
write_results.t2d_res <- function(x) {
  x <- validate_res(x)
  fname <- attr(x, "file")
  check_file(fname, "slf")

  # full header
  vars <- stringr::str_to_upper(unique(x$values$variable))
  iv <- sapply(vars, function(v) which(stringr::str_to_upper(x$header$varnames) == v), USE.NAMES = F)
  x_head <- list(title = x$header$title,
                  precision = 4L,
                  nbv1 = length(vars),
                  varnames = vars,
                  varunits = stringr::str_to_upper(x$header$varunits[iv]),
                  date = x$header$date,
                  ntimes = x$header$ntimes,
                  nelem = x$header$nelem,
                  npoin = x$header$npoin,
                  ndp = x$header$ndp,
                  ikle = x$tin$triangles,
                  ipobo = c(x$tin$boundaries, rep(0, x$header$npoin - length(x$tin$boundaries))),
                  x = x$tin$points[,1],
                  y = x$tin$points[,2])

  # prepare data (bring into correct order as defined by mesh in x_head)
  x_df <- arrange_meshdata(x_head$x, x_head$y, stringr::str_to_lower(vars), x$values)
  times <- unique(x_df$timestep)
  x_var <- list(
    precision = x_head$precision,
    time = times,
    values = array(x_df$value, dim = c(x_head$npoin, x_head$nbv1, length(times)))
  )

  # write data
  write_slf_header(fname, x_head)
  write_slf_variable(fname, x_var)

  # return updated t2d_res object
  x_head$varnames <- x_head$varnames
  validate_res(new_res(data = list(header = x_head, tin = x$tin, values = as.data.frame(x_df)),
                       log = x$log, fname = fname, lname = attr(x, "log")))
}

#' @name write_results
#' @export
write_results.t2d <- function(x) {
  x <- validate_t2d(x)
  write_results.t2d_res(x$res)
}
