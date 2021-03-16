#' Read slf header
#'
#' Reads the header of a SELAFIN file (*.slf).
#'
#' @param fname \code{character}, name of a SELAFIN file.
#'
#' @return A \code{list} with the following elements:
#' \describe{
#'   \item{title}{Title of the file}
#'   \item{precision}{Precision of numbers (bytes)}
#'   \item{nbv1}{Number of variables}
#'   \item{varnames}{Names of variables}
#'   \item{varunits}{Units of variables}
#'   \item{date}{Starting date-time (\code{\link{POSIXct}} object)}
#'   \item{ntimes}{Number of timesteps}
#'   \item{nelem}{Number of mesh elements (triangles)}
#'   \item{npoin}{Number of mesh points}
#'   \item{ndp}{Number of points per element (3 in case of triangles)}
#'   \item{ikle}{A \code{nelem x ndp} integer matrix of point indices (referring to \code{x} and \code{y}) defining the mesh elements}
#'   \item{ipobo}{An integer vector of length \code{npoin} defining the mesh boundaries (inner boundaries are zero, outer boundaries numbered)}
#'   \item{x}{A vector of length \code{npoin} giving the x coordinates of the mesh points}
#'   \item{y}{A vector of length \code{npoin} giving the y coordinates of the mesh points}
#'   \item{seek_head}{Position in the SELAFIN file where the header ends (required by function \code{\link{read_slf_variable}})}
#' }
#' @export
read_slf_header <- function(fname) {
  stopifnot(is.character(fname) && length(fname) == 1)
  check_file(fname, "slf")
  stopifnot(file.exists(fname))

  # open file connection
  con <- file(fname, "rb")
  on.exit(close(con), add = TRUE)

  # Title / variables
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  title <- stringr::str_trim(readChar(con, 72))
  precision <- stringr::str_trim(readChar(con, 8))
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  precision_caps <- toupper(precision)
  fsize <- {if (precision_caps %in% c("SERAFIN", "SELAFIN")) 4L
            else if (precision_caps %in% c("SERAFIND", "SELAFIND")) 8L
            else if (precision_caps == "") 4L
            else NA_integer_
  }
  if (is.na(fsize)) stop(paste0("Could not infer precision, unknown specification: '", precision, "'."))

  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  nbv1 <- readBin(con, integer(), 1, size = 4, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  varnames <- character(nbv1)
  varunits <- character(nbv1)
  for (i in seq_len(nbv1)) {
    waste <- readBin(con, integer(), 1, size = 4, endian = "big")
    varnames[i] <- stringr::str_trim(readChar(con, 16))
    varunits[i] <- stringr::str_trim(readChar(con, 16))
    waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  }

  # parameters
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  iparam <- readBin(con, integer(), 10, size = 4, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  # start datetime
  if (iparam[10] == 1) {
    waste <- readBin(con, integer(), 1, size = 4, endian = "big")
    date <- as.POSIXct(paste(readBin(con, integer(), 6, size = 4, endian = "big"), collapse = "-"),
                       tz = "UTC", format = "%Y-%M-%d-%H-%M-%S")
    waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  }

  # number of points/elements
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  nelem <- readBin(con, integer(), 1, size = 4, endian = "big")
  npoin <- readBin(con, integer(), 1, size = 4, endian = "big")
  ndp <- readBin(con, integer(), 1, size = 4, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  # IKLE, mesh connectivities
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  if (waste / 4 != nelem*ndp)
    stop("Dimensions of connectivity table IKLE do not match!")
  ikle <- readBin(con, integer(), nelem*ndp, size = 4, endian = "big")
  ikle <- matrix(ikle, nrow = nelem, ncol = ndp, byrow = TRUE)
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  # IPOBO, boundaries
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  if (waste / 4 != npoin)
    stop("Dimensions of boundary table IPOBO do not match!")
  ipobo <- readBin(con, integer(), npoin, size = 4, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  # X/Y coordinates
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  xvals <- readBin(con, double(), npoin, size = fsize, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  waste <- readBin(con, integer(), 1, size = 4, endian = "big")
  yvals <- readBin(con, double(), npoin, size = fsize, endian = "big")
  waste <- readBin(con, integer(), 1, size = 4, endian = "big")

  # number of timesteps
  nbts <- 4 + fsize + 4 + (npoin * fsize + 8) * nbv1
  seek_head <- seek(con)
  seek(con, 0, origin = "end")
  seek_end <- seek(con)
  ntimes <- (seek_end - seek_head) / nbts

  return(list(title = title,
              precision = fsize,
              nbv1 = nbv1,
              varnames = varnames,
              varunits = varunits,
              date = date,
              ntimes = ntimes,
              nelem = nelem,
              npoin = npoin,
              ndp = ndp,
              ikle = ikle,
              ipobo = ipobo,
              x = xvals,
              y = yvals,
              seek_head = seek_head))
}


#' Read slf variables
#'
#' Reads the variables stored in a SELAFIN file (*.slf).
#'
#' @param fname \code{character}, name of a SELAFIN file.
#' @param seek_start \code{integer}, starting position to read data from the binary
#' file; can be inferred with function \code{\link{read_slf_header}}.
#' @param vars \code{numeric}, vector giving the indices of the variables that shall
#' be read (can be inferred from header information, \code{\link{read_slf_header}}).
#' @param nv \code{integer} giving the total number of variables in the file.
#' @param fsize \code{integer}, precision of the values (number of bytes).
#' @param npoin \code{integer}, number of mesh points.
#' @param times \code{integer} vector, the timesteps to be read. If any specified
#' timestep cannot be found in the file an error is raised. Default (\code{NULL}): all.
#'
#' @return A \code{list} with the following elements:
#' \describe{
#'   \item{time}{Timesteps in seconds relative to \code{date} (element in output of \code{\link{read_slf_header}})}
#'   \item{values}{An array of dimensions \code{(npoin x nv x length(time))} with the values of the variables for each meshpoint and timestep}
#' }
#' @export
read_slf_variable <- function(fname, seek_start, vars, nv, fsize, npoin, times = NULL) {
  stopifnot(is.character(fname) && length(fname) == 1)
  check_file(fname, "slf")
  stopifnot(file.exists(fname))
  stopifnot(all(sapply(c(seek_start, vars, nv, fsize, npoin), is.numeric)))

  nvars <- length(vars)

  # open file connection
  con <- file(fname, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, seek_start)

  # initialisations
  timestep <- NULL
  values_t <- matrix(NA_real_, nrow = npoin, ncol = nvars)
  values <- array(values_t, dim = c(dim(values_t), 1))

  # number of bytes per time step
  nbts <- 4 + (npoin * fsize + 8) * nv

  # as long as new values can be found
  while (TRUE) {
    # check if there is more data
    waste <- readBin(con, integer(), 1, size = 4, endian = "big")
    if (length(waste) == 0) break

    # timestep
    time_t <- readBin(con, double(), 1, size = fsize, endian = "big")
    if (!is.null(times) && !(time_t %in% times)) {
      seek(con, nbts, origin = "current")
      next
    }
    timestep <- c(timestep, time_t)
    waste <- readBin(con, integer(), 1, size = 4, endian = "big")

    # values of variables
    seek_pos <- seek(con)
    for (i in seq_len(nvars)) {
      seek(con, seek_pos + (npoin * fsize + 8) * (vars[i] - 1))
      waste <- readBin(con, integer(), 1, size = 4, endian = "big")
      values_t[,i] <- readBin(con, double(), npoin, size = fsize, endian = "big")
      waste <- readBin(con, integer(), 1, size = 4, endian = "big")
    }
    seek(con, seek_pos + (npoin * fsize + 8) * nv)

    # append to array
    values <- array(c(values, values_t), dim = c(dim(values)[1:2], dim(values)[3] + 1))
  }
  if (!is.null(times) && !all(times %in% timestep))
    stop("There are specified timesteps that cannot be found in the dataset!", call. = F)

  return(list(time = timestep,
              values = values[,,-1, drop = F]))
}


#' Write slf header
#'
#' Writes the header of a SELAFIN file (*.slf).
#'
#' @param fname \code{character}, name of the SELAFIN file to be created.
#' @param header A \code{list} with all header information (see output of \code{\link{read_slf_header}}).
#' @return No return value, called for side effects.
#' @note
#' If a file \code{fname} already exists, it will be silently overwritten.
#'
#' Make sure the datatypes in \code{header} (\code{integer} vs. \code{double}) match
#' (compare with output of \code{\link{read_slf_header}}).
#' @export
write_slf_header <- function(fname, header) {
  stopifnot(is.character(fname) && length(fname) == 1)
  stopifnot(inherits(header, "list"))
  stopifnot(all(c("title", "precision", "nbv1", "varnames", "varunits",
                  "nelem", "npoin", "ndp", "ikle", "ipobo", "x", "y",
                  "date") %in% names(header)))
  check_file(fname, "slf")

  # open file connection
  fs::dir_create(dirname(fname))
  con <- file(fname, "wb")
  on.exit(close(con), add = TRUE)

  # Title / variables
  prec_char <- dplyr::case_when(
    header$precision == 4L ~ "SELAFIN",
    header$precision == 8L ~ "SELAFIND",
    TRUE ~ NA_character_
  )
  if (is.na(prec_char)) stop("Precision value not supported (must be 4 or 8)!", call. = F)
  writeBin(80L, con, size = 4, endian = "big")
  writeChar(stringr::str_pad(header$title, 72, "right"), con, eos = NULL)
  writeChar(stringr::str_pad(prec_char, 8, "right"), con, eos = NULL)
  writeBin(80L, con, size = 4, endian = "big")

  writeBin(8L, con, size = 4, endian = "big")
  writeBin(header$nbv1, con, 1, size = 4, endian = "big")
  writeBin(0L, con, 1, size = 4, endian = "big")
  writeBin(8L, con, size = 4, endian = "big")

  for (i in seq_len(header$nbv1)) {
    writeBin(32L, con, size = 4, endian = "big")
    writeChar(stringr::str_pad(header$varnames[i], 16, "right"), con, eos = NULL)
    writeChar(stringr::str_pad(header$varunits[i], 16, "right"), con, eos = NULL)
    writeBin(32L, con, size = 4, endian = "big")
  }

  # parameters
  writeBin(40L, con, size = 4, endian = "big")
  writeBin(as.integer(c(1, rep(0, 8), 1)), con, size = 4, endian = "big")
  writeBin(40L, con, size = 4, endian = "big")

  # start datetime
  writeBin(24L, con, size = 4, endian = "big")
  writeBin(as.integer(unlist(stringr::str_split(format(header$date, "%Y %m %d %H %M %S"), " "))),
           con, size = 4, endian = "big")
  writeBin(24L, con, size = 4, endian = "big")

  # number of points/elements
  writeBin(16L, con, size = 4, endian = "big")
  writeBin(header$nelem, con, size = 4, endian = "big")
  writeBin(header$npoin, con, size = 4, endian = "big")
  writeBin(header$ndp, con, size = 4, endian = "big")
  writeBin(1L, con, size = 4, endian = "big")
  writeBin(16L, con, size = 4, endian = "big")

  # IKLE, mesh connectivities
  writeBin(4L*header$nelem*header$ndp, con, size = 4, endian = "big")
  writeBin(as.integer(t(header$ikle)), con, size = 4, endian = "big")
  writeBin(4L*header$nelem*header$ndp, con, size = 4, endian = "big")

  # IPOBO, boundaries
  writeBin(4L*header$npoin, con, size = 4, endian = "big")
  writeBin(as.integer(header$ipobo), con, size = 4, endian = "big")
  writeBin(4L*header$npoin, con, size = 4, endian = "big")

  # X/Y coordinates
  writeBin(header$precision*header$npoin, con, size = 4, endian = "big")
  writeBin(header$x, con, size = header$precision, endian = "big")
  writeBin(header$precision*header$npoin, con, size = 4, endian = "big")

  writeBin(header$precision*header$npoin, con, size = 4, endian = "big")
  writeBin(header$y, con, size = header$precision, endian = "big")
  writeBin(header$precision*header$npoin, con, size = 4, endian = "big")
}


#' Write slf variable
#'
#' Writes variable values into a SELAFIN file (*.slf).
#'
#' @param fname \code{character}, name of the SELAFIN file (has to exist already and contain header information).
#' @param data A \code{list} with variable values; see output of \code{\link{read_slf_variable}}
#' for the required elements; in addition an element 'precision' (see \code{\link{read_slf_header}}) is needed.
#' @return No return value, called for side effects.
#' @export
write_slf_variable <- function(fname, data) {
  stopifnot(is.character(fname) && length(fname) == 1)
  check_file(fname, "slf")
  stopifnot(file.exists(fname))
  stopifnot(inherits(data, "list"))
  stopifnot(all(c("time", "values", "precision") %in% names(data)))
  if (length(dim(data$values)) != 3 || dim(data$values)[3] != length(data$time))
    stop("Element 'value' in input 'data' must be of dimensions (npoin x length(nv) x length(time))!", call. = F)

  # open file connection
  con <- file(fname, "ab")
  on.exit(close(con), add = TRUE)

  for (j in seq_along(data$time)) {
    # timestep
    writeBin(data$precision, con, size = 4, endian = "big")
    writeBin(data$time[j], con, size = data$precision, endian = "big")
    writeBin(data$precision, con, size = 4, endian = "big")

    # variables
    for (i in seq_len(dim(data$values)[2])) {
      writeBin(data$precision*dim(data$values)[1], con, size = 4, endian = "big")
      writeBin(data$values[,i,j], con, size = data$precision, endian = "big")
      writeBin(data$precision*dim(data$values)[1], con, size = 4, endian = "big")
    }
  }
}
