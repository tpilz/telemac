# internal constructor for a t2d_cli object
new_cli <- function(df, fname) {
  stopifnot(is.character(fname) && length(fname) == 1)
  stopifnot(inherits(df, "data.frame"))

  structure(
    df,
    file = fname,
    class = c("t2d_cli", "data.frame")
  )
}

# internal validator for a t2d_cli object
validate_cli <- function(x) {
  stopifnot(inherits(x, c("t2d_cli", "data.frame")))
  stopifnot("file" %in% names(attributes(x)))
  stopifnot(all(c("lihbor", "liubor", "livbor",
                  "hbor", "ubor", "vbor", "aubor",
                  "litbor", "tbor", "atbor", "btbor",
                  "n", "k") %in% colnames(x)))

  x
}

#' Boundary (*.cli) object
#'
#' Initialise a boundary (*.cli) object for use within TELEMAC.
#'
#' @param x Either: A \code{character} string providing the name of an existing boundary file;
#'    a \code{numeric} vector of \code{ipobo} values (see \code{\link{geo}}) resulting
#'    in  template boundary values (closed boundary with zero prescribed depths / velocities);
#'    an object of class \code{t2d_geo} providing \code{ipobo} values converted into template boundary values;
#'    a \code{data.frame} with required columns (see 'Value') giving the boundary parameters;
#'    an object of class \code{t2d_cli}.
#' @param fname \code{character} string providing the name of the boundary file
#' that is to be generated (can also be used to replace an existing entry).
#' @param ... Arguments passed to or from other methods.
#' @return An object of class \code{t2d_cli} consisting of an attribute \code{file}
#' pointing to a specific boundary file and a \code{data.frame} where each row
#' represents a point along the mesh boundary. Columns refer to:
#' \describe{
#'   \item{lihbor}{Depth conditions}
#'   \item{liubor}{Velocity conditions in u direction}
#'   \item{livbor}{Velocity conditions in v direction}
#'   \item{hbor}{Prescribed depth if lihbor = 5}
#'   \item{ubor}{Prescribed velocity if liubor = 6}
#'   \item{vbor}{Prescribed velocity if livbor = 6}
#'   \item{aubor}{Friction coefficient at boundary if liubor or livbor = 2}
#'   \item{litbor}{Tracer conditions}
#'   \item{tbor}{Prescribed value of tracer if litbor = 5}
#'   \item{atbor}{Coefficient of flow relation}
#'   \item{btbor}{Coefficient of flow relation}
#'   \item{n}{Global number of boundary point}
#'   \item{k}{Point number of boundary point numbering; can also represent a node colour}
#' }
#'
#' See TELEMAC-2D user manual for more information.
#' @export
cli <- function(x, fname, ...) UseMethod("cli")

#' @name cli
#' @export
cli.character <- function(x, fname = NULL, ...) {
  dat <- read_cli(x)
  if (is.null(fname)) fname <- x
  validate_cli(new_cli(dat, fname))
}

#' @name cli
#' @export
cli.numeric <- function(x, fname = NULL, ...) {
  if (is.null(fname)) fname <- "<unspecified>"

  # only outer boundaries (ipobo > 0)
  ipobo <- x[x > 0]

  # template boundary conditions: closed boundary with zero prescribed depths / velocities
  df <- data.frame(
    lihbor = rep(2, length(ipobo)), liubor = rep(2, length(ipobo)), livbor = rep(2, length(ipobo)),
    hbor = rep(0, length(ipobo)), ubor = rep(0, length(ipobo)), vbor = rep(0, length(ipobo)),
    aubor = rep(0, length(ipobo)), litbor = rep(2, length(ipobo)),
    tbor = rep(0, length(ipobo)), atbor = rep(0, length(ipobo)), btbor = rep(0, length(ipobo)),
    n = ipobo, k = 1:length(ipobo))
  validate_cli(new_cli(df, fname))
}

#' @name cli
#' @export
cli.t2d_geo <- function(x, fname = NULL, ...) {
  x <- validate_geo(x)
  cli.numeric(x$tin$boundaries, fname)
}

#' @name cli
#' @export
cli.data.frame <- function(x, fname = NULL, ...) {
  if (is.null(fname)) fname <- "<unspecified>"
  validate_cli(new_cli(x, fname))
}

#' @param df \code{data.frame} that can be given to update an existing object \code{x}.
#' @name cli
#' @export
cli.t2d_cli <- function(x, fname = NULL, df = NULL, ...) {
  x <- validate_cli(x)

  update <- FALSE
  if (!is.null(fname)) {
    update <- TRUE
  } else {
    fname <- attr(x, "file")
  }

  if (!is.null(df)) {
    update <- TRUE
  } else {
    df <- as.data.frame(x)
  }

  if (update)
    x <- cli.data.frame(df, fname)

  x
}

#' @name cli
#' @param n Maximum number of boundary points to print.
#' @export
print.t2d_cli <- function(x, ..., n = 10) {
  cat("Object of class t2d_cli: TELEMAC boundary parameters\n")
  cat("Associated boundary file:", attr(x, "file"), "\n")

  if (n > 0) {
    if (inherits(x, "tbl_df"))
      NextMethod()
    else {
      y <- x
      if (nrow(y) > n) {
        cat(paste("First", n, "of", nrow(y), "boundary points:\n"))
        y <- x[1:n, , drop = FALSE]
      }
      print.data.frame(y, ...)
    }
  }

  invisible(x)
}

#' @name cli
#' @export
`[.t2d_cli` <- function(x, ...) {
  out <- NextMethod()
  attr(out, "file") <- attr(x, "file")
  class(out) <- class(x)
  out
}

#' Read boundary (*.cli) file
#'
#' Reads the boundary file of a TELEMAC project.
#'
#' @param fname \code{character} File name of the boundary file to be read.
#' @return A \code{data.frame} where each row represents a point along the mesh boundary.
#' Columns refer to:
#' \describe{
#'   \item{lihbor}{Depth conditions}
#'   \item{liubor}{Velocity conditions in u direction}
#'   \item{livbor}{Velocity conditions in v direction}
#'   \item{hbor}{Prescribed depth if lihbor = 5}
#'   \item{ubor}{Prescribed velocity if liubor = 6}
#'   \item{vbor}{Prescribed velocity if livbor = 6}
#'   \item{aubor}{Friction coefficient at boundary if liubor or livbor = 2}
#'   \item{litbor}{Tracer conditions}
#'   \item{tbor}{Prescribed value of tracer if litbor = 5}
#'   \item{atbor}{Coefficient of flow relation}
#'   \item{btbor}{Coefficient of flow relation}
#'   \item{n}{Global number of boundary point}
#'   \item{k}{Point number of boundary point numbering; can also represent a node colour}
#' }
#'
#' See TELEMAC-2D user manual for more information.
#' @export
read_cli <- function(fname) {
  stopifnot(file.exists(fname))
  check_file(fname, "cli")
  read.table(fname, col.names = c("lihbor", "liubor", "livbor",
                                  "hbor", "ubor", "vbor", "aubor",
                                  "litbor", "tbor", "atbor", "btbor",
                                  "n", "k"))
}


#' Write boundary file (*.cli)
#'
#' Writes boundary conditions for a TELEMAC simulation.
#'
#' @param x An object of class \code{t2d_cli} or \code{t2d}.
#'
#' @return Returns input \code{x} invisibly.
#'
#' @export
write_cli <- function(x) UseMethod("write_cli")

#' @name write_cli
#' @export
write_cli.t2d_cli <- function(x) {
  x <- validate_cli(x)
  fname <- attr(x, "file")
  check_file(fname, "cli")

  # format numbers
  x <- x %>%
    dplyr::mutate_at(dplyr::vars(1:3,8, 12, 13), sprintf, fmt = "%d") %>%
    dplyr::mutate_at(dplyr::vars(4:7, 9:11), sprintf, fmt = "%.3f")

  write.table(x, fname, quote = F, row.names = F, col.names = F)
  invisible(x)
}

#' @name write_cli
#' @export
write_cli.t2d <- function(x) {
  x <- validate_t2d(x)
  write_cli.t2d_cli(x$cli)
}
