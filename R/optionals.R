# internal constructor for a t2d_opt object
new_opt <- function(lines, fname) {
  stopifnot(inherits(lines, "t2d_opt_LINES"))
  stopifnot(inherits(fname, "character"))

  df <- data.frame(file = fname)
  df[["value"]] <- lines

  class(df) <- c("t2d_opt", "data.frame")
  df
}

# internal validator for a t2d_opt object
validate_opt <- function(x) {
  stopifnot(inherits(x, c("t2d_opt", "data.frame")))
  stopifnot(all(c("file", "value") %in% names(x)))
  stopifnot(inherits(x$value, "t2d_opt_LINES"))
  stopifnot(inherits(x$file, "character"))
  x
}

# class t2d_opt_LINES: element 'value' of t2d_opt objects
new_optlines <- function(x) {
  stopifnot(inherits(x, "character"))
  structure(list(x), class = "t2d_opt_LINES")
}
validate_optlines <- function(x) {
  stopifnot(inherits(x, "t2d_opt_LINES"))
  stopifnot(all(sapply(x, function(i) class(i)[1]) == "character"))
  x
}

#' @importFrom utils str
#' @export
str.t2d_opt_LINES <- function(object, ...) {
  n <- length(object)
  cat(paste0(class(object)[1], " of length ", n))
  if (n > 0) {
    cat("; first list element: ")
    str(object[[1]], ...)
  }
}

#' @export
c.t2d_opt_LINES <- function(...) {
  lst <- list(...)
  classes <- sapply(lst, function(x) class(x)[1])
  if (!all(classes == "t2d_opt_LINES"))
    stop("All objects must be of type 't2d_opt_LINES'!", call. = T)
  out <- unlist(lapply(lst, unclass), recursive = F)
  class(out) <- "t2d_opt_LINES"
  validate_optlines(out)
}


#' Optional input files
#'
#' Initialise optional input files for TELEMAC-2D.
#'
#' @param x Either:
#'   a \code{character} vector where each element represents a line of content for
#'   the optional input file;
#'   a \code{list} with multiple \code{character} vectors to provide the contents
#'   for multiple optional files;
#'   an object of type \code{t2d_opt} to add further optional file(s).
#' @param fname \code{character}, file name(s).
#' @param ... Arguments passed to or from other methods.
#' @return An object of type \code{t2d_opt} consisting of a \code{data.frame} with
#'   elements \code{file}, file name(s) of the optional input file(s), and \code{value},
#'   an object of type \code{t2d_opt_LINES} that is essentially a \code{list} of
#'   \code{character} vectors where each element represents the contents of an
#'   optional input file.
#' @note When providing optional input files to a \code{t2d} setup the user still
#' needs to add the respective keywords to the steering (cas) file, otherwise the
#' optional input will be ignored! The reason is that many optional input files
#' require additional settings (keywords in the cas file) that cannot be foreseen.
#'
#' So far only text-based optional input files are supported (e.g.
#' \code{SECTIONS INPUT FILE} or \code{FORMATTED DATA FILE}) but no binary files.
#' @example inst/examples/opt.R
#' @export
optionals <- function(x, fname, ...) UseMethod("optionals")

#' @name optionals
#' @export
optionals.character <- function(x, fname, ...) {
  if (missing(fname)) stop("Argument 'fname' is required!", call. = F)
  optlines <- validate_optlines(new_optlines(x))
  validate_opt(new_opt(optlines, fname))
}

#' @name optionals
#' @export
optionals.list <- function(x, fname, ...) {
  if (missing(fname)) stop("Argument 'fname' is required!", call. = F)
  if (length(x) != length(fname))
    stop("Arguments 'x' and 'fname' must be of equal lengths!", call. = F)
  optlines <- lapply(x, function(y) validate_optlines(new_optlines(y)))
  optlines <- do.call("c", optlines)
  validate_opt(new_opt(optlines, fname))
}

#' @param vals If \code{x} is a \code{t2d_opt} object: a \code{character} vector
#' or a \code{list} with the values for the additional optional input file(s).
#' @name optionals
#' @export
optionals.t2d_opt <- function(x, fname, ..., vals) {
  x <- validate_opt(x)

  add <- NULL
  if (is.character(vals))
    add <- optionals.character(vals, fname)
  else if (is.list(vals))
    add <- optionals.list(vals, fname)
  else stop("Argument 'vals' must be a list or a character vector!", call. = F)

  out <- rbind(x, add)

  validate_opt(out)
}

#' @param n Maximum number of file lines to print.
#' @name optionals
#' @export
print.t2d_opt <- function(x, ..., n = 10) {
  cat("Object of class t2d_opt: TELEMAC optional input files\n")
  if (length(x$file) == 1)
    cat("There is one optional file:", x$file, "\n")
  else
    cat("There are", length(x$file), "optional files:", paste(x$file, collapse = ", "), "\n")

  # total output max. n lines
  f <- length(x$file)
  k <- ceiling(n / f)
  if (k < 5) {
    f <- ceiling(n / 5)
    k <- min(5, n)
  }
  for (i in 1:f) {
    if (f > 1) cat("  ", x$file[i], ":\n", sep = "")
    j <- min(length(x$value[[i]]), k)
    cat(x$value[[i]][1:j], sep = "\n")
    if (length(x$value[[i]]) > j)
      cat("[...]\n")
  }
  if (f < length(x$file))
    cat("[", length(x$file) - f, " file(s) omitted]", sep = "")

  invisible(x)
}

#' @param n Maximum number of file lines to print.
#' @name optionals
#' @export
print.t2d_opt_LINES <- function(x, ..., n = 10) {
  cat("Object of class t2d_opt_LINES: Contents of TELEMAC optional input files\n")
  if (length(x) == 1)
    cat("Contents of one optional file:\n")
  else
    cat("Contents of", length(x), "optional files:\n")

  # total output max. n lines
  f <- length(x)
  k <- n / f
  if (k < 5) {
    f <- max(floor(n / 5), 1)
    k <- min(5, n)
  }
  for (i in 1:f) {
    if (f > 1) cat("  File ",i, ":\n", sep = "")
    j <- min(length(x[[i]]), k)
    cat(x[[i]][1:j], sep = "\n")
    if (length(x[[i]]) > j)
      cat("[...]\n")
  }
  if (f < length(x))
    cat("[", length(x) - f, " file(s) omitted]", sep = "")

  invisible(x)
}


#' Write optional file(s)
#'
#' Writes optional TELEMAC-2D input file(s).
#'
#' @param x An object of class \code{t2d_opt} or \code{t2d}.
#' @return Returns input \code{x} invisibly.
#' @export
write_opt <- function(x) UseMethod("write_opt")

#' @name write_opt
#' @export
write_opt.t2d_opt <- function(x) {
  x <- validate_opt(x)
  waste <- lapply(x$file, check_file, ext = "txt")
  for (f in seq_along(x$file))
    write(x$value[[f]], x$file[f], sep = "\n")
  invisible(x)
}

#' @name write_opt
#' @export
write_opt.t2d <- function(x) {
  x <- validate_t2d(x)
  write_opt.t2d_opt(x$opt)
}
