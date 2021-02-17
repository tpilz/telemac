# internal constructor for a t2d_cas object
new_cas <- function(dat, fname) {
  stopifnot(is.character(fname) && length(fname) == 1)
  stopifnot(inherits(dat, "list"))

  structure(
    dat,
    file = fname,
    class = c("t2d_cas", "list")
  )
}

# internal validator for a t2d_cas object
validate_cas <- function(x) {
  stopifnot(inherits(x, c("t2d_cas", "list")))
  stopifnot("file" %in% names(attributes(x)))
  # maximum element length is 144
  if (any(sapply(x, function(s) nchar(paste(s, collapse = ",")) > 144)))
    stop("Values of steering parameters must not be longer than 144 characters!", call. = F)

  x
}

#' Steering file (*.cas)
#'
#' Initialise a steering file for use within TELEMAC.
#'
#' @param x Either: \code{NULL} (default), in which case a simple template will be generated;
#'    a \code{character} string providing the name of an existing steering file;
#'    a \code{list} with named elements being the steering parameters and their values;
#'    an object of class \code{t2d_cas}.
#' @param fname \code{character} string providing the name of the steering file
#' that is to be generated (can also be used to replace an existing entry).
#' @param ... Arguments passed to or from other methods.
#' @return An object of class \code{t2d_cas} consisting of a \code{list} with
#' steering parameters, and an
#' attribute \code{file} pointing to a specific steering file.
#' @examples
#' # template steering parameters
#' cas_tpl <- cas()
#'
#' # investigate object
#' cas_tpl
#' str(cas_tpl)
#' class(cas_tpl) # inherits from list
#'
#' # e.g. subsetting works as with regular lists
#' cas_subset <- cas_tpl[1:5]
#' cas_subset
#'
#' # update cas object (e.g. assign new file name)
#' cas_updated <- cas(cas_tpl, fname = "test.cas")
#' cas_updated
#' @export
cas <- function(x, fname, ...) UseMethod("cas")

#' @name cas
#' @export
cas.default <- function(x = NULL, fname = NULL, ...) {
  tpl <- FALSE
  if (is.null(x)) {
    if (is.null(fname)) fname <- "<template>"
    x <- system.file("extdata", "template.cas", package = "telemac", mustWork = T)
    tpl <- TRUE
  }
  if (is.null(fname)) fname <- "<unspecified>"

  dat <- read_cas(x)

  cas_obj <- new_cas(dat, ifelse(tpl, fname, x))

  validate_cas(cas_obj)
}

#' @name cas
#' @export
cas.list <- function(x, fname = NULL, ...) {
  if (is.null(fname)) fname <- "<unspecified>"
  validate_cas(new_cas(x, fname))
}

#' @param data \code{list} that can be given to update an existing object \code{x}.
#' @name cas
#' @export
cas.t2d_cas <- function(x, fname = NULL, data = NULL, ...) {
  x <- validate_cas(x)

  update <- FALSE
  if (!is.null(fname)) {
    update <- TRUE
  } else {
    fname <- attr(x, "file")
  }
  if (fname == "<template>") fname <- NULL

  if (!is.null(data)) {
    update <- TRUE
  } else {
    data <- unclass(x)
  }

  if (update)
    x <- cas.list(data, fname)

  x
}


#' @name cas
#' @param n Maximum number of steering parameters to print.
#' @export
print.t2d_cas <- function(x, ..., n = 10) {
  cat("Object of class t2d_cas: TELEMAC steering parameters\n")
  cat("Associated steering file:", attr(x, "file"), "\n")

  # all elements must be characters of length 1
  x <- lapply(x, paste, collapse = ",")

  # use data.frame for printing
  df <- data.frame(key = names(x), value = unname(unlist(x)), stringsAsFactors = F)
  if (n > 0) {
    y <- df
    if (nrow(y) > n) {
      cat(paste("First", n, "of", nrow(y), "parameters:\n"))
      y <- df[1:n, , drop = FALSE]
    }
    print.data.frame(y, ...)
  }

  invisible(x)
}


#' Read steering file (*.cas)
#'
#' Reads the steering file of a TELEMAC project.
#'
#' @param fname \code{character} File name of the steering file to be read.
#' @return A \code{list} with steering parameters and their values.
#' @seealso To obtain a \code{t2d_cas} object use function \code{\link{cas}}.
#' @export
read_cas <- function(fname) {
  stopifnot(file.exists(fname))

  df <- read.table(text = gsub(":", "=", readLines(fname)), sep = "=", comment.char = "/",
             quote = "", stringsAsFactors = F, col.names = c("key", "value")) %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    tidyr::pivot_wider(names_from = "key", values_from = "value")

  as.list(df)
}


#' Write steering file (*.cas)
#'
#' Writes the steering file for a TELEMAC project.
#'
#' @param x Either: An object of class \code{t2d_cas};
#'    a \code{list} with elements as in a \code{t2d_cas} object;
#'    a \code{data.frame} with columns \code{key} and \code{value} giving the
#'    steering parameters;
#'    an object of class \code{t2d} with element \code{cas}.
#' @param ... Arguments passed to or from other methods.
#' @return Returns input \code{x} invisibly.
#' @note An existing steering file will be silently overwritten.
#' @examples
#' \dontrun{
#' # creates test.cas in current working directory
#' cas_tpl <- cas()
#' cas_tpl <- cas(cas_tpl, fname = "test.cas")
#' write_cas(cas_tpl)
#' }
#' @export
write_cas <- function(x, ...) UseMethod("write_cas")

#' @param fname \code{character}, a file name (extension .cas) where the steering
#' parameters should be written to.
#' @name write_cas
#' @export
write_cas.data.frame <- function(x, fname, ...) {
  stopifnot(is.character(fname) && length(fname) == 1)
  stopifnot(inherits(x, "data.frame") && all(c("key", "value") %in% colnames(x)))
  check_file(fname, "cas")
  # arrange parameters nicely readable (separate treatment of continued lines)
  maxlen <- max(sapply(x$key, stringr::str_length))
  x$key <- sapply(x$key, function(s) ifelse(nchar(s) > 0,
                                            stringr::str_pad(s, width = maxlen, side = "right"),
                                            ""))
  dat_out <- paste(x$key, x$value, sep = "   :   ")
  cont <- grep("^(   :   )", dat_out)
  dat_out[cont] <- sub("   :   ", "", dat_out[cont])
  write(dat_out, file = fname, sep = "\n")
  invisible(x)
}

#' @name write_cas
#' @export
write_cas.list <- function(x, fname, ...) {
  # all elements must be characters of length 1
  dat <- lapply(x, paste, collapse = ",")

  # values with special symbols must be quoted
  dat <- lapply(dat, check_symbols)

  # data.frame
  df <- data.frame(key = names(dat), value = unname(unlist(dat)), stringsAsFactors = F)

  # elements (key + value) must not be longer than 72 characters, but can be split if so
  df <- cas_lineadapt(df, 7) # sep is "   :   "

  write_cas.data.frame(df, fname = fname)
}

#' @name write_cas
#' @export
write_cas.t2d_cas <- function(x, ...) {
  x <- validate_cas(x)

  write_cas.list(unclass(x), fname = attr(x, "file"))
}

#' @name write_cas
#' @export
write_cas.t2d <- function(x, ...) {
  x <- validate_t2d(x)
  write_cas.t2d_cas(x$cas)
}
