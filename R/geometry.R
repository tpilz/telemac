# internal constructor for a t2d_geo object
new_geo <- function(header, fname, tin = NULL, elevation, privars = NULL) {
  if (is.null(fname)) fname <- "<unspecified>"
  stopifnot(is.character(fname) && length(fname) == 1)
  stopifnot(inherits(header, "list"))

  if (is.null(tin))
    tin <- tin.matrix(cbind(header$x, header$y), ikle = header$ikle, ipobo = header$ipobo)

  structure(
    list(header = list(title = header$title,
                       precision = header$precision,
                       nelem = header$nelem,
                       npoin = header$npoin,
                       ndp = header$ndp),
         tin = tin,
         elevation = elevation,
         privars = privars),
    file = fname,
    class = c("t2d_geo", "list")
  )
}

# internal validator for a t2d_geo object
validate_geo <- function(x) {
  stopifnot(inherits(x, "t2d_geo"))
  stopifnot("file" %in% names(attributes(x)))
  stopifnot(all(c("header", "tin", "elevation", "privars") %in% names(x)))
  stopifnot(all(c("title", "precision", "nelem", "npoin", "ndp") %in% names(x$header)))

  if (x$header$ndp != 3) stop("Expect 'ndp' to be equal to 3 (resulting in a triangle mesh)!", call. = F)

  x$tin <- validate_tin(x$tin)

  if (any(c(nrow(x$tin$points), length(x$elevation), sapply(x$privars, function(y) length(y$values))) != x$header$npoin))
    stop("Number of points in 'tin', 'elevation', and private variables must be equal to 'npoin' in the header!", call. = F)

  if (any(dim(x$tin$triangles) != c(x$header$nelem, x$header$ndp)))
    stop("'tringales' in 'tin' must be a matrix of dimensions ('nelem' x 'ndp')!", call. = F)

  if (stringr::str_length(x$header$title) > 72)
    stop("Element 'title' must not be longer than 72 characters!", call. = F)

  x
}

#' Mesh / geometry  object
#'
#' Initialise a mesh (geometry) object for use within TELEMAC.
#'
#' @param x Either: A \code{character} string providing the name of an existing geometry file;
# #'    a \code{data.frame} with mesh elements (a triplet of points defines an element);
#'    a \code{list} with all required elements (see 'Value');
#'    an object of class \code{t2d_geo};
#'    an object of class \code{t2d_tin} (requires input \code{dem}).
#' @param fname \code{character}, name for the associated geometry file (can also be used to replace an existing entry).
#' @param ... Arguments passed to or from other methods: \code{\link{read_geo}}
#' if \code{x} is a file name or \code{\link{interpol}} if \code{x} is a \code{t2d_tin} object.
#' @return An object of class \code{t2d_geo} consisting of an attribute \code{file}
#' pointing to a specific geometry file and a \code{list} with the following elements:
#' \describe{
#'   \item{header}{General mesh information including title, precision (of numbers in
#'     the slf file in bytes), the numbers of mesh elements and points, and the number
#'     of points per mesh (3 in case of triangles which is the only supported value for now).}
#'   \item{tin}{A mesh object of class \code{t2d_tin}, see \link{tin}.}
#'   \item{elevation}{Values of mesh point elevations.}
#'   \item{privars}{A named \code{list} of additional, in TELEMAC termed 'private'
#'     variables with elements \code{values} and \code{unit}.}
#' }
#' @note Also note the associated \code{\link{plot.t2d_geo}} method.
#' @example inst/examples/geo.R
#' @export
geo <- function(x, fname, ...) UseMethod("geo")

#' @name geo
#' @export
geo.character <- function(x, fname = NULL, ...) {
  data <- read_geo(x, ...)
  if (is.null(fname)) fname <- x
  validate_geo(new_geo(data, fname, tin = NULL, elevation = data$elevation, privars = data$privars))
}

# This function is not really needed?!
# #' @param col_x If \code{x} is a \code{data.frame}: column with the x coordinates.
# #' @param col_y If \code{x} is a \code{data.frame}: column with the y coordinates.
# #' @param col_elev If \code{x} is a \code{data.frame}: column with the mesh elevation.
# #' @param col_elem If \code{x} is a \code{data.frame}: column with identifier for mesh elements (3 point are an element / triangle).
# #' @param title \code{character}, mesh title (not necessary, max. 72 characters).
# #' @note If \code{x} is a \code{data.frame}, all input arguments referring to columns
# #' of \code{x} support \code{\link[rlang]{quasiquotation}}, i.e. they can be specified by
# #' their names, quoted or unquoted, or as column position.
# #' @name geo
# #' @export
# geo.data.frame <- function(x, fname = NULL, col_x = "x", col_y = "y", col_elev = "z", col_elem = "id", col_var, title = "", ...) {
#   if (stringr::str_length(title) > 72)
#     stop("The 'title' must not be longer than 72 characters!", call. = F)
#
#   if (missing(col_var)) {
#     col_var <- "var"
#     x <- mutate(x, var = "elevation")
#   }
#
#   x_var <- tidyselect::vars_pull(names(x), !! enquo(col_x))
#   y_var <- tidyselect::vars_pull(names(x), !! enquo(col_y))
#   elev_var <- tidyselect::vars_pull(names(x), !! enquo(col_elev))
#   elem_var <- tidyselect::vars_pull(names(x), !! enquo(col_elem))
#   var_var <- tidyselect::vars_pull(names(x), !! enquo(col_var))
#
#   x_sym <- sym(x_var)
#   y_sym <- sym(y_var)
#   elev_sym <- sym(elev_var)
#   elem_sym <- sym(elem_var)
#   var_sym <- sym(var_var)
#
#   # 'elevation' must exist
#   if (!("elevation" %in% unique(x[[var_var]])))
#     stop("Value 'elevation' must exist in column 'col_var'!", call. = F)
#
#   # make sure x is sorted according to col_elem
#   x <- dplyr::arrange(x, !! var_sym, !! elem_sym)
#
#   # unique points
#   points <- x %>%
#     dplyr::filter(!! var_sym == "elevation") %>%
#     dplyr::distinct(x = !! x_sym, y = !! y_sym, z = !! elev_sym)
#
#   # ikle matrix
#   ikle <- find_ikle(cbind(x[[x_var]], x[[y_var]]), cbind(points$x, points$y))
#
#   # compose header information
#   x_head <- list(
#     title = title,
#     precision = 4L,
#     nelem = nrow(ikle),
#     npoin = nrow(points),
#     ndp = 3L,
#     ikle = ikle,
#     ipobo = find_ipobo(ikle, points$x, points$y),
#     x =  points$x,
#     y =  points$y
#   )
#
#   # variable (elevation)
#   data <- c(x_head, list(elevation = points$z,
#                          privars = ))
#
#   validate_geo(new_geo(data, fname))
# }

#' @name geo
#' @export
geo.list <- function(x, fname = NULL, ...) {
  validate_geo(new_geo(x$header, fname, tin = x$tin, elevation = x$elevation, privars = x$privars))
}

#' @param data \code{list} with all required elements (see \code{Value}) that can
#' be given to update an existing object \code{x}.
#' @name geo
#' @export
geo.t2d_geo <- function(x, fname = NULL, data = NULL, ...) {
  x <- validate_geo(x)

  update <- FALSE
  if (!is.null(fname)) {
    update <- TRUE
  } else {
    fname <- attr(x, "file")
  }

  if (!is.null(data)) {
    stopifnot(inherits(data, "list"))
    update <- TRUE
  } else {
    data <- unclass(x)
  }

  if (update)
    x <- geo.list(data, fname)

  x
}

#' @param dem If \code{x} is of type \code{t2d_tin}: a single DEM or a named list
#' with element \code{elevation} and further private variables from which the values will
#' be interpolated to the TIN mesh points. Each element will be forwarded to
#' \code{\link{interpol}}, argument \code{src} (see descriptions therein for supported
#' types). Private variables must be a list with elements \code{values} (the object passed
#' to \code{interpol}), \code{pars_interp} (variable-specific parameter \code{list} passed to
#' \code{interpol} with priority over \code{...}), and \code{unit} (the unit of the
#' variable). Parameters for interpolation include, e.g., the number of neighbours
#' \code{n}, etc.
#' @param title \code{character}, mesh title (not necessary, max. 72 characters).
#' @name geo
#' @export
geo.t2d_tin <- function(x, fname = NULL, ..., dem, title = "") {
  stopifnot(!missing(dem))

  if (stringr::str_length(title) > 72)
    stop("The 'title' must not be longer than 72 characters!", call. = F)

  if (inherits(dem, "list")) {
    if (!("elevation" %in% names(dem)))
      stop("There must be an element 'elevation' in 'dem' if it is a list!", call. = F)
    elev <- interpol(x, dem[["elevation"]], output = "numeric", ...)
    privar <- dem
    privar[["elevation"]] <- NULL
    if (!all(sapply(privar, inherits, "list")) && !all(sapply(privar, function(p) all(names(p) %in% c("values", "unit")))))
      stop("Private variables in 'dem' must each be given as named list with elements 'values' and 'unit'!", call. = F)
    privar <- purrr::map(privar, function(p) interpol_privar(x, p, output = "numeric", ...))
  } else {
    elev <- interpol(x, dem, output = "numeric", ...)
    privar <- NULL
  }

  data <- list(title = title,
               precision = 4L,
               nelem = nrow(x$triangles),
               npoin = nrow(x$points),
               ndp = 3L)

  validate_geo(new_geo(data, fname, x, elev, privar))
}

#' @name geo
#' @export
print.t2d_geo <- function(x, ...) {
  cat("Object of class t2d_geo: TELEMAC geometry (mesh)\n")
  cat("Mesh title:", x$header$title, "\n")
  cat("The mesh is composed of", x$header$nelem, "elements (triangles) and", x$header$npoin, "points\n")
  if (!is.null(x$privars))
    cat("There are additional 'private' variable(s):",
        paste0(names(x$privars), " (", sapply(x$privars, function(y) y$unit), ")", collapse = ", "), "\n")
  cat("Associated geometry file:", attr(x, "file"), "\n")

  invisible(x)
}


#' Read geometry file (*.slf)
#'
#' Reads the geometry / mesh information of a TELEMAC project from a SELAFIN file
#'
#' @param fname \code{character} File name of the geometry file to be read.
#' @param privar \code{logical}, shall variables in addition to bottom elevation
#' (in TELEMAC denoted as private variables) be imported if available? Default: \code{TRUE}.
#' @return A \code{list} with header information (see output of \code{\link{read_slf_header}});
#' \code{elevation}, the mesh point elevations; \code{privars}, named \code{list}
#' of additional 'private' variables.
#' @details At least variable \code{BOTTOM} (bottom elevations of the mesh) is expected
#' in the geometry file.
#' @export
read_geo <- function(fname, privar = TRUE) {
  stopifnot(file.exists(fname))
  check_file(fname, "slf")

  # read header information
  x_head <- read_slf_header(fname)
  if (!any(stringr::str_to_lower(x_head$varnames) %in% "bottom"))
    stop("No variable 'BOTTOM' found in the geometry file!", call. = F)

  # read variable (mesh point elevation) values
  i_elev <- which(stringr::str_to_lower(x_head$varnames) == "bottom")
  if (x_head$nbv1 == 1) privar <- FALSE
  if (privar)
    vars <- seq_len(x_head$nbv1)
  else
    vars <- i_elev
  x_var <- read_slf_variable(fname, x_head$seek_head, vars, x_head$nbv1, x_head$precision, x_head$npoin)

  # output as list
  privars <- NULL
  if (privar) {
    privars <- lapply(vars[-i_elev], function(i) list(values = c(x_var$values[,i,]),
                                                      unit = x_head$varunits[i]))
    names(privars) <- x_head$varnames[-i_elev]
  } else
    i_elev <- 1

  c(x_head, list(elevation = c(x_var$values[,i_elev,]),
                 privars = privars))
}


#' Write geometry file (*.slf)
#'
#' Writes geometry / mesh information into a SELAFIN file.
#'
#' @param x An object of class \code{t2d_geo} or \code{t2d}.
#'
#' @return Returns input \code{x} invisibly.
#'
#' @export
write_geo <- function(x) UseMethod("write_geo")

#' @name write_geo
#' @export
write_geo.t2d_geo <- function(x) {
  x <- validate_geo(x)
  fname <- attr(x, "file")
  check_file(fname, "slf")

  if (is.null(x$privars)) {
    varn <- "BOTTOM"
    varu <- "M"
    nvar <- 1L
    vals <- array(x$elevation, dim = c(x$header$npoin, 1, 1))
  } else {
    varn <- c("BOTTOM", stringr::str_to_upper(names(x$privars)))
    varu <- c("M", stringr::str_to_upper(sapply(x$privars, "[[", "unit")))
    nvar <- length(varn)
    vals <- array(c(x$elevation, sapply(x$privars, "[[", "values")), dim = c(x$header$npoin, nvar, 1))
  }

  # assign (partly dummy) values for missing required information
  x_head <- c(x$header, list(x = x$tin$points[,1], y = x$tin$points[,2],
                             ikle = x$tin$triangles,
                             ipobo = c(x$tin$boundaries, rep(0, x$header$npoin - length(x$tin$boundaries))),
                             nbv1 = nvar, varnames = varn, varunits = varu, date = Sys.time()))

  # write data
  write_slf_header(fname, x_head)

  x_var <- list(
    precision = x_head$precision,
    time = 0,
    values = vals
  )
  write_slf_variable(fname, x_var)
  invisible(x)
}

#' @name write_geo
#' @export
write_geo.t2d <- function(x) {
  x <- validate_t2d(x)
  write_geo.t2d_geo(x$geo)
}

