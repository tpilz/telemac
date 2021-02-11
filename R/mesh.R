#' Interpolate TIN-based mesh values to grid
#'
#' Linearly interpolates the values of a mesh based on irregular triangles to a
#' regular grid.
#'
#' @param x Either: a \code{data.frame} with mesh coordinates and elevations;
#'    an object of class \code{t2d_geo} or \code{t2d_res}.
#' @param s \code{numeric} value defining the resolution of the output grid.
#' @param output \code{character}, either: \code{"data.frame"} (default), \code{"list"}, or
#'    \code{"raster"} to return a \code{\link[raster]{raster}} object. If multiple variables
#'    \code{v} or timesteps \code{t} are given, a \code{data.frame} will be returned in any case.
#' @param ... Arguments passed to or from other methods.
#' @param v \code{character}, name of the variable(s) that is/are to be extracted and
#' interpolated (default all variables that can be found).
#' If \code{x} is of class \code{t2d_geo} \code{elevation} will be taken by default.
#' @return If \code{output == "data.frame"}: A \code{data.frame} with:
#' \describe{
#'   \item{x or \code{col_x}}{X coordinates of the output grid}
#'   \item{y or \code{col_y}}{Y coordinates of the output grid}
#'   \item{z or \code{col_z}}{Interpolated values}
#'   \item{variable}{OPTIONAL (more than one given): imported variable the current values refer to}
#'   \item{timestep}{OPTIONAL (more than one given): simulation timestep the current values refer to}
#' }
#'
#' If \code{output == "list"}: A \code{list} with:
#' \describe{
#'   \item{x}{X coordinates of the output grid}
#'   \item{y}{Y coordinates of the output grid}
#'   \item{z}{Matrix with interpolated values, where \code{z[i,j]} points to coordinates \code{x[i]} and \code{y[j]}}
#' }
#'
#' If \code{output == "raster"}: An object of class \code{\link[raster]{raster}}.
#' @note If you import many variables or timesteps or the mesh is huge or \code{s} very small
#' the resulting dataset might become excessively large (especially if \code{output} is a
#' \code{data.frame})!
#' @example inst/examples/tin2grid.R
#' @export
tin2grid <- function(x, s, output, ...) UseMethod("tin2grid")

#' @param col_x If \code{x} is a \code{data.frame}: column with the x coordinates.
#' @param col_y If \code{x} is a \code{data.frame}: column with the y coordinates.
#' @param col_z If \code{x} is a \code{data.frame}: column with the mesh values.
#' @param tinmat \code{integer} matrix of point indices (referring to x and y coordinates)
#' defining the mesh elements (such as element \code{triangles} of a \code{t2d_tin} object).
#' @note If \code{x} is a \code{data.frame}, all input arguments referring to columns
#' of \code{x} support \code{\link[rlang]{quasiquotation}}, i.e. they can be specified by
#' their names, quoted or unquoted, or as column position.
#' @name tin2grid
#' @export
tin2grid.data.frame <- function(x, s, output = c("data.frame", "list", "raster"), ..., col_x = "x", col_y = "y", col_z = "z", tinmat) {
  output <- match.arg(output)
  stopifnot(inherits(x, "data.frame"))
  stopifnot(all(sapply(list(tinmat, s), is.numeric)))

  x_var <- tidyselect::vars_pull(names(x), !! enquo(col_x))
  y_var <- tidyselect::vars_pull(names(x), !! enquo(col_y))
  z_var <- tidyselect::vars_pull(names(x), !! enquo(col_z))

  # output grid
  xgrd <- seq(min(x[[x_var]]), max(x[[x_var]]), s)
  ygrd <- seq(min(x[[y_var]]), max(x[[y_var]]), s)
  grd <- matrix(NA_real_, nrow = length(xgrd), ncol = length(ygrd))

  # interpolation
  grd <- interpol_grd(x[[x_var]], x[[y_var]], x[[z_var]], tinmat, xgrd, ygrd, grd)

  # output
  out <- NULL
  if (output == "data.frame") {
    out <- tidyr::expand_grid(!! x_var := xgrd, !! y_var := ygrd) %>%
      dplyr::mutate(!! z_var := c(t(grd)))
  } else if (output == "list") {
    out <- list(x = xgrd, y = ygrd, z = grd)
  } else if (output == "raster") {
    grd <- t(grd)
    grd <- grd[nrow(grd):1,]
    out <- raster::raster(grd, xmn = min(xgrd), xmx = max(xgrd), ymn = min(ygrd), ymx = max(ygrd))
  }
  out
}

#' @name tin2grid
#' @export
tin2grid.t2d_geo <- function(x, s, output = c("data.frame", "list", "raster"), ..., v = "elevation") {
  # checks
  x <- validate_geo(x)
  output <- match.arg(output)
  if (any(!(v %in% c("elevation", names(x$privars)))))
    stop("The following variable(s) was/were not found in the geometry file: ",
         paste(v[which(!(v %in% c("elevation", names(x$privars))))], collapse = ", "),
         call. = F)
  if (output != "data.frame" && length(v) > 1) {
    output <- "data.frame"
    warning("Output will be a data.frame when more than one variable 'v' is given!",
            call. = F)
  }

  # get values
  v_priv <- grep("elevation", v, invert = T, value = T)

  vals <- NULL
  if ("elevation" %in% v)
    vals <- c(vals, x[["elevation"]])
  if (length(v_priv) > 0)
    vals <- c(vals, c(sapply(x$privars[v_priv], `[[`, "values")))

  # call tin2grid.data.frame()
  out <- NULL
  if (output == "data.frame") {
    vars <- if("elevation" %in% v) c("elevation", v_priv) else v_priv
    out <- data.frame(x = rep(x$tin$points[,1], length(vars)),
               y = rep(x$tin$points[,2], length(vars)),
               value = vals,
               variable = rep(vars, each = nrow(x$tin$points) * length(vars))) %>%
      tidyr::nest(data = c(.data$x, .data$y, .data$value)) %>%
      dplyr::mutate(interpol = purrr::map(.data$data,
                                          ~ tin2grid.data.frame(.x, tinmat = x$tin$triangles, s = s,
                                                                output = "data.frame", col_z = "value"))) %>%
      dplyr::select(.data$interpol, .data$variable) %>%
      tidyr::unnest(.data$interpol)
  } else if (output %in% c("raster", "list"))
    out <- tin2grid.data.frame(data.frame(x = x$tin$points[,1], y = x$tin$points[,2], z = vals),
                               tinmat = x$tin$triangles, s = s, output = output)

  out
}

#' @param t \code{integer}, timestep(s) that is/are to be extracted and interpolated
#' (default: all timesteps).
#' @name tin2grid
#' @export
tin2grid.t2d_res <- function(x, s, output = c("data.frame", "list", "raster"), ..., v = NULL, t = NULL) {
  x <- validate_res(x)

  output <- match.arg(output)

  x$values$variable <- stringr::str_to_lower(x$values$variable)
  vars <- unique(x$values$variable)
  if (is.numeric(v)) v <- vars[v]
  if (is.null(v)) v <- vars
  v <- stringr::str_to_lower(v)

  if (!all(v %in% vars))
    stop("There are variables requested that are not available in the data!", call. = F)

  times <- unique(x$values$timestep)
  if (is.null(t)) t <- times
  if (!all(t %in% times))
    stop("There are timesteps requested that are not available in the data!", call. = F)

  if (output != "data.frame" && any(c(length(v) > 1, length(t) > 1))) {
    output <- "data.frame"
    warning("Output will be a data.frame when more than one variable 'v' or timestep 't' is given!",
            call. = F)
  }

  # call tin2grid.data.frame()
  out <- NULL
  if (output == "data.frame") {
    out <- x$values %>%
      dplyr::filter(.data$timestep %in% t & .data$variable %in% v) %>%
      tidyr::nest(data = c(.data$x, .data$y, .data$value)) %>%
      dplyr::mutate(interpol = purrr::map(.data$data,
                                          ~ tin2grid.data.frame(.x, tinmat = x$tin$triangles, s = s,
                                                                output = "data.frame", col_z = "value"))) %>%
      dplyr::select(.data$interpol, .data$variable, .data$timestep) %>%
      tidyr::unnest(.data$interpol)
  } else if (output %in% c("raster", "list")) {
    dat_sel <- x$values %>%
      dplyr::filter(.data$variable == v & .data$timestep == t)
    dat_sel <- arrange_meshdata(x$tin$points[,1], x$tin$points[,2], v, dat_sel)
    out <- tin2grid.data.frame(data.frame(x = dat_sel$x, y = dat_sel$y, z = dat_sel$value),
                               tinmat = x$tin$triangles, s = s, output = output)
  }

  out
}


#' Adjust line vertex spacing
#'
#' Function harmonises the lengths of the segments of lines, i.e. the spacing of vertices.
#'
#' @param x Either: a \code{data.frame} with vertex coordinates and line identifier;
#'    a numeric vector with the x coordinates of line vertices;
#'    a matrix with two columns, the x and y coordinates of line vertices;
#'    an object of class \code{SpatialLines*}.
#' @param s \code{numeric} value giving the target spacing of line vertices
#' (i.e. line segment lengths) in units of the input coordinates.
#' @param output Return either: \code{"df"}, a \code{data.frame} (default) or
#' \code{"sp"}, an object of class \code{SpatialLines}.
#' @param ... Arguments passed to or from other methods.
#' @return If \code{output == "df"}: a \code{data.frame} (or \code{\link[tibble]{tibble}})
#' with elements \code{x}, \code{y}, and \code{line} (or \code{col_x}, \code{col_y},
#' \code{col_line}) defining the harmonised line(s).
#'
#' If \code{output == "sp"}: a \code{\link[sp]{SpatialLines}} object of the harmonised line(s).
#'
#' @example inst/examples/line_spacing.R
#' @export
line_spacing <- function(x, s, output = c("df", "sp"), ...) UseMethod("line_spacing")

#' @param col_x If \code{x} is a \code{data.frame}: column with the x coordinates.
#' @param col_y If \code{x} is a \code{data.frame}: column with the y coordinates.
#' @param col_line If \code{x} is a \code{data.frame}: column with the line identifier.
#' @note If \code{x} is a \code{data.frame}, all input arguments referring to columns
#' of \code{x} support \code{\link[rlang]{quasiquotation}}, i.e. they can be specified by
#' their names, quoted or unquoted, or as column position.
#' @name line_spacing
#' @export
line_spacing.data.frame <- function(x, s, output = c("df", "sp"), ..., col_x = "x", col_y = "y", col_line = "line") {
  stopifnot(inherits(x, "data.frame"))
  stopifnot(is.numeric(s))
  output <- match.arg(output)

  x_var <- tidyselect::vars_pull(names(x), !! enquo(col_x))
  y_var <- tidyselect::vars_pull(names(x), !! enquo(col_y))
  line_var <- tidyselect::vars_pull(names(x), !! enquo(col_line))

  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)
  line_sym <- rlang::sym(line_var)

  out_t <- x %>%
    dplyr::group_by(!! line_sym) %>%
    dplyr::summarise(line_seg_adapt(!! x_sym, !! y_sym, s), .groups = "drop") %>%
    dplyr::rename(!! x_sym := .data$x, !! y_sym := .data$y)

  out <- NULL
  if (output == "df") {
    out <- out_t
  } else if (output == "sp") {
    out_lines <- out_t %>%
      tidyr::nest(coords = c(!! x_sym, !! y_sym)) %>%
      dplyr::mutate(splines = purrr::map2(.data$coords, !! line_sym, ~ sp::Lines(sp::Line(cbind(..1[[x_var]], ..1[[y_var]])), ID = ..2)))
    out <- sp::SpatialLines(out_lines$splines)
  }

  out
}

#' @param y The y coordinates of line vertices (if \code{x} is a numeric vector).
#' @param line \code{numeric} vector of identifiers to distinguish individual lines
#' via index (or row) in \code{x} (only needed if \code{x} is a vector or matrix and
#' more than one line is given).
#' @name line_spacing
#' @export
line_spacing.numeric <- function(x, s, output = c("df", "sp"), ..., y, line = NULL) {
  stopifnot(all(sapply(list(x, y), is.numeric)))
  stopifnot(length(x) == length(y))
  stopifnot(is.numeric(s))
  if (is.null(line)) {
    line <- rep(1, length(x))
  } else {
    stopifnot(length(line) == length(x))
  }

  line_spacing.data.frame(data.frame(x = x, y = y, line = line), s = s, output = output, ...)
}

#' @param y The y coordinates of line vertices (if \code{x} is a numeric vector).
#' @param line \code{numeric} vector of identifiers to distinguish individual lines
#' via index (or row) in \code{x} (only needed if \code{x} is a vector or matrix and
#' more than one line is given).
#' @name line_spacing
#' @export
line_spacing.matrix <- function(x, s, output = c("df", "sp"), ..., line = NULL) {
  stopifnot(inherits(x, "matrix"))
  stopifnot(is.numeric(s))
  if (is.null(line)) {
    line <- rep(1, nrow(x))
  } else {
    stopifnot(length(line) == nrow(x))
  }

  line_spacing.data.frame(data.frame(x = x[, 1], y = x[, 2], line = line), s = s, output = output, ...)
}

#' @name line_spacing
#' @export
line_spacing.SpatialLines <- function(x, s, output = "sp", ...) {
  df <- sl2df(x)
  line_spacing.data.frame(df, col_line = "line", s = s, output = output, ...)
}

#' @name line_spacing
#' @export
line_spacing.SpatialLinesDataFrame <- function(x, s, output = "sp", ...)
  line_spacing.SpatialLines(sp::as.SpatialLines.SLDF(x), s = s, output = output, ...)


#' Interpolate from source to target locations
#'
#' Interpolates values from source to target location using inverse distance weighting
#' (IDW) of nearest neighbours.
#'
#' @param trg The target locations. Either: a \code{matrix} or \code{data.frame}
#' with x and y coordinates; an object of class \code{\link[sp]{SpatialPoints}};
#' an object of class \code{t2d_tin}.
#' @param src The source locations. Either: a \code{matrix} with x and y coordinates
#' (argument \code{z} as to be provided); a \code{data.frame} with x and y coordinates and
#' the variable of interest (\code{"z"}); an object of class \code{\link[sp]{SpatialPointsDataFrame}},
#' \code{\link[raster]{raster}}, or \href{https://r-spatial.github.io/stars/index.html}{stars}
#' (only the first attribute is taken).
#' @param n The number of nearest neighbours used for interpolation (default is 5).
#' @param output The type of output: \code{numeric}, \code{sp}, or \code{data.frame} (see below).
#' @param ... Further arguments passed to \code{\link[gstat]{idw}}.
#' @param z If \code{src} is a \code{matrix}: \code{numeric} vector of values at \code{src} locations to be interpolated
#' to \code{trg} locations.
#' @details Function calls \code{\link[gstat]{idw}}. You can pass further arguments to
#' that function, e.g. \code{idp} to influence the distance-based weighting of neighbours
#' (default is 2).
#' @return \code{output = "numeric"}: a vector of values interpolated to \code{trg}
#' locations.
#'
#' \code{output = "data.frame"}: a \code{data.frame} with x and y coordinates of \code{trg}
#' location and interpolated values ("z").
#'
#' \code{output = "sp"}: an object of class \code{\link[sp]{SpatialPointsDataFrame}}
#' with the interpolated values at \code{trg} locations.
#' @export
interpol <- function(trg, src, ...) UseMethod("interpol")

#' @name interpol
#' @export
interpol.SpatialPoints <- function(trg, src, ..., z, n = 5, output = "sp") {
  stopifnot(!missing(src))
  stopifnot(output %in% c("sp", "numeric", "data.frame"))

  if (inherits(src, "matrix")) {
    stopifnot(ncol(src) == 2)
    stopifnot(!missing(z))
    stopifnot(nrow(src) == length(z))
    src <- sp::SpatialPointsDataFrame(src, data.frame(z = z))
  } else if (inherits(src, "data.frame")) {
    stopifnot(c("x", "y", "z") %in% names(src))
    src <- sp::SpatialPointsDataFrame(src[,c("x", "y")], data.frame(z = src$z))
  } else if (inherits(src, "SpatialPointsDataFrame")) {
    src <- src
  } else if (inherits(src, "Raster")) {
    src <- sp::SpatialPointsDataFrame(raster::coordinates(src), data.frame(z = raster::values(src)))
  } else if (inherits(src, "stars")) {
    if (!requireNamespace("stars", quietly = TRUE))
      stop("Package \"stars\" is needed. Please install it.", call. = FALSE)
    src <- sp::SpatialPointsDataFrame(sf::st_coordinates(src), data.frame(z = c(src[[1]])))
  } else
    stop("Argument 'src' is of unsupported type!", call. = F)

  # remove NA points in src
  nas <- which(is.na(src@data$z))
  if (any(nas)) src <- src[-nas,]

  trgsp <- gstat::idw(z~1, locations = src, newdata = trg, nmax = n, debug.level = 0, ...)
  trgsp <- trgsp[,"var1.pred"]
  colnames(trgsp@data) <- "z"

  if (output == "numeric") {
    out <- trgsp@data$z
  } else if (output == "sp") {
    out <- trgsp
  } else if (output == "data.frame") {
    out <- data.frame(x = trgsp@coords[,1], y = trgsp@coords[,2],
                      z = trgsp@data$z)
  }

  out
}

#' @name interpol
#' @export
interpol.matrix <- function(trg, src, ..., n = 5, output = "numeric")
  interpol.SpatialPoints(trg = sp::SpatialPoints(trg), src, n = n, output = output, ...)

#' @name interpol
#' @export
interpol.data.frame <- function(trg, src, ..., n = 5, output = "data.frame")
  interpol.SpatialPoints(trg = sp::SpatialPoints(trg), src, n = n, output = output, ...)

#' @name interpol
#' @export
interpol.t2d_tin <- function(trg, src, ..., n = 5, output = "numeric")
  interpol.SpatialPoints(trg = sp::SpatialPoints(trg$points), src, n = n, output = output, ...)
