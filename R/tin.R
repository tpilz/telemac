# internal constructor for a t2d_geo object
new_tin <- function(pts, tri, edg, bnd, brk) {
  structure(
    list(points = pts,
         triangles = tri,
         edges = edg,
         boundaries = bnd,
         breaklines = brk
         ),
    class = c("t2d_tin", "list")
  )
}

# internal validator for a t2d_geo object
validate_tin <- function(x) {
  stopifnot(inherits(x, "t2d_tin"))
  stopifnot(all(c("points", "triangles", "edges", "boundaries", "breaklines") %in% names(x)))

  stopifnot(inherits(x$points, "matrix"))
  stopifnot(ncol(x$points) == 2)

  stopifnot(inherits(x$triangles, "matrix"))
  stopifnot(ncol(x$triangles) == 3)

  stopifnot(inherits(x$edges, "matrix"))
  stopifnot(ncol(x$edges) == 2)

  if (!is.null(x$boundaries)) {
    stopifnot(inherits(x$boundaries, "integer"))
  }

  if (!is.null(x$breaklines)) {
    stopifnot(inherits(x$breaklines, "matrix"))
    stopifnot(ncol(x$breaklines) == 2)
  }

  # all points must occur in triangles and edges
  if (any(!(1:nrow(x$points) %in% unique(c(x$triangles)))) ||
      any(!(1:nrow(x$points) %in% unique(c(x$edges)))))
    stop("There are 'points' not occurring in 'triangles' and / or 'edges'!", call. = F)

  # check boundary points (first points, continuous, anticlockwise)
  if (!all(x$boundaries %in% seq(1, length(x$boundaries))) ||
      length(unique(x$boundaries)) != length(x$boundaries))
    stop(paste0("Boundary points do not comply with requirements: ",
                "outer boundary has to come first and point sequence has to be continuous!"), call. = F)
  pts_bnd <- x$points[x$boundaries,]
  pt_min_i <- which.min(pts_bnd[,2])
  i_check <- (pt_min_i + c(-1, 0, 1)) %% nrow(pts_bnd)
  i_check <- replace(i_check, i_check == 0, nrow(pts_bnd))
  pts_check <- pts_bnd[i_check,]
  # formula see https://en.wikipedia.org/wiki/Curve_orientation
  det <- (pts_check[2,1] - pts_check[1,1]) * (pts_check[3,2] - pts_check[1,2]) -
    (pts_check[3,1] - pts_check[1,1]) * (pts_check[2,2] - pts_check[1,2])
  if (det < 0)
    stop(paste0("Boundary points do not comply with requirements: ",
                "orientation has to be anticlockwise!"), call. = F)

  x
}

#' TIN object
#'
#' Initialise a TIN mesh object for use within TELEMAC.
#'
#' @param x Either: a \code{character} string providing the name of a SELAFIN
#' (\code{*.slf}) or a Gmsh mesh (\code{*.msh}, format version 2) file of which the mesh will be extracted;
#' a \code{matrix} of mesh points with 2 columns containing the x and y coordinates
#' (arguments \code{ikle} and \code{ipobo} are required);
#' a \code{list} with boundary and breakline definitions (see \code{Details}).
#' @param ... Arguments passed to or from other methods. If \code{x} is a \code{list},
#' further arguments passed to \code{\link[RTriangle]{triangulate}}.
#' @return An object of class \code{t2d_tin}, which is a list with the following elements:
#' \describe{
#'   \item{points}{A \code{matrix} with the x and y coordinates (as columns) of mesh points.}
#'   \item{triangles}{A \code{matrix} with 3 columns of indices referring to rows in
#'     \code{points}; each row represents a mesh element (triangle).}
#'   \item{edges}{A \code{matrix} with 2 columns of indices referring to rows in
#'     \code{points}, the node points; each row represents an edge / segment of a triangle.}
#'   \item{boundaries}{A \code{vector} of indices referring to rows in \code{points}, each
#'     marking a point of the outer catchment boundary.}
#'   \item{breaklines}{A \code{matrix} with 2 columns of indices referring to rows in
#'     \code{points}, the vertices of the breaklines (used for mesh refinement during
#'     triangulation).}
#' }
#' @details If \code{x} is a \code{list} this function creates a Triangulated Irregular Network
#' (TIN) using function \code{\link[RTriangle]{triangulate}}. The following \code{list}
#' elements are required to perform the triangulation:
#' \describe{
#'   \item{boundary}{A \code{matrix}, \code{data.frame}, \code{SpatialLines*} or \code{sf} object
#'     with two columns, each row defining a point along
#'     the outer catchment boundary. Points are connected one-by-one to a line starting
#'     with the first point, i.e. make sure points are in the right order! The first and
#'     last point will be connected to close the boundary.}
#'   \item{breaklines}{OPTIONAL, a \code{matrix}, \code{data.frame}, \code{SpatialLines*} or \code{sf}
#'     object with three columns
#'     \code{x} and \code{y}, the x and y coordinates of vertices along the breaklines,
#'     and \code{line}, an identifier to identify individual breaklines.}
#' }
#' @note
#' Duplicated mesh points are silently removed.
#'
#' Make sure breaklines do not intersect as this is not supported by the Triangle
#' algorithm. A possible workaround to split intersecting breaklines in R using
#' \href{https://r-spatial.github.io/sf/}{sf} is shown in the examples.
#'
#' If you want to construct a \code{t2d_tin} object and get the error
#' \code{Boundary points do not comply with requirements: [...]}
#' the reason might be that breaklines are too close to the boundary causing that
#' points of the breaklines are used as boundary points which eventually results
#' in a discontinuous outer boundary. Try to increase the distance of breaklines
#' to the catchment boundary.
#' @example inst/examples/tin.R
#' @export
tin <- function(x, ...) UseMethod("tin")

#' @name tin
#' @export
tin.character <- function(x, ...) {
  fileext <- tail(unlist(strsplit(x, ".", fixed = T)), 1)

  if (fileext == "slf")
    dat <- read_slf_header(x)
  else if (fileext == "msh")
    dat <- read_msh(x)
  else stop("Unsupported file extension!", call. = F)

  pts <- cbind(dat$x, dat$y)
  tri <- dat$ikle
  edg <- as.matrix(unique(data.table::data.table(get_edges(tri))))
  bnd <- dat$ipobo[dat$ipobo > 0]
  brk <- NULL
  validate_tin(new_tin(pts, tri, edg, bnd, brk))
}

#' @param ikle If \code{x} is a matrix of points: A \code{matrix} with 3 columns
#' of indices referring to rows in \code{x}; each row represents a mesh
#' element (triangle). In TELEMAC termed \code{IKLE}.
#' @param ipobo If \code{x} is a matrix of points: A \code{vector} of indices referring
#' to rows in \code{x}, each marking a boundary point. In TELEMAC termed \code{IPOBO}.
#' @name tin
#' @export
tin.matrix <- function(x, ..., ikle, ipobo) {
  edg <- as.matrix(unique(data.table::data.table(get_edges(ikle))))
  bnd <- ipobo[ipobo > 0]
  validate_tin(new_tin(x, ikle, edg, bnd, NULL))
}

#' @param s \code{numeric}, if \code{x} is a \code{list}: OPTIONAL value giving the
#' resolution of vertices along the boundary line for triangulation. If not given,
#' the points are used as they are supplied, otherwise \code{line_spacing} is called to ensure
#' equal spacing of vertices with the given segment lengths.
#' @param s_brk As \code{s} but for breaklines.
#' @param a \code{numeric}, maximum triangle area; passed to \code{\link[RTriangle]{triangulate}}.
#' Default: squared spacing of points (either given as \code{s} or inferred from \code{x$boundary}).
#' @param q \code{numeric}, minimum triangle angle; passed to \code{\link[RTriangle]{triangulate}}.
#' Default: 30 degrees.
#' @name tin
#' @export
tin.list <- function(x, ..., s, s_brk, a, q = 30) {
  stopifnot("boundary" %in% names(x))

  # due to license conflict this package must appear in "Suggests"
  if (!requireNamespace("RTriangle", quietly = TRUE))
    stop("Package \"RTriangle\" is needed for this function. Please install it.", call. = FALSE)

  if (inherits(x$boundary, "SpatialLinesDataFrame"))
    x$boundary <- sp::as.SpatialLines.SLDF(x$boundary)

  if (inherits(x$boundary, "SpatialLines"))
    x$boundary <- sl2df(x$boundary)[,c("x", "y")]

  if (inherits(x$boundary, c("sf", "sfc", "sfg")))
    x$boundary <- sf2df(x$boundary)[,c("x", "y")]

  # boundary points
  pts <- as.matrix(x$boundary)
  pts <- unique(pts)
  pts <- rbind(pts, pts[1,]) # close boundary
  if (!missing(s))
    pts <- as.matrix(line_spacing(pts, s = s, output = "df")[,c("x", "y")])
  pts <- pts[1:(nrow(pts) - 1),] # last point duplicated (or slightly off track)
  seg <- cbind(seq(1, nrow(pts)), c(seq(2, nrow(pts)), 1))

  # add breaklines
  if ("breaklines" %in% names(x)) {
    if (inherits(x$breaklines, "SpatialLinesDataFrame"))
      x$breaklines <- sp::as.SpatialLines.SLDF(x$breaklines)

    if (inherits(x$breaklines, "SpatialLines"))
      x$breaklines <- sl2df(x$breaklines)

    if (inherits(x$breaklines, c("sf", "sfc", "sfg")))
      x$breaklines <- sf2df(x$breaklines)

    brks <- as.data.frame(x$breaklines)[,c("x", "y", "line")]
    if (!missing(s_brk))
      brks <- line_spacing(brks, s = s_brk, output = "df")

    # points must be unique
    brks <- dplyr::anti_join(brks, data.frame(x = pts[,1], y = pts[,2]),
                             by = c("x", "y"))

    if (any(duplicated(brks[,c("x", "y")])))
      stop("There are dplicated points in the breaklines, e.g. because breaklines intersect, which is not supported by the Triangle algorithm!", call. = F)

    seg_brk <- brks %>%
      dplyr::mutate(i = 1:dplyr::n()) %>%
      dplyr::select(.data$line, .data$i) %>%
      dplyr::group_by(.data$line) %>%
      dplyr::filter(dplyr::n() > 1) %>% # remove single points if there are any
      tidyr::nest(pos = .data$i) %>%
      dplyr::mutate(s1 = purrr::map(.data$pos, ~ seq(dplyr::first(.x$i), dplyr::nth(.x$i, -2)) + nrow(pts)),
                    s2 = purrr::map(.data$pos, ~ seq(dplyr::nth(.x$i, 2), dplyr::last(.x$i)) + nrow(pts))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$s1, .data$s2) %>%
      tidyr::unnest(cols = c(.data$s1, .data$s2)) %>%
      as.matrix()
    # merge boundary and breakline points and segments
    pts <- rbind(pts, as.matrix(brks[,c("x", "y")]))
    seg <- rbind(seg, seg_brk)
  }

  # there might be points not pointed to by segments which have to be removed and segments need to be updated accordingly
  pts_rm <- which(!(1:nrow(pts) %in% unique(c(seg))))
  if (any(pts_rm)) {
    pts <- pts[-pts_rm,]
    for (i in seq_along(pts_rm)) {
      i_rm <- seg > (pts_rm[i] - i + 1)
      seg[i_rm] <- seg[i_rm] - 1
    }
  }

  # triangulate
  pts_pslg <- RTriangle::pslg(P = pts, S = seg)
  if (missing(a)) {
    if (missing(s))
      a <- max(diff(pts[c(t(seg)),1]))^2
    else
      a <- s^2
    }
  tri <- RTriangle::triangulate(pts_pslg, a = a, q = q, ...)

  # there might be points that are not part of triangles and cause problems in TELEMAC
  pts_rm <- which(!(1:nrow(tri$P) %in% unique(c(tri$T))))
  if (any(pts_rm)) {
    tri$P <- tri$P[-pts_rm,]
    for (i in seq_along(pts_rm)) {
      i_rm <- tri$T > (pts_rm[i] - i + 1)
      tri$T[i_rm] <- tri$T[i_rm] - 1

      i_rm <- tri$E > (pts_rm[i] - i + 1)
      tri$E[i_rm] <- tri$E[i_rm] - 1

      i_rm <- tri$S > (pts_rm[i] - i + 1)
      tri$S[i_rm] <- tri$S[i_rm] - 1
    }
  }

  # t2d_tin object
  if ("breaklines" %in% names(x))
    brkl <- tri$S[which(!as.logical(tri$SB)),]
  else
    brkl <- NULL
  bnd <- find_ipobo(tri$T, tri$P[,1], tri$P[,2]) # ensure proper order instead of deriving directly from tri
  bnd <- bnd[bnd > 0]
  validate_tin(new_tin(tri$P, tri$T, tri$E, bnd, brkl))
}

#' @name tin
#' @export
print.t2d_tin <- function(x, ...) {
  cat("Object of class t2d_tin: TELEMAC mesh (TIN)\n")
  cat("The mesh is composed of", nrow(x$triangles), "elements (triangles) and", nrow(x$points), "points.\n")

  invisible(x)
}
