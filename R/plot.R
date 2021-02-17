
#' Plot TELEMAC geometry
#'
#' Plots selected objects of class \code{t2d_geo} by interpolating the underlying mesh
#' to a regular grid and using \code{\link[raster]{plot}} of package \code{\link[raster]{raster}}.
#'
#' @param x An object of class \code{t2d_geo}.
#' @param s \code{numeric} value defining the resolution for plotting (the mesh has to be interpolated to a grid for that).
#' @param v \code{character}, name of the variable that is to be plotted
#' (only one at a time; default is \code{elevation}).
#' @param ... Arguments passed to \code{\link[raster]{plot}}.
#' @return No return value, called for side effects (plot).
#' @export
plot.t2d_geo <- function(x, s, v = "elevation", ...) {
  x_rast <- tin2grid(x, s = s, output = "raster", v = v)
  raster::plot(x_rast, ...)
}

#' Plot TELEMAC results
#'
#' Plots selected variable and timestep of objects of class \code{t2d_res}
#' by interpolating the underlying mesh to a regular grid and using \code{\link[raster]{plot}}
#' of package \code{\link[raster]{raster}}.
#'
#' @param x An object of class \code{t2d_res}.
#' @param s \code{numeric} value defining the resolution for plotting (the mesh
#' has to be interpolated to a grid for that).
#' @param v \code{character}, name of the variable that is to be plotted
#' (only one at a time; default is to take the first variable that can be found).
#' @param t \code{integer}, timestep that is to be plotted
#' (only one at a time; default is to plot the first timestep).
#' @param ... Arguments passed to \code{\link[raster]{plot}}.
#' @return No return value, called for side effects (plot).
#' @export
plot.t2d_res <- function(x, s, v = NULL, t = 0, ...) {
  x_rast <- tin2grid(x, s = s, output = "raster", v = v, t = t)
  raster::plot(x_rast, ...)
}

#' Plot TELEMAC mesh
#'
#' Plots the mesh (triangles) of an object of class \code{t2d_tin}.
#'
#' @param x An object of class \code{t2d_tin}.
#' @param ... Arguments passed to \code{\link[graphics]{plot}}.
#' @return No return value, called for side effects (plot).
#' @export
plot.t2d_tin <- function(x, ...) {
  plot(x$points, asp = 1, ...)
  lines.t2d_tin(x, ...)
}

#' Add TELEMAC mesh as lines to a plot
#'
#' Adds the mesh (triangles) of an object of class \code{t2d_tin}
#' as lines to an existing plot.
#'
#' @param x An object of class \code{t2d_tin}.
#' @param ... Arguments passed to \code{\link[graphics]{plot}}.
#' @return No return value, called for side effects (plot).
#' @export
lines.t2d_tin <- function(x, ...) {
  # adapted from RTriangle::plot.triangulation()
  with(x, {
    segments(points[edges[,1],1], points[edges[,1],2],
             points[edges[,2],1], points[edges[,2],2], ...)
    if (!is.null(boundaries)) {
      bnd <- c(boundaries, boundaries[1])
      lines(points[bnd,1], points[bnd,2], col = "red", ...)
    }
    if (!is.null(breaklines)) {
      segments(points[breaklines[,1],1], points[breaklines[,1],2],
               points[breaklines[,2],1], points[breaklines[,2],2], col = "green", ...)
    }
  })
}
