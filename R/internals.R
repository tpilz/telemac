# check file extensions and if it is a relative path
check_file <- function(f, ext, check_rel = FALSE) {
  stopifnot(is.character(f) && length(f) == 1)

  fileext <- tail(unlist(strsplit(f, ".", fixed = T)), 1)
  if (length(fileext) == 0 || fileext != ext)
    stop("File extension must be '.", ext, "'!", call. = F)

  if (check_rel && fs::is_absolute_path(dirname(f)))
    stop("Path to file must be relative (to wdir set in t2d())!", call. = F)
}

# re-arrange data.frame according to order of mesh coordinates and variables
arrange_meshdata <- function(x, y, vars, values) {
  data.frame(x = x, y = y) %>%
    dplyr::left_join(values, by = c("x", "y")) %>% # ensure correct order of x and y
    dplyr::mutate(variable = factor(.data$variable, levels = vars)) %>% # order of variable
    dplyr::arrange(.data$timestep, .data$variable) # correct order of timestep and variable
}


# SpatialLines to data.frame with columns x, y, and line (derived from cump)
sl2df <- function(x) {
  as.data.frame(raster::geom(x)) %>%
    dplyr::rename(line = .data$cump) %>%
    dplyr::select(.data$x, .data$y, .data$line)
}


# sf (MULTI)LINESTRING geometry to data.frame with columns x, y, and line
sf2df <- function(x) {
  if (inherits(sf::st_geometry(x), "sfc_GEOMETRY"))
    x <- sf::st_cast(x, "MULTILINESTRING")
  if (inherits(sf::st_geometry(x), "sfc_MULTILINESTRING")) {
    out <- data.frame(sf::st_coordinates(x)) %>%
      tidyr::unite(.data$L1, .data$L2, col = "line", sep = "") %>%
      dplyr::rename(x = .data$X, y = .data$Y)
  } else if (inherits(sf::st_geometry(x), "sfc_LINESTRING")) {
    out <- data.frame(sf::st_coordinates(x)) %>%
      dplyr::rename(x = .data$X, y = .data$Y, line = .data$L1)
  } else stop("Object's geometry must be of type LINESTRING or MULTILINESTRING, or GEOMETRY that can be cast to the latter!", call. = F)

  out
}


# function checks if a character string contains special characters and if so makes sure they are quoted
check_symbols <- function(x) {
  if (grepl("[/:=& ]", x)) {
    if (!grepl("^'", x))
      x <- paste0("'", x)
    if (!grepl("'$", x))
      x <- paste0(x, "'")
  }
  x
}


# wrapper for interpol() of private data given as list elements (p)
interpol_privar <- function(x, p, output, ...) {
  dots <- list(...)
  args <- list(trg = x, src = p$values, output = output)
  args = c(args, dots)
  if ("pars_interp" %in% names(p))
    args <- utils::modifyList(args, p$pars_interp)

  list(values = do.call("interpol", args),
       unit = p$unit)
}

# convert character string to numeric vector (numbers separated by single space)
tonum <- function(x) as.numeric(unlist(strsplit(x, " ")))
