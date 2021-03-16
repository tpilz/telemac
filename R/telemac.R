#' @import rlang
#' @importFrom magrittr "%>%"
#' @importFrom utils write.table read.table tail
#' @importFrom graphics plot
#' @useDynLib telemac, .registration=TRUE
#' @importFrom Rcpp sourceCpp
globalVariables(".")

# use data.table without importing it
.datatable.aware = TRUE

#' @description This package includes methods for initialisation, simulation, and
#' visualisation of the hydrodynamic model TELEMAC-2D. To start with the package
#' first have a look at the vignettes: \code{browseVignettes(package = "telemac")}
#' @keywords internal
"_PACKAGE"
