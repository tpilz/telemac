# internal constructor for a t2d project
new_t2d <- function(title, wdir, exec, cas_in, geo_in, cli_in, res_in, opt_in) {
  stopifnot(is.character(title) && length(title) == 1)
  stopifnot(is.null(exec) || (is.character(exec) && length(exec) == 1))
  stopifnot(is.character(wdir) && length(wdir) == 1)

  cas_obj <- cas(cas_in)
  geo_obj <- geo(geo_in)
  cli_obj <- cli(cli_in)
  if (is.null(res_in) && !is.null(cas_obj[["RESULTS FILE"]]))
    res_obj <- results(res_in, fname = cas_obj[["RESULTS FILE"]])
  else
    res_obj <- results(res_in)

  if (is.null(opt_in))
    opt_obj <- NULL
  else
    opt_obj <- opt_in

  structure(
    list(title = title,
         wdir = wdir,
         exec = exec,
         cas = cas_obj,
         geo = geo_obj,
         cli = cli_obj,
         res = res_obj,
         opt = opt_obj),
    class = "t2d"
  )
}

# internal validator for a t2d object
validate_t2d <- function(x) {

  stopifnot(inherits(x, "t2d"))
  stopifnot(all(c("title", "wdir", "exec", "cas", "geo", "cli", "res", "opt") %in% names(x)))
  stopifnot(inherits(x$cas, "t2d_cas"))
  stopifnot(inherits(x$geo, "t2d_geo"))
  stopifnot(inherits(x$cli, "t2d_cli"))
  stopifnot(inherits(x$res, "t2d_res"))
  if (!is.null(x$opt))
    stopifnot(inherits(x$opt, "t2d_opt"))

  if (!is.null(x$exec)) {
    tryCatch({
      exec_test <- suppressWarnings(system2(x$exec, "--help", stdout = T, stderr = T))
    }, error = function(e) stop("Executable '", x$exec, "' could not be found on the system!", call. = F))
    if (any(grepl("error", exec_test, ignore.case = T))) {
      warnlen <- options()$warning.length
      options(warning.length = 8170)
      on.exit(options(warning.length = warnlen), add = TRUE)
      stop(paste0("Found executable but simple call '", x$exec, " --help' caused an error:\n"), paste(exec_test, collapse = "\n"), call. = F)
    }
  }

  x
}

#' Initialise a TELEMAC-2D project
#'
#' Initialises a project for 2-dimensional hydrodynamic modelling with TELEMAC-2D.
#'
#' @param title \code{character} string giving the title of the project (argument
#'     \code{TITLE} in the steering file).
#' @param wdir \code{character} string, the project directory were inputs and outputs
#' will be written to. TELEMAC-2D's input filenames must be relative to this directory!
#' Default: current working directory.
#' @param exec \code{character} string specifying the TELEMAC-2D executable (system command).
#' Default: \code{NULL}. Only required for \code{\link{simulate_t2d}} to work.
#' @param cas Passed to \code{\link{cas}} (preferably a \code{t2d_cas}) object).
#' @param geo Passed to \code{\link{geo}} (preferably a \code{t2d_geo}) object).
#' @param cli Passed to \code{\link{cli}} (preferably a \code{t2d_cli}) object).
#' @param res Passed to \code{\link{results}}. Can be the name of an existing
#' results file or left empty at the beginning in which case an empty template
#' object will be generated.
#' @param opt Object of class \code{t2d_opt} (if any; default: \code{NULL}).
#'
#' @return An object of class \code{t2d}, that is a list with elements \code{title},
#' \code{wdir}, \code{exec}, \code{cas}, \code{geo}, \code{cli}, \code{res}, and \code{opt}.
#' The latter five are objects of type \code{t2d_*}.
#'
#' @details First, make sure TELEMAC-2D is installed and works!
#'
#' @examples
#' library(sf)
#' library(raster)
#'
#' # template setup with example data
#' bnd <- st_read(system.file("dem/boundary_lagos.gpkg", package = "telemac"))
#' dem_rast <- raster(system.file("dem/dem_merit_lagos.tif", package = "telemac"))
#' tin_obj <- tin(list(boundary = bnd), s = 90, a = 100^2, q = 30)
#' geo_obj <- geo(tin_obj, dem = dem_rast)
#' cli_obj <- cli(geo_obj)
#' cas_obj <- cas()
#'
#' # TELEMAC-2D setup
#' t2d_obj <- t2d("Test setup", "path/to/wdir",
#'                cas = cas_obj, geo = geo_obj, cli = cli_obj)
#' t2d_obj
#'
#' @export
t2d <- function(title = "", wdir = ".",
                cas, geo, cli, res = NULL, opt = NULL, exec = NULL) {
  if (any(c(missing(cas), missing(geo), missing(cli))))
    stop("Arguments 'cas', 'geo', and 'cli' are required!", call. = F)
  validate_t2d(new_t2d(title, wdir, exec, cas, geo, cli, res, opt))
}

#' @name t2d
#' @param x An object of class \code{t2d}.
#' @param ... Optional arguments passed to \code{print} methods.
#' @export
print.t2d <- function(x, ...) {
  cat("Object of class t2d: a TELEMAC-2D project\n")
  cat("\n")
  cat("Title:                 ", x$title, "\n")
  cat("Project directory:     ", x$wdir, "\n")
  cat("TELEMAC-2D executable: ", ifelse(is.null(x$exec), "<unspecified>", x$exec), "\n")
  cat("Steering parameters:    A t2d_cas object pointing to", attr(x$cas, "file"), "\n")
  cat("Geometry / mesh:        A t2d_geo object pointing to", attr(x$geo, "file"), "\n")
  cat("Boundary conditions:    A t2d_cli object pointing to", attr(x$cli, "file"), "\n")
  if (is.null(x$opt))
    cat("Optional input:         None given\n")
  else
    cat("Optional input:         ", paste(x$opt$file, collapse = ", "), "\n", sep = "")
  cat("Simulation results:     A t2d_res object pointing to", attr(x$res, "file"))
  if (!file.exists(paste(x$wdir, attr(x$res, "file"), sep = "/")))
    cat(" (file does not yet exist)\n")
  else if (is.null(x$res$values))
    cat(" (no results imported yet)\n")
  else
    cat("\n")

  invisible(x)
}


#' Write TELEMAC-2D files
#'
#' Writes all input files of a TELEMAC-2D project setup.
#'
#' @param x An object of class \code{t2d}.
#' @return Returns input \code{x} invisibly.
#' @note This function is basically a wrapper around other \code{write_*} functions
#' of the telemac package.
#'
#' The associated file of \code{x$res} will replace entry \code{RESULTS FILE}
#' in the steering parameters.
#'
#' If \code{x$wdir} does not exist, it will be created.
#'
#' If \code{x$wdir} is a relative path it will be considered relative to the current
#' working directory.
#'
#' All \code{t2d_*} objects will be written to the associated filenames into directory
#' \code{wdir} (element of \code{x}). In case the associated
#' filenames contain a relative path the files will be written relative to \code{wdir}.
#'
#' Existing files will be silently overwritten!
#'
#' Parameters \code{BOUNDARY CONDITIONS FILE}, \code{GEOMETRY FILE}, \code{RESULTS FILE},
#' and \code{TITLE} will be adapted to the current setup.
#' @export
write_t2d <- function(x) {
  x <- validate_t2d(x)

  f_cas <- attr(x$cas, "file")
  f_cli <- attr(x$cli, "file")
  f_geo <- attr(x$geo, "file")
  f_res <- attr(x$res, "file")

  check_file(f_cas, "cas", check_rel = T)
  check_file(f_cli, "cli", check_rel = T)
  check_file(f_geo, "slf", check_rel = T)
  check_file(f_res, "slf", check_rel = T)
  if (!is.null(x$opt))
    waste <- lapply(x$opt$file, check_file, ext = "txt", check_rel = T)

  if (fs::is_absolute_path(x$wdir))
    wdir_abs <- x$wdir
  else
    wdir_abs <- fs::path_wd(x$wdir)
  fs::dir_create(wdir_abs)

  x$cas[["BOUNDARY CONDITIONS FILE"]] <- f_cli
  x$cas[["GEOMETRY FILE"]] <- f_geo
  x$cas[["RESULTS FILE"]] <- f_res
  x$cas[["TITLE"]] <- paste0("'", x$title, "'")

  if (!fs::is_absolute_path(f_cas))
    attr(x$cas, "file") <- file.path(wdir_abs, f_cas)
  if (!fs::is_absolute_path(f_cli))
    attr(x$cli, "file") <- file.path(wdir_abs, f_cli)
  if (!fs::is_absolute_path(f_geo))
    attr(x$geo, "file") <- file.path(wdir_abs, f_geo)
  if (!is.null(x$opt) && any(!fs::is_absolute_path(x$opt$file)))
    x$opt$file <- file.path(wdir_abs, x$opt$file)

  write_cas(x)
  write_geo(x)
  write_cli(x)
  if (!is.null(x$opt))
    write_opt(x)

  invisible(x)
}


#' TELEMAC-2D model run
#'
#' Conduct a TELEMAC-2D model run using the model's system command.
#'
#' @param x An object of class \code{t2d}.
#' @param log \code{character}, a filename for logging of runtime messages to be
#' created in the project directory (\code{x$wdir}). Default: \code{run.log}. Set to
#' \code{NULL} if no logfile shall be created (in that case the runlog will not be
#' checked for errors).
#' @param res \code{character}, the results file (must be of type *_.slf). If \code{NULL}
#' (default), the information from the existing steering file will be used or
#' (if the file does not yet exist) will be extracted from \code{x$res}.
#' @param vars Selection of variables to be imported after the simulation: either \code{"all"}
#' (default) reading all variables, \code{"none"} giving only the header, a character
#' vector of variable names, or a numeric vector of positions. Importing all variables
#' may require large storage capacities (depending on the number of variables, mesh points,
#' and specified output intervals).
#' @param exec \code{character}, the TELEMAC-2D executable if not already specified in
#' \code{x}.
#' @return An object of class \code{t2d} with new or updated element \code{res}
#' (an object of class \code{t2d_res}).
#' @note
#' In case the project directory or any of the mandatory input files do not yet exist,
#' this function will first call \code{\link{write_t2d}} and then run the model.
#'
#' TELEMAC-2D runs are often rather long. During simulation time this command will,
#' by default, write nothing to console but store the output in the specified log file.
#'
#' In case of large projects with expected long model runtimes it might make
#' more sense to run TELEMAC-2D directly instead of using this function.
#' @export
simulate_t2d <- function(x, log = "run.log", res = NULL, vars = "all", exec) {


  if (is.null(x$exec))
    if (missing(exec))
      stop("Argument 'exec' required as no executable could be found in 'x'!", call. = F)
    else
      x$exec <- exec

  x <- validate_t2d(x)

  if (fs::is_absolute_path(x$wdir))
    wdir_abs <- x$wdir
  else
    wdir_abs <- fs::path_wd(x$wdir)
  fs::dir_create(wdir_abs)

  # results file
  if (!is.null(res)) {
    f_res <- res
    check_file(f_res, "slf", check_rel = T)
    attr(x$res, "file") <- f_res
    f_cas <- attr(x$cas, "file")
    if (!fs::is_absolute_path(f_cas))
      f_cas <- file.path(wdir_abs, f_cas)
    if (file.exists(f_cas)) {
      cas_dat <- read_cas(f_cas)
      cas_dat[["RESULTS FILE"]] <- f_res
      write_cas(cas_dat, f_cas)
    } else {
      x$cas[["RESULTS FILE"]] <- f_res
    }
  }

  # check if project dir and input files exist
  if (!fs::dir_exists(wdir_abs)) {
    write_t2d(x)
  } else {
    f_cas <- attr(x$cas, "file")
    f_cli <- attr(x$cli, "file")
    f_geo <- attr(x$geo, "file")
    if (any(!file.exists(paste(wdir_abs, c(f_cli, f_geo, f_cas), sep = "/"))))
      write_t2d(x)
  }

  # run
  wd <- getwd()
  setwd(wdir_abs)
  on.exit(setwd(wd), add = TRUE)
  cat("Starting simulation. This may take a while depending on your setup ...\n")
  cmdout <- sys::exec_wait(x$exec, attr(x$cas, "file"),
                           std_out = log, std_in = log)
  cat("Simulation completed!\n")

  # check output log for errors
  if (!is.null(log)) {
    logdat <- readLines(paste(wdir_abs, log, sep = "/"))
    if (!any(grepl("My work is done", logdat)))
      stop("An error occurred during the simulation, see the log file!", call. = F)
  }

  # import results
  x$res <- results(attr(x$res, "file"), log = log, vars = vars)

  x
}
