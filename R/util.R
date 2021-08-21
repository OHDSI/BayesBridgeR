get_os <- function() {
  if (.Platform$OS.type == "windows") {
    os <- "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    os <- "mac"
  } else if (.Platform$OS.type == "unix") {
    os <- "unix"
  } else {
    os <- "unknown"
    warning("Unknown OS")
  }
  return(os)
}

guess_anaconda_path <- function() {
  os <- get_os()
  if (os == 'windows') {
    home <- Sys.getenv("homepath")
    anaconda <- "AppData\\Local\\Continuum\\anaconda3"
    bin <- "python.exe"
    python_path <- paste(home, anaconda, bin, sep="\\")
  } else if (os %in% c("mac", "unix")) {
    home <- path.expand("~")
    anaconda <- "anaconda3"
    bin <- 'bin/python'
    python_path <- paste(home, anaconda, bin, sep="/")
  } else {
    python_path <- NULL
    warning('Unknown OS, cannot guess a default Anaconda Python path.')
  }
  return(python_path)
}

#' Set up a virtual environment to install the required Python packages.
#'
#' @export
setup_python_env <- function(
    envname='bayesbridge', bayesbridge_ver='==0.2.1',
    python_path=NULL, use_existing=FALSE, ignore_installed=FALSE
  ) {
  py_environments <- reticulate::virtualenv_list()
  if (reticulate::virtualenv_exists(envname) && !use_existing) {
    stop(paste(
      sprintf("Virtual environment %s already exists.", envname),
      "Choose an alternative environment or set 'use_existing' to TRUE."
    ))
  }
  invisible(capture.output(
    path_to_env <- reticulate::virtualenv_create(envname=envname, python=python_path)
      # Simply returns the path if environment already exists
  ))
  packages <- c(paste0('bayesbridge', bayesbridge_ver))
  message("Installing bayesbridge and its dependencies to the environment.")
  reticulate::virtualenv_install(
    envname=envname, packages = packages, ignore_installed = ignore_installed
  )
  return(path_to_env)
}

#' Use the virtual environment created using setup_python_env()
#'
#' @export
configure_python <- function(envname='bayesbridge') {
  if (!reticulate::virtualenv_exists(envname)) {
    stop(paste0(
      "Environment '", envname, "' does not exist. ",
      "Set it up via 'setup_python_env().'"
    ))
  }
  python_path <- reticulate::use_virtualenv(envname, required = TRUE)
  reticulate::use_python(python_path, required = TRUE)
  invisible(python_path)
}
