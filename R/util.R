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
  # Based on https://docs.anaconda.com/anaconda/user-guide/faq/#installing-anaconda
  # (as retrieved on Aug 2021) and on other resources.
  os <- get_os()
  if (os == 'windows') {
    home <- Sys.getenv("homepath")
    bin <- "python.exe"
    anaconda_loc <- c("Anaconda3", "AppData\\Local\\Continuum\\anaconda3")
    python_path_candidate <- sapply(
      anaconda_loc, function (anaconda) paste(home, anaconda, bin, sep="\\")
    )
  } else if (os %in% c("mac", "unix")) {
    home <- path.expand("~")
    bin <- 'bin/python'
    anaconda_loc <- c("anaconda3", "opt/anaconda3")
    python_path_candidate <- sapply(
      anaconda_loc, function (anaconda) paste(home, anaconda, bin, sep="/")
    )
  } else {
    python_path_candidate <- NULL
    warning('Unknown OS, cannot guess a default Anaconda Python path.')
  }
  for (python_path in python_path_candidate) {
    if (file.exists(python_path)) return(python_path)
  }
  warning("Anaconda not found in common locations. Returning the searched paths.")
  return(python_path_candidate)
}

#' Set up a virtual environment to install the required Python packages.
#'
#' @export
setup_python_env <- function(
    envname='bayesbridge', bayesbridge_ver='==0.2.1',
    python_path=NULL, use_existing=FALSE, ignore_installed=FALSE, use_test_pypi=FALSE
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
  pip_options <- ifelse(
    use_test_pypi, '--index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple', ''
  )
  reticulate::virtualenv_install(
    envname=envname, packages = packages, ignore_installed = ignore_installed,
    pip_options = pip_options
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
