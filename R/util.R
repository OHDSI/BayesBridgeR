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

set_reticulate_python_path <- function(python_path = NULL) {
  if (!is.null(python_path)) {
    reticulate::use_python(python_path, required = TRUE)
    return()
  }
  os <- get_os()
  if (os == 'windows') {
    home <- Sys.getenv("homepath")
    anaconda <- "AppData\\Local\\Continuum\\anaconda3"
    bin <- "bin\\python.exe"
    python_path <- paste(home, anaconda, bin, sep="\\")
  } else if (os %in% c("mac", "unix")) {
    home <- path.expand("~")
    anaconda <- "anaconda3"
    bin <- 'bin/python'
    python_path <- paste(home, anaconda, bin, sep="/")
  } else {
    python_path <- 'unknownOS'
  }
  tryCatch(
    reticulate::use_python(python_path, required = TRUE),
    error = function(c) {
      stop("Python binary not found in a default path. Manually specify the path.")
    }
  )
  return(python_path)
}
