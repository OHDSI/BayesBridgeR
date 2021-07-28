bayesbridge <- NULL

.onLoad <- function(libname, pkgname) {
  # delay load foo module (will only be loaded when accessed via $)
  bayesbridge <<- reticulate::import("bayesbridge", delay_load = TRUE)
}
