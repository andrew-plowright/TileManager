#' @importFrom graphics par text
#' @importFrom methods as new slot
#' @importFrom stats var

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("TileManager - Please note that version 0.4.0 is NOT backwards compatible"))
}
