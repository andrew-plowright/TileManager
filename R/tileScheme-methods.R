#' @rdname plot
#' @method plot tileScheme
#' @S3method plot tileScheme

plot.tileScheme <- function(x, add = FALSE, ...){

  if(!all(names(x) == c("tilePolygons", "buffPolygons", "nbuffPolygons"))) stop("Invalid 'tileScheme' object")

  sp::plot(x$nbuffPolygons, border = "red", add = add)
  sp::plot(x$buffPolygons, border = "red", lty = 2, add = TRUE)
  sp::plot(x$tilePolygons, border = "blue", lwd = 2, add = TRUE)
}
