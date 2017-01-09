tplot <- function(tiles, add = FALSE){

  ### GATEKEEPER
  if(length(tiles) != 3){stop("Must be a list of length 3")}

  if(!add){
    old <- par()$mar
    par(mar = rep(0,4))
  }

  sp::plot(tiles$buffPolygons, border = "red", add = add, lty = "dotted")
  sp::plot(tiles$nbuffPolygons, border = "red", add = TRUE)
  sp::plot(tiles$tilePolygons, border = "blue", add = TRUE)

  if(!add) par(mar = old)
}
