#' Subset
#'
#' Subset tiles using the single bracket operator. Subset geometry (tiles, buffs or nbuffs) using the double brackets
#'
#' @param x a 'tileScheme' object
#' @param i,j,... indices specifying elements to extract
#' @param drop argument not used for 'tileScheme'
#'
#' @export
#'
#' @rdname subset

setMethod("[", "tileScheme", function(x, i, j, ...) {

  oneside <- length(sys.call()) - length(match.call()) == 1

  w <- if(missing(j) & !oneside){

    if(is.character(i)){

      if(any(!i %in% row.names(x@data))) stop("Could not find input tile name(s)")

    }else if(is.numeric(i)){

      if(any(!i %in% 1:nrow(x@data))) stop("Tile number is out of bounds")

    }else stop("Subset using a character or numeric vector")

    i

  }else{

    if(missing(i)) i <- x@data$row
    if(missing(j)) j <- x@data$col

    if(!is.numeric(i) | !is.numeric(j)) stop("Must use numeric values to subset according to row or column number")

    if(any(!j %in% x@data$col)) stop("Column number is out of bounds")
    if(any(!i %in% x@data$row)) stop("Row number is out of bounds")

    which(x@data$col %in% j & x@data$row %in% i)
  }

  if(any(is.na(w)) | length(w) == 0) stop("Invalid subset")

  x@data   <- x@data[w,]
  x@tiles  <- x@tiles[w]
  x@buffs  <- x@buffs[w]
  x@nbuffs <- x@nbuffs[w]

  return(x)

})


#' @export
#' @rdname subset

setMethod("[[", "tileScheme", function(x, i, ...){

  valid <- c('tiles', 'buffs', 'nbuffs')

  if(missing(i) || length(i) != 1 || !i %in% valid){
    stop("Select one of the following: 'tiles', 'buffs', or 'nbuffs'")
  }

  sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(slot(x, i), proj4string = x@crs), x@data)

})


#' Show
#'
#' Print information about a 'tileScheme' object
#'
#' @param object a 'tileScheme' object
#'
#' @export

setMethod("show", "tileScheme", function(object){

  cat(
    "class     : tileScheme", "\n",
    "extent    : ", paste(raster::extent(sp::SpatialPolygons(object@tiles))[], collapse = ", "),  " (xmin, xmax, ymin, ymax)", "\n",
    "CRS       : ", as.character(object@crs), "\n",
    "tiles     : ", length(object@tiles), "\n",
    "nrow/ncol : ", length(unique(object@data$row)), ",", length(unique(object@data$col)), "\n",
    "buffer    : ", object@buffer, "\n",
    "variables : ", paste(names(object@data), collapse = ", "),
    sep = ""
  )

})


#' Plot
#'
#' Plot a 'tileScheme' object
#'
#' @param x a 'tileScheme' object
#' @param y argument not used for 'tileScheme' objects
#' @param labels logical. Add row and column labels
#' @param add logical. Plot 'tileScheme' on top of existing plot
#' @param ... arguments t be passed to methods
#'
#' @export
#' @method plot tileScheme

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @export
#' @rdname plot

setMethod("plot", "tileScheme", function(x, labels = TRUE, add = FALSE, ...){

  sp::plot(x[["nbuffs"]], border = "red",  add = add)
  sp::plot(x[["buffs"]],  border = "red",  lty = 2, add = TRUE)
  sp::plot(x[["tiles"]],  border = "blue", lwd = 2, add = TRUE)

  if(labels){

    r <- x@data[!duplicated(x@data$row), ]
    c <- x@data[!duplicated(x@data$col), ]

    for(rl in r$tileName) text(par()$usr[1], x@tiles[[rl]]@labpt[2], paste0("R", r[rl, "row"]), cex = 0.8, col = "darkgrey", pos = 4, offset = 0.1)
    for(cl in c$tileName) text(x@tiles[[cl]]@labpt[1], par()$usr[3], paste0("C", c[cl, "col"]), cex = 0.8, col = "darkgrey", pos = 3, offset = 0.1)

  }
})



#' Length
#'
#' Get the number of a cells contained in a 'tileScheme' object
#'
#' @param x 'tileScheme' object
#'
#' @export

setMethod("length", "tileScheme", function(x) nrow(x@data))


#' Identical
#'
#' Determine if two 'tileScheme' objects are identical
#'
#' @param x,y 'tileScheme' object
#' @param num.eq,single.NA,attrib.as.set,ignore.bytecode,ignore.environment,ignore.srcref arguments unused for 'tileScheme'
#'
#' @export
#'
#' @method identical tileScheme

setGeneric("identical")

#' @export
#' @rdname identical

setMethod("identical", "tileScheme", function(x, y){

  sameLength <- all(
    nrow(x@data)     == nrow(y@data),
    length(x@tiles)  == length(y@tiles),
    length(x@buffs)  == length(y@buffs),
    length(x@nbuffs) == length(y@nbuffs)
  )

  if(sameLength){

    all(

      # Same data
      all(x@data[,c("row", "col", "tileName")] == y@data[,c("row", "col", "tileName")]),

      # Same data row.names
      all(row.names(x@data) == row.names(y@data)),

      # Same buffer
      x@buffer == y@buffer,

      # Same CRS
      as.character(x@crs) == as.character(y@crs),

      # Same tiles
      all(mapply(identical, y@tiles, x@tiles)),

      # Same buffs
      all(mapply(identical, y@buffs, x@buffs)),

      # Same nbuffs
      all(mapply(identical, y@nbuffs, x@nbuffs))

    )

  }else FALSE
})
