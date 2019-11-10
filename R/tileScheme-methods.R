#' Subset
#'
#' Subset tiles using the single bracket operator. Subset geometry (tiles, buffs or nbuffs) using the double brackets
#'
#' @param x a 'tileScheme' object
#' @param i,j,... indices specifying elements to extract
#' @param drop,exact arguments not used for 'tileScheme'
#'
#' @rdname subset
#' @export
setMethod("[", signature(x = "tileScheme", i = "character"),
          function(x, i, j, ..., drop = TRUE) {

            if(!missing(j)) stop("Cannot use second index when subsetting using tile name")

            w <- match(i, x@data$tileName)

            if(any(is.na(w))) stop("Could not find input tile name")

            .subset_ts(x, w)

          })

#' @rdname subset
#' @export
setMethod("[", signature(x = "tileScheme", i = "numeric", j = "numeric"),
          function(x, i, j, ..., drop = TRUE) {

            if(any(!j %in% x@data$col)) stop("Column number is out of bounds")
            if(any(!i %in% x@data$row)) stop("Row number is out of bounds")

            w <- which(x@data$col %in% j & x@data$row %in% i)

            .subset_ts(x, w)

          })

#' @rdname subset
#' @export
setMethod("[", signature(x = "tileScheme", i = "numeric", j = "missing"),
          function(x, i, j, ..., drop = TRUE) {

            theCall <- sys.call(-1)
            narg <- length(theCall) - length(match.call(call=sys.call(-1)))

            w <- if(narg > 0){

              if(any(!i %in% x@data$row)) stop("Row number is out of bounds")
              which(x@data$row %in% i)

            }else{

              if(any(!i %in% 1:nrow(x@data))) stop("Tile number index is out of bounds")

              i
            }

            .subset_ts(x, w)

          })

#' @rdname subset
#' @export
setMethod("[", signature(x = "tileScheme", i = "missing", j = "numeric"),
          function(x, i, j, ..., drop = TRUE) {

            if(any(!j %in% x@data$col)) stop("Column number is out of bounds")

            w <- which(x@data$col %in% j)

            .subset_ts(x, w)

          })


#' @rdname subset
#' @export
setMethod("[[", signature(x = "tileScheme", i = "character", j = "missing"),

          function(x, i, j, ..., exact=TRUE) {

            valid <- c('tiles', 'buffs', 'nbuffs')

            if(length(i) != 1 || !i %in% valid){
              stop("Select one of the following: 'tiles', 'buffs', or 'nbuffs'")
            }

            sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(slot(x, i), proj4string = x@crs), x@data)
          })

.subset_ts <- function(x, w){

  if(any(duplicated(w))) stop("Cannot take duplicated indices")

  x@data   <- x@data[w,]
  x@tiles  <- x@tiles[w]
  x@buffs  <- x@buffs[w]
  x@nbuffs <- x@nbuffs[w]

  return(x)
}


#' Get and set data
#'
#' @param x a 'tileScheme' object
#' @param name name of data column
#' @param value vector of new data values
#'
#' @rdname getdata
#' @export

setMethod("$", "tileScheme", function(x, name) x@data[[name]])

#' @rdname getdata
#' @export

setMethod("$<-", "tileScheme", function(x, name, value){
  if(name %in% c("row", "col", "tileName")){
    stop("Cannot modify 'row', 'col' or 'tileName' columns")
  }

  x@data[[name]] = value
  return(x)
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

#' @rdname plot
#' @export

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

#' @rdname identical
#' @export

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
