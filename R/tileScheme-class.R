tileScheme_valid <- function(object){

  checks <- logical()

  # 'tiles', 'buffs' and 'nbuffs' are same length
  checks["tileNumber"] <- var(sapply(list(object@tiles, object@buffs, object@nbuffs), length)) == 0
  if(!checks["tileNumber"]) stop("The tile scheme's 'tiles', 'buffs' and 'nbuffs' are not the same length")

  # Number of data rows is equal to number of tiles
  checks["dataRows"] <- length(object@tiles) == nrow(object@data)
  if(!checks["dataRows"]) stop("Number of tiles and data rows does not match")

  # Tile names match as they should
  checks["tileNames"] <- all(names(object@tiles) == names(object@buffs)) & all(names(object@tiles) == names(object@nbuffs))
  if(!checks["tileNames"]) stop("Tile names for do for 'tiles', 'buffs' and 'nbuffs' do not match")
  checks["tileNames2"] <-   all(names(object@tiles) == row.names(object@data))
  if(!checks["tileNames2"]) stop("Tile names do not match data row.names")
  checks["tileNames3"] <-   all(names(object@tiles) == object@data$tileName)
  if(!checks["tileNames3"]) stop("'TileName' field does not match data row.names")

  return(all(checks))
}


#' @importClassesFrom sp CRS

setClass('tileScheme',
         representation(
           tiles  = 'list',
           buffs  = 'list',
           nbuffs = 'list',
           crs    = 'CRS',
           buffer = 'numeric',
           data   = 'data.frame'
         ),
         validity = tileScheme_valid
        )


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


setMethod("[[", "tileScheme", function(x, i, ...){

  valid <- c('tiles', 'buffs', 'nbuffs')

  if(missing(i) || length(i) != 1 || !i %in% valid){
    stop("Select one of the following: 'tiles', 'buffs', or 'nbuffs'")
  }

  sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(slot(x, i), proj4string = x@crs), x@data)

})


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

if (!isGeneric("plot")) {
  setGeneric("plot", function(x,y,...)
    standardGeneric("plot"))
}


#' @export
#' @method plot tileScheme

setGeneric("plot", function(x, y, ...)
  standardGeneric("plot"))

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

#' @export
#' @method length tileScheme

setMethod("length", "tileScheme", function(x) nrow(x@data))


#' @export
#' @method identical tileScheme

setGeneric("identical")

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
