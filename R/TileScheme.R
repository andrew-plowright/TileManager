#' Tile Scheme
#'
#' This function aims to provide an all-in-one tool for creating tiling schemes, which includes options for overlapping
#' buffers and methods for describing tile sizes in various ways (i.e. using either distance units or cell numbers).
#'
#' @param input filename (character), \link[raster]{extent}, \link[raster]{raster} or a vector of four numbers
#' @param dimByCell vector of two numbers. Defines the 'x' and 'y' dimensions of each tile in number of cells. Can only
#' be used when \code{input} is a raster.
#' @param dimByDist vector of two numbers. Defines the 'x' and 'y' dimensions of each tile in the distance unit of
#' \code{input} (usually meters)
#' @param buffer numeric. If set to >0, overlapping buffers will be created around each tile. Defined in cell number or distance unit, depending on the
#' the usage of \code{dimByCell} or \code{dimByDist} respectively
#' @param bufferspill logical. Default is \code{FALSE}, in which case the tiling grid will be pushed inwards so that the
#' buffers of the outer tiles are within the extent of \code{input}. If set to \code{TRUE}, the buffers will extend outside
#' of the extent of \code{input}
#' @param prj character. PROJ4 string defining output coordinate reference system (CRS). If set to NULL, the function will attempt to get
#' a CRS from \code{input} (only works if it is a raster). Set to NA to force the output to have no CRS.
#' @param snap optional. Vector of two numbers corresponding to a pair of coordinates to which the tiling scheme will
#' be aligned. Can only be used in conjunction with \code{dimByDist}. The coordinates do not need to b within the extent of
#' \code{input}
#' @param removeEmpty logical. Default is \code{FALSE}. If set to \code{TRUE}, tiles containing only \code{NA} cell values
#' will be removed from the tiling scheme. Can only be used when \code{input} is a Raster object
#'
#' @return The output of this function is a list of three \link[sp]{SpatialPolygonsDataFrame} objects:
#'   \item{tilePolygons}{The tiling grid. Each polygon corresponds to the extent of a single unbuffered tile.}
#'   \item{buffPolygons}{The buffered tiling grid. Each polygon corresponds to the extent of a buffered tile. These
#'   polygons overlap with neighboring tiles. If \code{buffer} is set to 0, this output will be identical to \code{tilePolygons}.}
#'   \item{nbuffPolygons}{Non-overlapping buffered tiles. These polygons remove overlapping buffers for adjacent tiles, but
#'   preserve buffers for tiles on the edges of the tiling grid. Useful for "reassembling" data that had been originally broken
#'   into tiles.}
#'
#' @examples
#' # Create an irregularly shaped grid defined by the number of raster cells
#' ts1 <- TileScheme(CHMdemo, dimByCell = c(100,120))
#'
#' # Create an square shaped grid defined by unit distance (m)
#' ts2 <- TileScheme(CHMdemo, dimByDist = c(50,50))
#'
#' # Create a grid with buffered cells
#' ts3 <- TileScheme(CHMdemo, dimByDist = c(50,50), buffer = 5)
#' @export

TileScheme <- function(input, dimByCell = NULL, dimByDist = NULL, buffer = 0, bufferspill = FALSE, prj = NULL, snap = NULL, removeEmpty = FALSE){

  ### CHECK INPUTS ----

    # Either "dimByCell" or "dimByDist" must be defined
    if(is.null(dimByCell) & is.null(dimByDist)){
      stop("Please define either \"dimByCell\" or \"dimByDist\".")}

    # Both "dimByCell" or "dimByDist" cannot both be defined
    if(!is.null(dimByCell) & !is.null(dimByDist)){
      stop("Please choose either \"dimByCell\" or \"dimByDist\". Both cannot be used simultaneously.")}

    # If "input" is a file path, attempt to read it as a 'raster' object
    if(is.character(input)){
      if(file.exists(input)){
        input <- try(raster::raster(input), silent=TRUE)
        if(class(input) == "try-error"){stop("Input path for \'input\' must direct to a raster file.")}
      }else{
        stop("Invalid file path for \'input\'. File does not exist.")
      }
    }

    # When using "dimByCell", "input" must be a raster object
    if(!is.null(dimByCell) & !(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack"))){
      stop("When using \"dimByCell\", \"input\" must be a Raster object.")}

    # When using "removeEmpty", "input" must be a raster object
    if(removeEmpty & !(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack"))){
      stop("When using \"removeEmpty\", \"input\" must be a Raster object.")}

    # When using "dimByCell", "snap" cannot be defined
    if(!is.null(dimByCell) & !is.null(snap)){
      stop("The \"snap\" setting can only be used with \"dimByDist\"")}

    # "buffer" cannot be negative
    if(buffer < 0){stop("The value of \"buffer\" cannot be negative.")}

    # "buffer" cannot be equal to or larger than half of the input's narrowest side
    if(
      (!is.null(dimByCell) && buffer >= (min(c(nrow(input), ncol(input))) / 2)) |
      (!is.null(dimByDist) && buffer >= (min(c(raster::extent(input)@xmax - raster::extent(input)@xmin, raster::extent(input)@ymax - raster::extent(input)@ymin)) /2))
    ){stop("\"buffer\" cannot be equal to or larger than half of narrowest side of the input raster")}

    # "buffer" cannot be equal to or larger than half of the narrowest tile side
    if(buffer >= (min(c(dimByDist, dimByCell)) / 2)){stop("\"buffer\" cannot be equal to or larger than half of the narrowest tile side")}

    # Extract projection from input
    prj <- if(!is.null(prj)){
      raster::crs(prj)
    }else if(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack")){
      raster::crs(input)
    }else NA

    # If a single number is input to either "dimByCell" or "dimByDist", repeat it a second time
    if(!is.null(dimByCell) && length(dimByCell) == 1) dimByCell <- rep(dimByCell, 2)
    if(!is.null(dimByDist) && length(dimByDist) == 1) dimByDist <- rep(dimByDist, 2)


  ### DIMENSIONS BY DISTANCE ----

    if(!is.null(dimByDist)){

      # Define dist/cell buffer
      buffer.dist <- buffer

      # Get the extent of the input
      input.ext <- raster::extent(input)

      # If there is a buffer and "bufferspill" is set to FALSE, shrink the input extent
      if(buffer != 0 & bufferspill == FALSE){
        input.ext <-  input.ext - buffer.dist * 2}

      ### CREATE SEQUENCE OF BREAKPOINTS

        # If no snap is set, then compute a sequence of breakpoints starting at the input's xmin and ymax
        if(is.null(snap)){
          xSeq <- seq(input.ext@xmin, input.ext@xmax, by = dimByDist[1])
          if(xSeq[length(xSeq)] < input.ext@xmax){
            xSeq <- c(xSeq, input.ext@xmax)}

          ySeq <- seq(input.ext@ymax, input.ext@ymin, by = -dimByDist[2])
          if(ySeq[length(ySeq)] > input.ext@ymin){
            ySeq <- c(ySeq, input.ext@ymin)}

        # If a 'snap' coordinate is set, compute a sequence of breakpoints from the snap value, subset those breakpoints
        # according to those that are within the input extent, and then start the sequence with the input's
        # xmin and ymax
        }else{
          xSeq <- seq(APfun::AProunder(input.ext@xmin, dimByDist[1], "up", snap = snap[1]),
                      APfun::AProunder(input.ext@xmax, dimByDist[1], "down", snap = snap[1]),
                      by = dimByDist[1])
          if(xSeq[1] > input.ext@xmin){xSeq <- c(input.ext@xmin, xSeq)}
          if(xSeq[length(xSeq)] < input.ext@xmax){xSeq <- c(xSeq, input.ext@xmax)}

          ySeq <- seq(APfun::AProunder(input.ext@ymin, dimByDist[2], "up", snap = snap[2]),
                      APfun::AProunder(input.ext@ymax, dimByDist[2], "down", snap = snap[2]),
                      by = dimByDist[2])
          if(ySeq[1] > input.ext@ymin){ySeq <- c(input.ext@ymin, ySeq)}
          if(ySeq[length(ySeq)] < input.ext@ymax){ySeq <- c(ySeq, input.ext@ymax)}
          ySeq <- rev(ySeq)} # Reverse the order of the y Sequence (max to min)

      # Assemble break points into intervals
      xInt <-  data.frame(xmin = xSeq[1:(length(xSeq) - 1)],
                          xmax = xSeq[2:length(xSeq)])
      yInt <- data.frame(ymin = ySeq[2:length(ySeq)],
                         ymax = ySeq[1:(length(ySeq) - 1)])

      # Create series of row and col numbers
      tilesColRow <- expand.grid(1:nrow(yInt), 1:nrow(xInt))[2:1]
      colnames(tilesColRow) <- c("col", "row")
      tilesColRow.names <- paste0("C", tilesColRow[,"col"], "R", tilesColRow[,"row"])

      # Join all combinations of intervals
      tileInt <-  t(apply(tilesColRow, 1, function(pair){
        as.numeric(c(xInt[pair[1],], yInt[pair[2],]))}))
      colnames(tileInt) <- c("xmin", "xmax", "ymin", "ymax")

      # Apply buffer
      buffInt <- tileInt
      buffInt[,c("xmin", "ymin")] <- buffInt[,c("xmin", "ymin")] - buffer.dist
      buffInt[,c("xmax", "ymax")] <- buffInt[,c("xmax", "ymax")] + buffer.dist

      # Convert to Extent objects
      tileExt <- apply(tileInt, 1, raster::extent)
      names(tileExt) <- tilesColRow.names
      buffExt <- apply(buffInt, 1, raster::extent)
      names(buffExt) <- tilesColRow.names
    }


  ### DIMENSIONS BY NUMBER OF CELLS ----

    if(!is.null(dimByCell)){

      # Define dist/cell buffer
      buffer.cell <- buffer
      buffer.dist <- buffer * raster::res(input)[1]

      # Set extent of raster in terms of rows and columns
      dims <- c(1, ncol(input), 1, nrow(input))
      names(dims) <- c("colmin", "colmax", "rowmin", "rowmax")

      # If there is a buffer and "bufferspill" is set to FALSE, shrink the input extent
      if(buffer != 0 & bufferspill == FALSE){
        dims[c("colmin", "rowmin")] <- dims[c("colmin", "rowmin")] + buffer.cell
        dims[c("colmax", "rowmax")] <- dims[c("colmax", "rowmax")] - buffer.cell}

      # Compute sequence of break points
      colSeq <- seq(dims["colmin"], dims["colmax"], by = dimByCell[1])
      if((dims["colmax"] - dims["colmin"]) %% dimByCell[1] != 0){colSeq <- c(colSeq, dims["colmax"])}
      rowSeq <- seq(dims["rowmin"], dims["rowmax"], by = dimByCell[2])
      if((dims["rowmax"] - dims["rowmin"]) %% dimByCell[2] != 0){rowSeq <- c(rowSeq, dims["rowmax"])}

      # Assemble break points into intervals
      colInt <-  data.frame(colmin = colSeq[1:(length(colSeq) - 1)],
                            colmax = colSeq[2:length(colSeq)] - c(rep(1, length(colSeq) - 2), 0))
      rowInt <-  data.frame(rowmin = rowSeq[1:(length(rowSeq) - 1)],
                            rowmax = rowSeq[2:length(rowSeq)] - c(rep(1, length(rowSeq) - 2), 0))

      # Create series of tile row and tile col numbers
      tilesColRow <- expand.grid(1:nrow(rowInt), 1:nrow(colInt))[2:1]
      colnames(tilesColRow) <- c("col", "row")
      tilesColRow.names <- paste0("C", tilesColRow[,"col"], "R", tilesColRow[,"row"])

      # Join all combinations of intervals
      tileInt <-  t(apply(tilesColRow, 1, function(pair){
        as.numeric(c(colInt[pair[1],], rowInt[pair[2],]))}))
      colnames(tileInt) <- c("colmin", "colmax", "rowmin", "rowmax")

      # Convert to extent objects
      tileExt <- apply(tileInt, 1, function(tile){
        raster::extent(input, tile["rowmin"],  tile["rowmax"],  tile["colmin"],  tile["colmax"])})
      names(tileExt) <- tilesColRow.names

      # Apply buffer
      buffExt <- lapply(tileExt, function(tile){tile + buffer.dist * 2 })
      names(buffExt) <- tilesColRow.names
    }


  ### REMOVE EMPTY TILES ----

    if(removeEmpty){

      # Determine which tiles hold only NA values
      empties <- sapply(tileExt, function(tile){
        all(is.na(suppressWarnings(raster::getValues(raster::crop(input, tile)))))
        #all(is.na(suppressWarnings(raster::crop(input, tile)[])))
      })

      # If no tiles contain any values, return empty object
      if(length(empties[empties]) == length(tileExt)){
        output <- list(NA, NA, NA)
        names(output) <- c("tilePolygons", "buffPolygons", "nbuffPolygons")
        warning("No tiles contained any values. All tiles removed.")
        return(output)
      }

      # Remove empty tiles
      tileExt <- tileExt[!empties]
      buffExt <- buffExt[!empties]
      tilesColRow <- tilesColRow[!empties,]
      tilesColRow.names <- tilesColRow.names[!empties]
    }


  ### CONVERT TO POLYGONS ----

    # Create function to convert extents to polygons
    convertToPolygons <- function(exts){sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(
      lapply(1:length(exts), function(tileNum){
        poly <- methods::as(exts[[tileNum]], 'SpatialPolygons')@polygons[[1]]
        poly@ID <- tilesColRow.names[tileNum]
        return(poly)
      })), data.frame(tileName = tilesColRow.names, col = tilesColRow[,"col"], row = tilesColRow[,"row"], row.names = tilesColRow.names))}

    # Convert to polygons
    tilePoly <- convertToPolygons(tileExt)
    raster::crs(tilePoly) <- prj
    buffPoly <- convertToPolygons(buffExt)
    raster::crs(buffPoly) <- prj


  ### CREATE NON-OVERLAPPING BUFFERS ----

    nbuffPoly <- NonoverlappingBuffers(buffPoly, tilePoly)


  ### RETURN OUTPUT ----

    output <- list(tilePoly, buffPoly, nbuffPoly)
    names(output) <- c("tilePolygons", "buffPolygons", "nbuffPolygons")
    class(output) <- "tileScheme"
    return(output)
}


#' @rdname plot
#' @method plot tileScheme
#' @S3method plot tileScheme

plot.tileScheme <- function(x, add = FALSE, ...){

  if(!all(names(x) == c("tilePolygons", "buffPolygons", "nbuffPolygons"))) stop("Invalid 'tileScheme' object")

  sp::plot(x$nbuffPolygons, border = "red", add = add)
  sp::plot(x$buffPolygons,  border = "red", lty = 2, add = TRUE)
  sp::plot(x$tilePolygons,  border = "blue", lwd = 2, add = TRUE)
}
