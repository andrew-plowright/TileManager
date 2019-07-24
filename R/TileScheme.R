#' Tile Scheme
#'
#' This function aims to provide an all-in-one tool for creating tiling schemes, which includes options for overlapping
#' buffers and methods for describing tile sizes in various ways (i.e. using either distance units or cell numbers).
#'
#' @section Non-overlapping buffers:
#'
#' When processing a tiled dataset, using buffered tiles can help remove the edge effects along the
#' individual tile borders. However, overlapping buffers generally need to be removed when recombining
#' a series of tiles back into a single raster. Although this can be accomplished by using the unbuffered
#' tile extent, this will also remove the buffered areas along the edge of the tile set. Once these
#' unbuffered tiles are reassembled, the resulting raster will then be smaller than the original
#' dataset before it was tiled.
#'
#' This may not be a desirable result. The polygons located in the \code{nbuffs} slot will produce a set of
#' polygons that correspond to the tile extents that conserve buffers only where they do not overlap onto
#' neighboring tiles (i.e.: along the edge of the tile set). These polygons are useful for cropping out
#' overlapping areas from buffered tiles in order to reassemble the tiles into a single raster.
#'
#'
#' @param input filename (character), Extent, Raster or a vector of four numbers
#' @param dimByCell vector of two numbers. Defines the 'x' and 'y' dimensions of each tile in number of cells. Can only
#' be used when \code{input} is a raster.
#' @param dimByDist vector of two numbers. Defines the 'x' and 'y' dimensions of each tile in the distance unit of
#' \code{input} (usually meters)
#' @param buffer numeric. If set to >0, overlapping buffers will be created around each tile. Defined in cell number or distance unit, depending on the
#' the usage of \code{dimByCell} or \code{dimByDist} respectively
#' @param bufferspill logical. Default is \code{FALSE}, in which case the tiling grid will be pushed inwards so that the
#' buffers of the outer tiles are within the extent of \code{input}. If set to \code{TRUE}, the buffers will extend outside
#' of the extent of \code{input}
#' @param crs character. PROJ4 string defining output coordinate reference system (CRS). If set to NULL, the function will attempt to get
#' a CRS from \code{input} (only works if it is a raster). Set to NA to force the output to have no CRS.
#' @param snap optional. Vector of two numbers corresponding to a pair of coordinates to which the tiling scheme will
#' be aligned. Can only be used in conjunction with \code{dimByDist}. The coordinates do not need to b within the extent of
#' \code{input}
#' @param removeEmpty logical. Default is \code{FALSE}. If set to \code{TRUE}, tiles containing only \code{NA} cell values
#' will be removed from the tiling scheme. Can only be used when \code{input} is a Raster object
#'
#' @return The output of this function is a list of three SpatialPolygonsDataFrame objects:
#'   \item{tiles}{The tiling grid. Each polygon corresponds to the extent of a single unbuffered tile.}
#'   \item{buffs}{The buffered tiling grid. Each polygon corresponds to the extent of a buffered tile. These
#'   polygons overlap with neighboring tiles. If \code{buffer} is set to 0, this output will be identical to \code{tilePolygons}.}
#'   \item{nbuffs}{Non-overlapping buffered tiles. These polygons remove overlapping buffers for adjacent tiles, but
#'   preserve buffers for tiles on the edges of the tiling grid. Useful for "reassembling" data that had been originally broken
#'   into tiles.}
#'
#' @examples
#' # Create an irregularly shaped grid defined by the number of raster cells
#' ts1 <- tileScheme(CHMdemo, dimByCell = c(100,120))
#'
#' # Create an square shaped grid defined by unit distance (m)
#' ts2 <- tileScheme(CHMdemo, dimByDist = c(50,50))
#'
#' # Create a grid with buffered cells
#' ts3 <- tileScheme(CHMdemo, dimByDist = c(50,50), buffer = 5)
#' @export

tileScheme <- function(input, dimByCell = NULL, dimByDist = NULL, buffer = 0, bufferspill = FALSE, crs = NULL, snap = NULL, removeEmpty = FALSE){

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
    crs <- if(!is.null(crs)){
      raster::crs(crs)
    }else if(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack")){
      raster::crs(input)
    }else sp::CRS()

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
          xSeq <- seq(
            APfun::AProunder(input.ext@xmin, dimByDist[1], "up",   snap = snap[1]),
            APfun::AProunder(input.ext@xmax, dimByDist[1], "down", snap = snap[1]),
            by = dimByDist[1]
            )
          if(xSeq[1] > input.ext@xmin){xSeq <- c(input.ext@xmin, xSeq)}
          if(xSeq[length(xSeq)] < input.ext@xmax){xSeq <- c(xSeq, input.ext@xmax)}

          ySeq <- seq(
            APfun::AProunder(input.ext@ymin, dimByDist[2], "up",   snap = snap[2]),
            APfun::AProunder(input.ext@ymax, dimByDist[2], "down", snap = snap[2]),
            by = dimByDist[2]
            )
          if(ySeq[1] > input.ext@ymin){ySeq <- c(input.ext@ymin, ySeq)}
          if(ySeq[length(ySeq)] < input.ext@ymax){ySeq <- c(ySeq, input.ext@ymax)}
          ySeq <- rev(ySeq)} # Reverse the order of the y Sequence (max to min)

      # Assemble break points into intervals
      xInt <-  data.frame(
        xmin = xSeq[1:(length(xSeq) - 1)],
        xmax = xSeq[2:length(xSeq)]
        )

      yInt <- data.frame(
        ymin = ySeq[2:length(ySeq)],
        ymax = ySeq[1:(length(ySeq) - 1)]
        )

      # Create series of row and col numbers
      tilesColRow <- expand.grid(1:nrow(xInt), 1:nrow(yInt))[2:1]
      colnames(tilesColRow) <- c("row", "col")
      tilesColRow$tileName <- paste0("R", tilesColRow[,"row"], "C", tilesColRow[,"col"])
      row.names(tilesColRow) <- tilesColRow$tileName

      # Join all combinations of intervals
      tileInt <- do.call(rbind, lapply(1:nrow(tilesColRow), function(x){

        cbind(xInt[tilesColRow[x, "col"], ],
              yInt[tilesColRow[x, "row"], ])
      }))

      # Apply buffer
      buffInt <- tileInt
      buffInt[,c("xmin", "ymin")] <- buffInt[,c("xmin", "ymin")] - buffer.dist
      buffInt[,c("xmax", "ymax")] <- buffInt[,c("xmax", "ymax")] + buffer.dist

      # Convert to Extent objects
      tileExt <- apply(tileInt, 1, raster::extent)
      names(tileExt) <- tilesColRow$tileName
      buffExt <- apply(buffInt, 1, raster::extent)
      names(buffExt) <- tilesColRow$tileName
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
      tilesColRow <- expand.grid(1:nrow(colInt), 1:nrow(rowInt))[2:1]
      colnames(tilesColRow) <- c("row", "col")
      tilesColRow$tileName <- paste0("R", tilesColRow[,"row"], "C", tilesColRow[,"col"])
      row.names(tilesColRow) <- tilesColRow$tileName

      # Join all combinations of intervals
      tileInt <- do.call(rbind, lapply(1:nrow(tilesColRow), function(x){

        cbind(colInt[tilesColRow[x, "col"], ],
              rowInt[tilesColRow[x, "row"], ])
      }))


      # Convert to extent objects
      tileExt <- apply(tileInt, 1, function(tile){
        raster::extent(input, tile["rowmin"],  tile["rowmax"],  tile["colmin"],  tile["colmax"])})
      names(tileExt) <- tilesColRow$tileName

      # Apply buffer
      buffExt <- lapply(tileExt, function(tile){tile + buffer.dist * 2 })
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
    }


  ### CONVERT TO POLYGONS ----

    tilePoly  <- .extents_to_polygons(tileExt)
    buffPoly  <- .extents_to_polygons(buffExt)
    nbuffPoly <- .nonoverlappingBuffers(tileExt, buffExt, tilesColRow)


  ### RETURN OUTPUT ----

    new("tileScheme",
        tiles  = tilePoly,
        buffs  = buffPoly,
        nbuffs = nbuffPoly,
        buffer = buffer,
        crs    = crs,
        data   = tilesColRow)
}





.extents_to_polygons <- function(extents){

  ps <- setNames(lapply(names(extents), function(extName){

    ext <- extents[[extName]]

    p <- rbind(
      c(ext@xmin, ext@ymin),
      c(ext@xmin, ext@ymax),
      c(ext@xmax, ext@ymax),
      c(ext@xmax, ext@ymin),
      c(ext@xmin, ext@ymin) )

    sp::Polygons(list(sp::Polygon(p)), extName)

  }), names(extents))

  for(i in 1:length(ps)) ps[[i]]@plotOrder <- i

  return(ps)
}



.nonoverlappingBuffers <- function(tileExt, buffExt, tilesColRow){

  neibrowcol <- expand.grid(c(-1,0,1), c(-1,0,1))[-5,]
  row.names(neibrowcol) <- c("topleft", "top", "topright", "left", "right", "bottomleft", "bottom", "bottomright")
  colnames(neibrowcol)  <- c("col", "row")

  neibgrid <- data.frame(
    corner = c("topleft", "topright", "bottomright", "bottomleft"),
    hor    = c("left", "right", "right", "left"),
    vert   = c("top", "top", "bottom", "bottom"),
    stringsAsFactors = FALSE)


  ps <- lapply(1:nrow(tilesColRow), function(tileNum){

    tileColRow <- as.numeric(tilesColRow[tileNum, c("col", "row")])
    tileEx     <- tileExt[[tileNum]]
    buffEx     <- buffExt[[tileNum]]

    # Get row/col of potential neighbors
    neibrowcol.tile <- as.data.frame(t(apply(neibrowcol, 1, function(r) r + tileColRow)))

    # Determine which neighbors exist
    neibrowcol.tile$exists <- utils::tail(duplicated(rbind(tilesColRow[,c("col", "row")], neibrowcol.tile)),8)
    neibgrid.tile <- cbind(neibgrid, data.frame(
      cornerExists = neibrowcol.tile[neibgrid$corner, "exists"],
      horExists    = neibrowcol.tile[neibgrid$hor,    "exists"],
      vertExists   = neibrowcol.tile[neibgrid$vert,   "exists"])
      )

    # Get positions of tile sides
    dims.tile <- data.frame(
      buff   = buffEx[],
      unbuff = tileEx[],
      diff   = buffEx[] - tileEx[],
      row.names = c("left", "right", "bottom", "top"))

    polyPts <- do.call(rbind, lapply(1:4, function(x){

      # Get corner
      crn <- neibgrid.tile[x,]

      dims.crn <- dims.tile[c(crn[,"hor"], crn[,"vert"]),]

      if(crn[,"horExists"] & crn[,"vertExists"]){

        # Straight corner
        return(dims.crn[cbind(1:2, as.numeric(c(crn[,"horExists"], crn[,"vertExists"])) + 1)])

      }else{

        if(crn[,"cornerExists"]){

          if(crn[,"horExists"]){
            horPt <- dims.crn[cbind(1:2, c(2,2))]
          }else{
            horPt <- dims.crn[cbind(1:2, c(1,2))] - c(0,dims.crn[crn[,"vert"], "diff"])
          }
          if(crn[,"vertExists"]){
            vertPt <- dims.crn[cbind(1:2, c(2,2))]
          }else{
            vertPt <- dims.crn[cbind(1:2, c(2,1))] - c(dims.crn[crn[,"hor"], "diff"],0)
          }
          if(crn[,"corner"] %in% c("topleft", "bottomright")){
            return(rbind(horPt, vertPt))
          }else{
            return(rbind(vertPt, horPt))
          }

        }else{

          # Straight corner
          return(dims.crn[cbind(1:2, as.numeric(c(crn[,"horExists"], crn[,"vertExists"])) + 1)])
        }
      }
    }))

    row.names(polyPts) <- 1:nrow(polyPts)
    return(sp::Polygons(list(sp::Polygon(polyPts)), ID = tilesColRow[tileNum, "tileName"]))

  })

  for(i in 1:length(ps)) ps[[i]]@plotOrder <- i

  return(ps)
}

