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
#' @param tiledim numeric. Defines the 'x' and 'y' dimensions of each tile. By default, dimensions are in map
#' units. If `cells` is set to TRUE, then dimensions are in number of cells
#' @param cells logical. If set to TRUE, \code{tiledim} and \code{buffer} dimensions will be in number of cells instead of
#' map units
#' @param buffer numeric. If set to >0, overlapping buffers will be created around each tile
#' @param bufferspill logical. Default is \code{FALSE}, in which case the tiling grid will be pushed inwards so that the
#' buffers of the outer tiles are within the extent of \code{input}. If set to \code{TRUE}, the buffers will extend outside
#' of the extent of \code{input}
#' @param crs character. PROJ4 string defining output coordinate reference system (CRS). If set to NULL, the function will attempt to get
#' a CRS from \code{input} (only works if it is a raster). Set to NA to force the output to have no CRS.
#' @param origin numeric. Optional vector of two numbers corresponding to a pair of coordinates to which the tiling scheme will
#' be aligned. Cannot be used in conjunction with \code{cells}. The coordinates do not need to be within the extent of
#' \code{input}
#' @param removeEmpty logical. Default is \code{FALSE}. If set to \code{TRUE}, tiles containing only \code{NA} cell values
#' will be removed from the tiling scheme. Can only be used when \code{input} is a Raster object.
#'
#' @return a 'tileScheme' object
#'
#' @examples
#' \dontrun{
#' ts1 <- tileScheme(CHMdemo, tiledim = c(50,50))
#'
#' ts2 <- tileScheme(CHMdemo, tiledim = c(100,120), cells = TRUE)
#'
#' ts3 <- tileScheme(CHMdemo, tiledim = 40, buffer = 5, origin = c(0.5, 0.5))
#' }
#' @export

tileScheme <- function(input, tiledim, cells = FALSE,
                       buffer = 0, bufferspill = FALSE,
                       crs = NULL, origin = NULL, removeEmpty = FALSE){

  ### CHECK INPUTS ----

    # If "input" is a file path, attempt to read it as a 'raster' object
    if(is.character(input)){

      if(file.exists(input)){

        input <- try(raster::raster(input), silent=TRUE)
        if(class(input) == "try-error"){stop("Input path for \'input\' must direct to a raster file.")}

      }else stop("Invalid file path for \'input\'. File does not exist.")
    }

    # Get the extent of the input
    inext <- raster::extent(input)

    if(cells & !(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack"))){
      stop("If 'cells' is set to TRUE, 'input' must be a Raster object.")}

    if(removeEmpty & !(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack"))){
      stop("If 'removeEmpty' is set to TRUE, 'input' must be a Raster object.")}

    if(cells & !is.null(origin)){
      stop("If 'cells' is set to TRUE, the 'origin' argument cannot be used")}

    if(buffer < 0){stop("The value of 'buffer' cannot be negative.")}

    if(
      ( cells && buffer >= (min(c(nrow(input), ncol(input))) / 2)) |
      (!cells && buffer >= (min(c(inext@xmax - inext@xmin, inext@ymax - inext@ymin)) /2))

    ){stop("'buffer' cannot be equal to or larger than half of narrowest side of the input extent")}

    if(buffer >= (min(tiledim) / 2)){stop("'buffer' cannot be equal to or larger than half of the narrowest tile side")}

    # Extract projection from input
    crs <- raster::crs(
      if(!is.null(crs)) crs
      else if(class(input) %in% c("RasterLayer", "RasterBrick", "RasterStack")) input
      else ""
    )

    # If a single number is input to 'tiledim', repeat it
    if(length(tiledim) == 1) tiledim <- rep(tiledim, 2)


  ### DIMENSIONS BY DISTANCE ----

    if(!cells){

      # If there is a buffer and "bufferspill" is set to FALSE, shrink the input extent
      if(buffer != 0 & bufferspill == FALSE) inext <-  inext - buffer * 2

      ### CREATE SEQUENCE OF BREAKPOINTS

        # If no origin is set, then compute a sequence of breakpoints starting at the input's xmin and ymax
        if(is.null(origin)){

          xSeq <- seq(inext@xmin, inext@xmax, by = tiledim[1])
          if(xSeq[length(xSeq)] < inext@xmax) xSeq <- c(xSeq, inext@xmax)

          ySeq <- seq(inext@ymax, inext@ymin, by = -tiledim[2])
          if(ySeq[length(ySeq)] > inext@ymin) ySeq <- c(ySeq, inext@ymin)

        # If a 'origin' coordinate is set, compute a sequence of breakpoints from the origin value, subset those breakpoints
        # according to those that are within the input extent, and then start the sequence with the input's
        # xmin and ymax
        }else{

          xSeq <- seq(APfun::AProunder(inext@xmin, tiledim[1], "up",   snap = origin[1]),
                      APfun::AProunder(inext@xmax, tiledim[1], "down", snap = origin[1]),
                      by = tiledim[1])

          if(xSeq[1] > inext@xmin) xSeq <- c(inext@xmin, xSeq)
          if(xSeq[length(xSeq)] < inext@xmax) xSeq <- c(xSeq, inext@xmax)

          ySeq <- seq(APfun::AProunder(inext@ymin, tiledim[2], "up",   snap = origin[2]),
                      APfun::AProunder(inext@ymax, tiledim[2], "down", snap = origin[2]),
                      by = tiledim[2])

          if(ySeq[1] > inext@ymin) ySeq <- c(inext@ymin, ySeq)
          if(ySeq[length(ySeq)] < inext@ymax) ySeq <- c(ySeq, inext@ymax)

          # Reverse the order of the y Sequence (max to min)
          ySeq <- rev(ySeq)

        }

      # Assemble break points into intervals
      xInt <- data.frame(xmin = xSeq[1:(length(xSeq) - 1)], xmax = xSeq[2:length(xSeq)])
      yInt <- data.frame(ymin = ySeq[2:length(ySeq)],       ymax = ySeq[1:(length(ySeq) - 1)])

      # Create series of row and col numbers
      tilesRC <- expand.grid(col = 1:nrow(xInt), row = 1:nrow(yInt))[,2:1]
      tilesRC$tileName <- paste0("R", tilesRC[,"row"], "C", tilesRC[,"col"])
      row.names(tilesRC) <- tilesRC$tileName

      # Join all combinations of intervals
      tileInt <- do.call(rbind, lapply(1:nrow(tilesRC), function(x){

        cbind(xInt[tilesRC[x, "col"], ],
              yInt[tilesRC[x, "row"], ])
      }))

      # Apply buffer
      buffInt <- tileInt
      buffInt[,c("xmin", "ymin")] <- buffInt[,c("xmin", "ymin")] - buffer
      buffInt[,c("xmax", "ymax")] <- buffInt[,c("xmax", "ymax")] + buffer

      # Convert to Extent objects
      tileExt <- setNames(apply(tileInt, 1, raster::extent), tilesRC$tileName)
      buffExt <- setNames(apply(buffInt, 1, raster::extent), tilesRC$tileName)

    }


  ### DIMENSIONS BY NUMBER OF CELLS ----

    if(cells){

      # Convert buffer to map units
      bufferCells <- buffer
      buffer <- buffer * raster::res(input)[1]

      # Set extent of raster in terms of rows and columns
      rasdim <- c(colmin = 1, colmax = raster::ncol(input), rowmin = 1, rowmax = raster::nrow(input))

      # If there is a buffer and "bufferspill" is set to FALSE, shrink the input extent
      if(buffer != 0 & bufferspill == FALSE){
        rasdim[c("colmin", "rowmin")] <- rasdim[c("colmin", "rowmin")] + bufferCells
        rasdim[c("colmax", "rowmax")] <- rasdim[c("colmax", "rowmax")] - bufferCells}

      # Compute sequence of break points
      colSeq <- seq(rasdim["colmin"], rasdim["colmax"], by = tiledim[1])
      if((rasdim["colmax"] - rasdim["colmin"]) %% tiledim[1] != 0){colSeq <- c(colSeq, rasdim["colmax"])}
      rowSeq <- seq(rasdim["rowmin"], rasdim["rowmax"], by = tiledim[2])
      if((rasdim["rowmax"] - rasdim["rowmin"]) %% tiledim[2] != 0){rowSeq <- c(rowSeq, rasdim["rowmax"])}

      # Assemble break points into intervals
      colInt <-  data.frame(colmin = colSeq[1:(length(colSeq) - 1)],
                            colmax = colSeq[2:length(colSeq)] - c(rep(1, length(colSeq) - 2), 0))
      rowInt <-  data.frame(rowmin = rowSeq[1:(length(rowSeq) - 1)],
                            rowmax = rowSeq[2:length(rowSeq)] - c(rep(1, length(rowSeq) - 2), 0))

      # Create series of tile row and tile col numbers
      tilesRC <- expand.grid(col = 1:nrow(colInt), row = 1:nrow(rowInt))[,2:1]
      tilesRC$tileName <- paste0("R", tilesRC[,"row"], "C", tilesRC[,"col"])
      row.names(tilesRC) <- tilesRC$tileName

      # Join all combinations of intervals
      tileInt <- do.call(rbind, lapply(1:nrow(tilesRC), function(x){

        cbind(colInt[tilesRC[x, "col"], ],
              rowInt[tilesRC[x, "row"], ])
      }))

      # Convert to extent objects
      tileExt <- apply(tileInt, 1, function(tile){
        raster::extent(input, tile["rowmin"],  tile["rowmax"],  tile["colmin"],  tile["colmax"])})
      names(tileExt) <- tilesRC$tileName

      # Apply buffer
      buffExt <- lapply(tileExt, function(tile){tile + buffer * 2 })
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
      tilesRC <- tilesRC[!empties,]
    }


  ### CONVERT TO POLYGONS ----

    tilePoly  <- .extents_to_polygons(tileExt)
    buffPoly  <- .extents_to_polygons(buffExt)
    nbuffPoly <- .nonoverlappingBuffers(tileExt, buffExt, tilesRC)


  ### RETURN OUTPUT ----

    new("tileScheme",
        tiles  = tilePoly,
        buffs  = buffPoly,
        nbuffs = nbuffPoly,
        buffer = buffer,
        crs    = crs,
        data   = tilesRC)
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



.nonoverlappingBuffers <- function(tileExt, buffExt, tilesRC){

  neibrowcol <- expand.grid(c(-1,0,1), c(-1,0,1))[-5,]
  row.names(neibrowcol) <- c("topleft", "top", "topright", "left", "right", "bottomleft", "bottom", "bottomright")
  colnames(neibrowcol)  <- c("col", "row")

  neibgrid <- data.frame(
    corner = c("topleft", "topright", "bottomright", "bottomleft"),
    hor    = c("left", "right", "right", "left"),
    vert   = c("top", "top", "bottom", "bottom"),
    stringsAsFactors = FALSE)


  ps <- lapply(1:nrow(tilesRC), function(tileNum){

    tileColRow <- as.numeric(tilesRC[tileNum, c("col", "row")])
    tileEx     <- tileExt[[tileNum]]
    buffEx     <- buffExt[[tileNum]]

    # Get row/col of potential neighbors
    neibrowcol.tile <- as.data.frame(t(apply(neibrowcol, 1, function(r) r + tileColRow)))

    # Determine which neighbors exist
    neibrowcol.tile$exists <- utils::tail(duplicated(rbind(tilesRC[,c("col", "row")], neibrowcol.tile)),8)
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
    return(sp::Polygons(list(sp::Polygon(polyPts)), ID = tilesRC[tileNum, "tileName"]))

  })

  for(i in 1:length(ps)) ps[[i]]@plotOrder <- i

  return(ps)
}

