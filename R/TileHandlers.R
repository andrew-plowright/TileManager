#' Tile Input
#'
#' Function for handling tile set inputs.
#'
#' Certain functions (ex.: \code{\link{TileApply}}) are built to handle tile sets in the form of a list
#' of RasterLayers. This function takes various types of input and returns such a list.
#' If the \code{input} is a vector of file paths, \code{TileInput} will read those paths and return
#' a list. If \code{input} is a single RasterLayer, it will return a list with a single element.
#'
#' @param input a list of RasterLayers (see \link[raster]{raster}), a single RasterLayer or a vector
#' of file paths directing to a series of raster files.
#' @param inputName character. A name for the input, used only in error messages in case of an invalid input
#'
#' @return a list of RasterLayers
#'

  TileInput <- function(input, inputName){

    # If input is a list, ensure that each list element is a RasterLayer object and that resolutions are identical
    if(class(input) == "list"){
      if(!all(sapply(input, class) == "RasterLayer")){stop("Invalid input list for \'", inputName ,"\'. List must be composed of tiled \'RasterLayer\' objects.")}

      raster::compareRaster(input, extent = FALSE, orig = TRUE, rowcol = FALSE, crs = TRUE, res = TRUE)
    }

    # If input is a character vector ensure that it refers to valid file path(s) to raster layers
    if(class(input) == "character"){
      if(all(file.exists(input))){
        input <- try(lapply(input, raster::raster), silent=TRUE)
        if(class(input) == "try-error"){stop("Input vector of raster file paths for \'", inputName ,"\' contains a non-raster file.")}
      }else{
        stop("Invalid file path for \'", inputName ,"\'")
      }
    }

    # If input is a single RasterLayer, convert it to a list with one element
    if(class(input) == "RasterLayer"){
      input <- list(input)
    }

    # Return error if input is anything other than a RasterLayer, a list of RasterLayers, or a path to a raster
    if(!class(input) %in% c("character", "list", "RasterLayer")){stop("Invalid input for \'", inputName ,"\'.")}

    return(input)
  }

#' Tile Apply
#'
#' A function for applying a tile scheme if an input raster exceeds a given number of cells.
#'
#' Tiling schemes are frequently used to break up rasters that are too large to be read into memory.
#' This function takes a value for \code{maxCells}, and breaks up any raster that exceeds that cell
#' count into tiles using \code{\link{TileScheme}}. The tiles themselves have a number of cells equal
#' to \code{maxCells}, and can also be given buffers using the \code{tileBuffer} argument. If the
#' input raster does not exceed \code{maxCells}, then a tiling scheme with a single tile will be
#' returned.
#'
#' If a list of RasterLayers is given that already correspond to a set of tiles,
#' \code{\link{TileDetector}} will be used to detect the existing tiling scheme.
#'
#' Input for \code{TileApply} should be a list of RasterLayers, and so would typically be used
#' in conjuction with \code{\link{TileInput}}.
#'
#' @param inList a list of RasterLayers. Either a single raster (for which a tiling scheme may or may
#' not be applied) or a list of existing tiles (for which a tiling scheme will be detected)
#' @param maxCells numeric. The maximum number of cells that an input raster may have before it is broken
#' up into multiple tiles
#' @param tileBuffer numeric. Number of buffer cells to be applied to tiles
#' @param verbose logical. Report progress
#'
#' @seealso \code{\link{TileInput}} \code{\link{TileScheme}} \code{\link{TileDetector}}
#'
#' @return The output of this function is a list of three \link[sp]{SpatialPolygonsDataFrame} objects:
#'   \item{tilePolygons}{The tiling grid. Each polygon corresponds to the extent of a single unbuffered tile.}
#'   \item{buffPolygons}{The buffered tiling grid. Each polygon corresponds to the extent of a buffered tile. These
#'   polygons overlap with neighbouring tiles. If \code{buffer} is set to 0, this output will be identical to \code{tilePolygons}.}
#'   \item{nbuffPolygons}{Non-overlapping buffered tiles. These polygons remove overlapping buffers for adjacent tiles, but
#'   preserve buffers for tiles on the edges of the tiling grid. Useful for "reassembling" data that had been originally broken
#'   into tiles.}
#'

  TileApply <- function(inList, maxCells, tileBuffer = 0, verbose = FALSE){

    if(length(inList) > 1){

      if(verbose) cat("Multiple tiles detected", "\n")

      # Detect tiling scheme
      tiles <- TileDetector(inList)

    }else{

      if(raster::ncell(inList[[1]]) > maxCells){

        if(verbose) cat("Input raster dataset exceeds maximum cell count of", maxCells, ". Applying tiling scheme", "\n")

        # Create tiling scheme
        tiles <- TileScheme(inList[[1]], dimByCell = rep(round(sqrt(maxCells)), 2), buffer = tileBuffer)

      }else{

        if(verbose) cat("Single raster dataset detected. No tiling applied", "\n")

        # Create tiling scheme for a single tile
        createSingleTile <- function(ext, ras){
          singleTile.sp <- methods::as(ext, "SpatialPolygons")
          singleTile.sp@polygons[[1]]@ID <- "C1R1"
          singleTile.spdf <- sp::SpatialPolygonsDataFrame(singleTile.sp, data.frame(row.names = "C1R1", tileName = "C1R1", col = 1, row = 1, file = basename(ras@file@name)))
          raster::crs(singleTile.spdf) <- raster::crs(ras)
          return(singleTile.spdf)
        }
        singleTile.buff <- createSingleTile(raster::extent(inList[[1]]), inList[[1]])
        singleTile.unbuff <- createSingleTile(raster::extent(inList[[1]]) - tileBuffer * 2, inList[[1]])

        tiles <- list(tilePolygons = singleTile.unbuff,
                      buffPolygons = singleTile.buff,
                      nbuffPolygons = singleTile.buff)
      }
    }
  }

#' Generate Temporary Tiles
#'
#' Applies a tiling scheme to a RasterLayer and saves tiles to a temporary folder.
#'
#' @param inRaster RasterLayer (see \link[raster]{raster})
#' @param tiles a tile scheme generated using \code{\link{TileScheme}}
#'

TempTiles <- function(inRaster, tiles){

  # Create temporary folder to house raster tiles
  tileFolder <- file.path(tempdir(), "tileFolder")
  dir.create(tileFolder)

  # Apply tiling scheme and save raster tiles to temporary folder
  for(i in 1:length(tiles$buffPolygons)){

    tile <- suppressWarnings(raster::crop(inRaster, raster::extent(tiles$buffPolygons[i,])))

    raster::writeRaster(tile, file.path(tileFolder, paste0(as.character(tiles$buffPolygons[["tileName"]])[i], ".tif")), overwrite = TRUE)
  }

  # Read tile files
  return(lapply(tiles$buffPolygons[["tileName"]], function(tile) raster::raster(file.path(tileFolder, paste0(tile, ".tif")))))
}

#' Remove Temporary Tiles
#'
#' Deletes temporary tiles created by \code{\link{TempTiles}}
#'

removeTempTiles <- function(){
  tileFolder <- file.path(tempdir(), "tileFolder")

  if(file.exists(tileFolder)){unlink(tileFolder, recursive = TRUE)}
}
