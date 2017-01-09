#' Tile Input
#'
#' Takes either a single Raster object or paths to raster files and returns a list of Raster objects
#'
#' @export

  TileInput <- function(input, inputName){

    # If input for is a list, ensure that each list element is a RasterLayer object and that resolutions are identical
    if(class(input) == "list"){
      if(!all(sapply(input, class) == "RasterLayer")){stop("Invalid input list for \'", inputName ,"\'. List must be composed of tiled \'RasterLayer\' objects.")}

      if(nrow(unique(do.call(rbind, lapply(input, res)))) > 1){stop("Invalid input list for \'", inputName ,"\'. Inconsistent pixel resolution")}
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
#' Takes a list of Raster objects and returns a tiling scheme.
#'   If the list contains multiple Raster objects, use TileDetector to detect existing tiles
#'   If the list contains a single small Raster object, return a single tile
#'   If the list contains a large Raster object (more cells than maxCells)
#' @export

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
          singleTile.sp <- as(ext, "SpatialPolygons")
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
#' @export

TempTiles <- function(inRaster, tiles){

  # Create temporary folder to house raster tiles
  tileFolder <- paste0(tempdir(), "\\tileFolder")
  dir.create(tileFolder)

  # Apply tiling scheme and save raster tiles to temporary folder
  for(i in 1:length(tiles$buffPolygons)){
    tile <- suppressWarnings(raster::crop(inRaster, raster::extent(tiles$buffPolygons[i,])))
    raster::writeRaster(tile, paste0(tileFolder, "\\", as.character(tiles$buffPolygons[["tileName"]])[i], ".tif"), overwrite = TRUE)
  }

  # Read tile files
  return(lapply(tiles$buffPolygons[["tileName"]], function(tile) raster::raster(paste0(tileFolder, "\\", tile, ".tif"))))
}

#' Remove Temporary Tiles
#'
#' @export

removeTempTiles <- function(){
  tileFolder <- paste0(tempdir(), "\\tileFolder")
  if(file.exists(tileFolder)){unlink(tileFolder, recursive = TRUE)}
}
