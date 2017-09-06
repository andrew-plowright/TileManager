#' Tile Detector
#'
#' Function for detecting existing tiling scheme from a list of RasterLayers.
#'
#' @param inRasters a list of RasterLayers (see \link[raster]{raster})
#' @param reord logical. If set to \code{FALSE}, tiles will be stored in the same order in which
#' they appear in \code{inRasters}. If \code{TRUE}, tiles will be reordered by column and then by row.
#'
#' @return The output of this function is a list of three \link[sp]{SpatialPolygonsDataFrame} objects:
#'   \item{tilePolygons}{The tiling grid. Each polygon corresponds to the extent of a single unbuffered tile.}
#'   \item{buffPolygons}{The buffered tiling grid. Each polygon corresponds to the extent of a buffered tile. These
#'   polygons overlap with neighboring tiles. If \code{buffer} is set to 0, this output will be identical to \code{tilePolygons}.}
#'   \item{nbuffPolygons}{Non-overlapping buffered tiles. These polygons remove overlapping buffers for adjacent tiles, but
#'   preserve buffers for tiles on the edges of the tiling grid. Useful for "reassembling" data that had been originally broken
#'   into tiles.}
#'
#' @export

TileDetector <- function(inRasters, reord = FALSE){

  ### GATE KEEPER

    if(length(inRasters) > 1){
      if(class(inRasters) == "list"){
        if(!all(sapply(inRasters, class) == "RasterLayer")){
          stop("Input list for \'inRasters\' contains non-RasterLayer objects. Must be a vector of raster file paths or a list of RasterLayer objects.")
        }
      }
      if(class(inRasters) == "character"){
        inRasters <- try(lapply(inRasters, raster::raster), silent=TRUE)
        if(class(inRasters) == "try-error"){stop("Input vector of raster file paths for \'inRasters\' contains one or more non-raster files.")}
      }
    }
    if(length(inRasters) == 1){
      stop("\'inRasters\' must contain multiple values: either a vector of raster file paths or a list of RasterLayer objects.")
    }
    if(length(inRasters) == 0){
      stop("\'inRasters\' contains no values. Must be a vector of raster file paths or a list of RasterLayer objects.")
    }

    # Check if all rasters have same projection
    if(length(unique(sapply(inRasters, raster::crs))) > 1){
      stop("\'inRasters\' contains rasters with conflicting projection systems")
    }

  ### HELPER FUNCTIONS

    # Function that takes a list of SpatialPolygons, extracts first 'polygons' from each list element, and reassembles
    # into a new SpatialPolygons object
    reassembleSP <- function(inList, IDs){
      mergePoly <- lapply(inList, function(e) e@polygons[[1]])
      for(i in 1:length(IDs)){mergePoly[[i]]@ID <- as.character(IDs[i])}
      return(sp::SpatialPolygons(mergePoly))
    }

  ### PRE-PROCESS: EXTRACT RASTER SPATIAL POLYGONS FOR EACH TILE

    # Extract Extent objects and SpatialPolygons for each tile (assumed to be buffered)
    buffs.ext <- lapply(inRasters, raster::extent)
    buffs.sp <- lapply(buffs.ext, methods::as, "SpatialPolygons")

    # Check if all tiles are connected
    if(length(rgeos::gUnaryUnion(reassembleSP(buffs.sp, IDs = 1:length(buffs.sp)))@polygons[[1]]@Polygons) > 1){
      stop("Orphaned tile(s) detected. All input tiles must be connected")
    }

  ### PROCESS: DETECT BUFFERS

    buff.val <- do.call(rbind, lapply(1:length(buffs.ext), function(i){

      # Isolate a given tile extent
      tile.ext <- buffs.ext[[i]]
      tile.sp <- buffs.sp[[i]]

      # Determine tile's neighbours
      neibs <- setdiff(which(sapply(buffs.sp, function(neighbour) rgeos::gIntersects(neighbour, tile.sp))), i)

      # Determine neighbour type (aligned on x-axis, y-axis, or other)
      neibs.x <- neibs[sapply(buffs.ext[neibs], function(neighbour) all(c(neighbour@xmin, neighbour@xmax) == c(tile.ext@xmin, tile.ext@xmax)))]
      neibs.y <- neibs[sapply(buffs.ext[neibs], function(neighbour) all(c(neighbour@ymin, neighbour@ymax) == c(tile.ext@ymin, tile.ext@ymax)))]
      neibs.other <- neibs[!neibs %in% c(neibs.x, neibs.y)]

      # Check if number of neighbours are as expected (up to 4 corner neighbours and up to 2 y/x neighbours)
      if(!length(neibs.other) %in% 0:4){
        stop("No tiling scheme detected")}
      if(!length(neibs.x) %in% 0:2 | !length(neibs.y) %in% 0:2){
        stop("No tiling scheme detected")}

      # Extract buffers
      buff.y <- unique(sapply(buffs.ext[neibs.x], function(neighbour){
        int <- raster::intersect(neighbour, tile.ext)
        if(is.null(int)){
          return(0)
        }else{
          return((int@ymax - int@ymin) / 2)
        }
      }))
      buff.x <- unique(sapply(buffs.ext[neibs.y], function(neighbour){
        int <- raster::intersect(neighbour, tile.ext)
        if(is.null(int)){
          return(0)
        }else{
          return((int@xmax - int@xmin) / 2)
        }
      }))

      # Check that buffers are consistent
      if(length(buff.x) > 1 | length(buff.y) > 1){stop("Inconsistent buffer. No tiling scheme detected")}

      return(c(buff.x, buff.y))

    }))
    buff.val <- unique(buff.val)
    if(nrow(buff.val) > 1){
      stop("Inconsistent buffer. No tiling scheme detected")
    }else{
      buff.val <- as.numeric(buff.val)
    }

  ### PROCESS: REORDER TILES, GET COL/ROW INFO AND CONVERT TO SPDF

    # Acquire centroids for each tile
    centroids <- do.call(rbind, lapply(buffs.sp, function(ext) rgeos::gCentroid(ext)@coords))

    # If needed, reorder tiles based on centroid position
    if(reord){
      ord <- order(centroids[,1], -centroids[,2])
    }else{
      ord <- 1:nrow(centroids)
    }
    tileData <- as.data.frame(centroids[ord,])
    row.names(tileData) <- 1:nrow(tileData)
    buffs.ext <- buffs.ext[ord]
    buffs.sp <- buffs.sp[ord]

    # Determine column and row positions
    tileData[,"col"] <- as.numeric(ordered(tileData[,"x"]))
    tileData[,"row"] <- as.numeric(ordered(-tileData[,"y"]))
    tileData[,"tileName"] <- paste0("C", tileData[,"col"], "R", tileData[,"row"])
    row.names(tileData) <- tileData[,"tileName"]

    # Get file names for input rasters
    tileData[,"file"] <- sapply(inRasters, function(inRaster) basename(inRaster@file@name))[ord]

    # Convert to SpatialPolygonsDataFrame
    buffs.spdf <- sp::SpatialPolygonsDataFrame(reassembleSP(buffs.sp, row.names(tileData)), tileData[,c("tileName", "col", "row", "file")])
    raster::crs(buffs.spdf) <- raster::crs(inRasters[[1]])

  ### PROCESS: CREATE UNBUFFERED TILES

    if(all(buff.val == 0)){
      unbuffs.sp <- buffs.sp
    }else{
      unbuffs.ext <- lapply(buffs.ext, function(ext){
        ext@xmin <- ext@xmin + buff.val[1]
        ext@xmax <- ext@xmax - buff.val[1]
        ext@ymin <- ext@ymin + buff.val[2]
        ext@ymax <- ext@ymax - buff.val[2]
        return(ext)})
      unbuffs.sp <- lapply(unbuffs.ext, methods::as, "SpatialPolygons")
    }

    # Convert to SpatialPolygonsDataFrame
    unbuffs.spdf <- sp::SpatialPolygonsDataFrame(reassembleSP(unbuffs.sp, row.names(tileData)), tileData[,c("tileName", "col", "row", "file")])
    raster::crs(unbuffs.spdf) <- raster::crs(inRasters[[1]])

  ### PROCESS: CREATE NON-OVERLAPPING UNBUFFERED TILES

    nbuffs.spdf <- NonoverlappingBuffers(buffs.spdf, unbuffs.spdf)

  ### OUTPUT

    # Assemble into output list
    outList <- list(unbuffs.spdf, buffs.spdf, nbuffs.spdf)
    names(outList) <- c("tilePolygons", "buffPolygons", "nbuffPolygons")
    class(outList) <- "tileScheme"
    return(outList)
    }
