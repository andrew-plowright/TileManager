#' Tile Detector
#'
#' Function for detecting existing tiling scheme from a list of raster files or RasterLayers. This includes detecting buffers,
#' row and column positions, and re-ordering based on tile location.
#'
#' @param inRasters a list of RasterLayers
#' @param roundCoords numeric. Round input coordinates to this digit
#'
#' @export

tileDetector <- function(inRasters, roundCoords = 4){

  ### CHECK INPUTS ----

    if(length(inRasters) > 1){
      if(class(inRasters) == "list"){
        if(!all(sapply(inRasters, class) == "RasterLayer")){
          stop("Input list for 'inRasters' contains non-RasterLayer objects. Must be a vector of raster file paths or a list of RasterLayer objects.")
        }
      }
      if(class(inRasters) == "character"){
        inRasters <- try(lapply(inRasters, raster::raster), silent=TRUE)
        if(class(inRasters) == "try-error"){stop("Input vector of raster file paths for 'inRasters' contains one or more non-raster files.")}
      }
    }
    if(length(inRasters) == 1){
      stop("'inRasters' must contain multiple values: either a vector of raster file paths or a list of RasterLayer objects.")
    }
    if(length(inRasters) == 0){
      stop("'inRasters' contains no values. Must be a vector of raster file paths or a list of RasterLayer objects.")
    }

    # Check if all rasters have same projection
    if(length(unique(sapply(inRasters, raster::crs))) > 1){
      stop("'inRasters' contains rasters with conflicting projection systems")
    }


  ### READ IN DATA ----

    # Extract Extent objects and SpatialPolygons for each tile (assumed to be buffered)
    buffExt  <- stats::setNames(lapply(inRasters, raster::extent), 1:length(inRasters))
    buffPoly <- .extents_to_polygons(buffExt)
    buffSP   <- sp::SpatialPolygons(buffPoly)

    # Check if all tiles are contiguous
    if(length(rgeos::gUnaryUnion(buffSP)@polygons[[1]]@Polygons) > 1){
      stop("Orphaned tile(s) detected. All input tiles must be contiguous")
    }

  ### DETECT BUFFERS ----

    buffers <- do.call(rbind, lapply(1:length(buffExt), function(i){

      # Isolate a given tile extent
      tileExt <- buffExt[[i]]
      tileSP  <- sp::SpatialPolygons(buffPoly[i])

      # Determine tile's neighbours
      neibs <- setdiff(which(rgeos::gIntersects(tileSP, buffSP, byid = TRUE)), i)

      # Determine neighbour type (aligned on x-axis, y-axis, or other)
      neibs.x <- neibs[sapply(buffExt[neibs], function(neighbour) all(c(neighbour@xmin, neighbour@xmax) == c(tileExt@xmin, tileExt@xmax)))]
      neibs.y <- neibs[sapply(buffExt[neibs], function(neighbour) all(c(neighbour@ymin, neighbour@ymax) == c(tileExt@ymin, tileExt@ymax)))]
      neibs.other <- neibs[!neibs %in% c(neibs.x, neibs.y)]

      # Check if number of neighbours are as expected (up to 4 corner neighbours and up to 2 y/x neighbours)
      if(length(neibs.other) > 4) stop("Tile ", i, " has more than four diagonal neighbours")
      if(length(neibs.x)     > 2) stop("Tile ", i, " has more than four neighbours along the x-axis")
      if(length(neibs.y)     > 2) stop("Tile ", i, " has more than four neighbours along the y-axis")

      # Extract buffers
      buff.y <- unique(sapply(buffExt[neibs.x], function(neighbour){
        int <- raster::intersect(neighbour, tileExt)
        if(is.null(int)) 0 else round((int@ymax - int@ymin) / 2, roundCoords)
      }))

      buff.x <- unique(sapply(buffExt[neibs.y], function(neighbour){
        int <- raster::intersect(neighbour, tileExt)
        if(is.null(int)) 0 else round((int@xmax - int@xmin) / 2, roundCoords)
      }))

      # Check that buffers are consistent
      if(length(buff.x) > 1 | length(buff.y) > 1){stop("Tile ", i, " has an inconsistent buffer with its neighbouring tiles")}

      return(c(buff.x, buff.y))

    }))

    buffer <- unique(as.numeric(buffers))
    if(length(buffer) > 1) stop("Tiles have inconsistent buffer")

  ### GET ROW/COL INFO AND REORDER TILES, ----

    # Acquire centroids for each tile
    centroids <- rgeos::gCentroid(buffSP, byid = TRUE)@coords

    # Round
    centroids <- round(centroids, roundCoords)

    # Determine column and row positions
    tileData <- data.frame(
      row = as.numeric(ordered(-centroids[,"y"])),
      col = as.numeric(ordered( centroids[,"x"]))
    )

    # Set names
    paste0("R", tileData[,"row"], "C", tileData[,"col"]) ->
      tileData[,"tileName"] ->
      row.names(tileData) ->
      names(buffExt) ->
      names(buffPoly)
    sp::spChFIDs(buffSP) <- tileData$tileName
    for(i in 1:length(buffPoly)) buffPoly[[i]]@ID <- tileData$tileName[i]

    # Get file names for input rasters
    tileData[,"file"] <- sapply(inRasters, function(inRaster) basename(inRaster@file@name))

    # Reorder based on tile position
    ord <- order(tileData$row, tileData$col)
    tileData <- tileData[ord,]
    buffExt  <- buffExt[ord]
    buffSP   <- buffSP[ord,]
    buffPoly <- buffPoly[ord]
    for(i in 1:length(buffPoly)) buffPoly[[i]]@plotOrder <- as.integer(i)


  ### CREATE UNBUFFERED TILES ----

    if(buffer == 0){

      tileExt  <- buffExt
      tilePoly <- buffPoly

    }else{

      tileExt  <- lapply(buffExt, function(ext) ext - (buffer * 2))
      tilePoly <- .extents_to_polygons(tileExt)
    }


  ### CREATE NON-OVERLAPPING UNBUFFERED TILES ----

    nbuffPoly <- .nonoverlappingBuffers(tileExt, buffExt, tileData)


  ### RETURN OUTPUT ----

    new("tileScheme",
        tiles  = tilePoly,
        buffs  = buffPoly,
        nbuffs = nbuffPoly,
        buffer = buffer,
        crs    = raster::crs(inRasters[[1]]),
        data   = tileData)
}


