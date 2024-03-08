

#' Load Tile Scheme
#'
#' Load a Tile Scheme to a SHP file. The file needs to be originally saved using \code{tileSave}, since
#' some metadata (saved to an XML file) is required.
#'
#' @param filepath file path
#'
#' @export


tileLoad <- function(filepath){

  ### CHECK INPUTS ----

  if(!file.exists(filepath)) stop("File not found")

  ### READ METADATA ----

  # Set expected XML file path
  xmlpath <- paste0(filepath, ".xml")
  if(!file.exists(xmlpath)) stop("Could not find '", xmlpath, "'")

  # Get buffer from XML file
  xmlDoc <- XML::xmlTreeParse(xmlpath, useInternalNodes = TRUE)
  xmlBuff <- XML::getNodeSet(xmlDoc, "/metadata/tileManager/buffer")[[1]]
  buffer <- as.numeric(XML::xmlValue(xmlBuff))

  ### READ SHP FILE ----

  spdf <- APfun::APSHPread(filepath)

  if(any(!c("row", "col", "tileName", "type") %in% names(spdf))) stop("Invalid column headers")

  polys <- lapply(c("tile", "buff", "nbuff"), function(tp){

    s <- spdf[spdf[["type"]] == tp,]
    sp::spChFIDs(s) <- s$tileName
    p <- s@polygons
    names(p) <- s$tileName
    for(i in 1:length(p)) p[[i]]@plotOrder <- as.integer(i)
    return(p)

  })

  tileData <- spdf@data[spdf[["type"]] == "tile", c("row", "col", "tileName")]
  row.names(tileData) <- tileData$tileName

  ### RETURN TILE SCHEME ----

  new("tileScheme",
      tiles  = polys[[1]],
      buffs  = polys[[2]],
      nbuffs = polys[[3]],
      buffer = buffer,
      crs    = raster::crs(spdf),
      data   = tileData)

}
