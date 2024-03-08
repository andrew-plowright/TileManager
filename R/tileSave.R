#' Save Tile Scheme
#'
#' Save a Tile Scheme to a SHP file.
#'
#' @param ts tileScheme
#' @param filepath file path
#' @param overwrite logical. Overwrite existing file
#'
#' @export

tileSave <- function(ts, filepath, overwrite = FALSE){

  ### CHECK INPUTS ----

    if(file.exists(filepath) & !overwrite) stop("Output file exists. Set 'overwrite' to TRUE")
    if(tolower(tools::file_ext(filepath)) != "shp") stop("Output file should end with '.shp' extension")


  ### WRITE METADATA ----

    # Set XML file path
    xmlpath <- paste0(filepath, ".xml")

    # Remove any existing file
    unlink(xmlpath)

    # Create XML document
    xmlDoc <- suppressWarnings(
      XML::newXMLDoc(
        node = XML::newXMLNode(
          "metadata",
          attrs = list(`xml:lang` = "en"),
          .children = list(
            newXMLNode = XML::newXMLNode(
              "tileManager",
              .children = list(
                XML::newXMLNode(
                  "buffer",
                  ts@buffer))
            )))))

    # Save XML file
    XML::saveXML( xmlDoc, file =  xmlpath, prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", indent = FALSE)


  ### CONVERT TILE SCHEME TO SINGLE SPDF ----

    # Modify IDs to allow merging
    for(i in 1:length(ts)){
      ts@tiles[[i]]@ID  <- paste0(ts@tiles[[i]]@ID, "_tile")
      ts@buffs[[i]]@ID  <- paste0(ts@buffs[[i]]@ID, "_buff")
      ts@nbuffs[[i]]@ID <- paste0(ts@nbuffs[[i]]@ID, "_nbuff")
    }

    # Merge to SpatialPolygonsDataFrame
    spoly <- sp::SpatialPolygons(c(ts@buffs, ts@nbuffs, ts@tiles))
    raster::crs(spoly) <- ts@crs
    sdata <-  rbind(
      cbind(ts@data, type = "buff"),
      cbind(ts@data, type = "nbuff"),
      cbind(ts@data, type = "tile")
    )
    row.names(sdata) <- sapply(spoly@polygons, slot, "ID")
    spdf <- sp::SpatialPolygonsDataFrame(spoly, sdata)


  ### SAVE ----

    APfun::APSHPsave(spdf, filepath, overwrite = overwrite)

}
