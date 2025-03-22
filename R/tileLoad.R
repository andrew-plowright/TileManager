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

  file_ext <- tolower(tools::file_ext(filepath))

  if(file_ext == "shp"){

    tileLoad.shp(filepath)

  }else if(file_ext == "gpkg"){

    tileLoad.gpkg(filepath)

  }else stop("Unrecognized file extension:", file_ext)

}


tileLoad.gpkg <- function(filepath){

  ### READ DATA ----

  metadata <- suppressWarnings(sf::st_read(filepath, "metadata", quiet = TRUE))

  tiles  <- sf::st_read(filepath, layer = "tiles",  quiet = TRUE)
  buffs  <- sf::st_read(filepath, layer = "buffs",  quiet = TRUE)
  nbuffs <- sf::st_read(filepath, layer = "nbuffs", quiet = TRUE)

  if(!all(c(
    nrow(tiles) == nrow(buffs),
    nrow(tiles) == nrow(nbuffs),
    all(tiles$tile_name == buffs$tile_name),
    all(tiles$tile_name == nbuffs$tile_name)
  ))) stop("Geopackage layers did not match")

  sf::st_geometry(tiles) <- "tiles"

  out_sf <- dplyr::bind_cols(
    tiles,
    buffs = buffs[["geom"]],
    nbuffs = nbuffs[["geom"]]
  )

  row.names(out_sf) <- out_sf[["tile_name"]]

  ### RETURN TILE SCHEME ----

  new("tileScheme", sf = out_sf, buffer = metadata[,"buffer"])
}


tileLoad.shp <- function(filepath){

  ### CHECK INPUTS ----

  xml_path <- paste0(filepath, ".xml")

  if(!file.exists(xml_path)) stop("XML file not found")

  ### READ DATA ----


  # Get metadata
  xml_content <- xml2::read_xml(xml_path)
  buffer_value <- as.numeric(xml2::xml_text(xml2::xml_find_first(xml_content, "//buffer")))

  # Read SHP file
  in_sf <- sf::st_read(filepath, quiet=T)

  buffs<-in_sf[in_sf[["type"]] == "buff",]
  nbuffs<-in_sf[in_sf[["type"]] == "nbuff",]
  tiles<-in_sf[in_sf[["type"]] == "tile",]

  sf::st_geometry(tiles) <- "tiles"

  out_sf <- dplyr::bind_cols(
    tiles,
    buffs = buffs[["geometry"]],
    nbuffs = nbuffs[["geometry"]]
  )

  out_sf[["type"]] <- NULL

  names(out_sf)[names(out_sf) == "tileName"] <- "tile_name"

  row.names(out_sf) <- out_sf[["tile_name"]]

  ### RETURN TILE SCHEME ----

  new("tileScheme", sf = out_sf, buffer = buffer_value)

}

