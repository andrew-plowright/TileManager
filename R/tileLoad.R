

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
