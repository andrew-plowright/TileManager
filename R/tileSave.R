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

    if(file.exists(filepath)){
      if(overwrite) unlink(filepath) else stop("Output file exists. Set 'overwrite' to TRUE")
    }
    if(tolower(tools::file_ext(filepath)) != "gpkg") stop("Output file should end with '.gpkg' extension")

  ### CONVERT TILE SCHEME TO SINGLE SPDF ----

    for(type in c("tiles", "buffs", "nbuffs")){
      sf::st_write(ts[[type]],  dsn = filepath, layer = type, delete_dsn = FALSE, delete_layer = TRUE, quiet = TRUE)
    }

    sf::st_write(data.frame("buffer" = ts@buffer),  dsn = filepath, layer = "metadata", delete_dsn = FALSE, delete_layer = TRUE, quiet = TRUE)
}
