

tileScheme_valid <- function(object){

  checks <- logical()

  # 'tiles', 'buffs' and 'nbuffs' are same length
  checks["tileNumber"] <- var(sapply(list(object@tiles, object@buffs, object@nbuffs), length)) == 0
  if(!checks["tileNumber"]) stop("The tile scheme's 'tiles', 'buffs' and 'nbuffs' are not the same length")

  # Number of data rows is equal to number of tiles
  checks["dataRows"] <- length(object@tiles) == nrow(object@data)
  if(!checks["dataRows"]) stop("Number of tiles and data rows does not match")

  # Tile names match as they should
  checks["tileNames"] <- all(names(object@tiles) == names(object@buffs)) & all(names(object@tiles) == names(object@nbuffs))
  if(!checks["tileNames"]) stop("Tile names for do for 'tiles', 'buffs' and 'nbuffs' do not match")
  checks["tileNames2"] <-   all(names(object@tiles) == row.names(object@data))
  if(!checks["tileNames2"]) stop("Tile names do not match data row.names")
  checks["tileNames3"] <-   all(names(object@tiles) == object@data$tileName)
  if(!checks["tileNames3"]) stop("'TileName' field does not match data row.names")

  return(all(checks))
}

#' Tile Scheme class
#'#'
#' @importClassesFrom sp CRS
#'
#' @export

setClass('tileScheme',
         representation(
           tiles  = 'list',
           buffs  = 'list',
           nbuffs = 'list',
           crs    = 'CRS',
           buffer = 'numeric',
           data   = 'data.frame'
         ),
         validity = tileScheme_valid
        )
