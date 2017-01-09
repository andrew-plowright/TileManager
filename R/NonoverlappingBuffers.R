#' Non-overlapping buffers
#'
#' @export

NonoverlappingBuffers <- function(buffPolys, unbuffPolys){

  if(any(buffPolys[["col"]] != unbuffPolys[["col"]])){stop("Non-matching")}
  if(any(buffPolys[["row"]] != unbuffPolys[["row"]])){}

  tilesColRow <- buffPolys@data[,c("col", "row")]

  neibrowcol <- expand.grid(c(-1,0,1), c(-1,0,1))[-5,]
  row.names(neibrowcol) <- c("topleft", "top", "topright", "left", "right", "bottomleft", "bottom", "bottomright")
  colnames(neibrowcol) <- c("col", "row")

  neibgrid <- data.frame(corner = c("topleft", "topright", "bottomright", "bottomleft"),
                         hor = c("left", "right", "right", "left"),
                         vert = c("top", "top", "bottom", "bottom"),
                         stringsAsFactors = FALSE)

  nbuffPolys <- sp::SpatialPolygons(lapply(1:nrow(tilesColRow), function(tileNum){

    tileColRow <- as.numeric(tilesColRow[tileNum,])
    buffPoly <- buffPolys[tileNum,]
    unbuffPoly <- unbuffPolys[tileNum,]

    # Get row/col of potential neighbours
    neibrowcol.tile <- as.data.frame(t(apply(neibrowcol, 1, function(r) r + tileColRow)))

    # Determine which neighbours exist
    neibrowcol.tile$exists <- tail(duplicated(rbind(tilesColRow, neibrowcol.tile)),8)
    neibgrid.tile <- cbind(neibgrid, data.frame(cornerExists = neibrowcol.tile[neibgrid$corner,"exists"],
                                                horExists = neibrowcol.tile[neibgrid$hor,"exists"],
                                                vertExists = neibrowcol.tile[neibgrid$vert,"exists"]))
    # Get positions of tile sides
    dims.tile <- data.frame(buff = raster::extent(buffPoly)[],
                            unbuff = raster::extent(unbuffPoly)[],
                            diff =  raster::extent(buffPoly)[] - raster::extent(unbuffPoly)[],
                            row.names = c("left", "right", "bottom", "top"))

    polyPts <- do.call(rbind, lapply(1:4, function(x){

      # Get corner
      crn <- neibgrid.tile[x,]

      dims.crn <- dims.tile[c(crn[,"hor"], crn[,"vert"]),]

      if(crn[,"horExists"] & crn[,"vertExists"]){

        # Straight corner
        return(dims.crn[cbind(1:2, as.numeric(c(crn[,"horExists"], crn[,"vertExists"])) + 1)])

      }else{

        if(crn[,"cornerExists"]){

          if(crn[,"horExists"]){
            horPt <- dims.crn[cbind(1:2, c(2,2))]
          }else{
            horPt <- dims.crn[cbind(1:2, c(1,2))] - c(0,dims.crn[crn[,"vert"], "diff"])
          }
          if(crn[,"vertExists"]){
            vertPt <- dims.crn[cbind(1:2, c(2,2))]
          }else{
            vertPt <- dims.crn[cbind(1:2, c(2,1))] - c(dims.crn[crn[,"hor"], "diff"],0)
          }
          if(crn[,"corner"] %in% c("topleft", "bottomright")){
            return(rbind(horPt, vertPt))
          }else{
            return(rbind(vertPt, horPt))
          }

        }else{

          # Straight corner
          return(dims.crn[cbind(1:2, as.numeric(c(crn[,"horExists"], crn[,"vertExists"])) + 1)])
        }
      }
    }))

    row.names(polyPts) <- 1:nrow(polyPts)
    return(sp::Polygons(list(sp::Polygon(polyPts)), ID = as.character(row.names(unbuffPoly@data))))
  }))
  nbuffPolys <- sp::SpatialPolygonsDataFrame(nbuffPolys, unbuffPolys@data)
  raster::crs(nbuffPolys) <- raster::crs(buffPolys)

  return(nbuffPolys)}
