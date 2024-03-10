#' Tile Scheme class
#'#'
#' @importClassesFrom sp CRS
#'
#' @export

setClass(
  'tileScheme',
  representation(
    sf    = 'sf',
    buffer = 'numeric'
  ))

#' Subset
#'
#' Subset tiles using the single bracket operator. Subset geometry (tiles, buffs or nbuffs) using the double brackets
#'
#' @param x a 'tileScheme' object
#' @param i,j,... indices specifying elements to extract
#' @param drop,exact arguments not used for 'tileScheme'
#'
#' @rdname subset
#' @export
setMethod(
  "[", signature(x = "tileScheme", i = "character"),
  function(x, i, j, ..., drop = TRUE) {

    if(!missing(j)) stop("Cannot use second index when subsetting using tile name")

    w <- match(i, x@sf$tile_name)

    if(any(is.na(w))) stop("Could not find input tile name")

    .subset_ts(x, w)

  })

#' @rdname subset
#' @export
setMethod(
  "[", signature(x = "tileScheme", i = "numeric", j = "numeric"),
  function(x, i, j, ..., drop = TRUE) {

    if(any(!j %in% x@sf$col)) stop("Column number is out of bounds")
    if(any(!i %in% x@sf$row)) stop("Row number is out of bounds")

    w <- which(x@sf$col %in% j & x@sf$row %in% i)

    .subset_ts(x, w)

  })

#' @rdname subset
#' @export
setMethod(
  "[", signature(x = "tileScheme", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {

    theCall <- sys.call(-1)
    narg <- length(theCall) - length(match.call(call=sys.call(-1)))

    # [i,] : select row
    w <- if(narg > 0){

      if(any(!i %in% x@sf$row)) stop("Row number is out of bounds")
      which(x@sf$row %in% i)

      # [i] : select tile number
    }else{

      if(any(!i %in% 1:nrow(x@sf))) stop("Tile number index is out of bounds")

      i
    }

    .subset_ts(x, w)
  })


#' @rdname subset
#' @export
setMethod(
  "[", signature(x = "tileScheme", i = "logical", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {

    theCall <- sys.call(-1)
    narg <- length(theCall) - length(match.call(call=sys.call(-1)))

    # [i,]
    w <- if(narg > 0){

      stop("Logical indices not accepted for row selection")

    # [i]
    }else{

      if(length(i) != length(x)) stop("Logical index of incorrect length")

      which(i)
    }

    .subset_ts(x, w)
  })



#' @rdname subset
#' @export
setMethod(
  "[", signature(x = "tileScheme", i = "missing", j = "numeric"),
  function(x, i, j, ..., drop = TRUE) {

    if(any(!j %in% x@sf$col)) stop("Column number is out of bounds")

    w <- which(x@sf$col %in% j)

    .subset_ts(x, w)

  })


#' @rdname subset
#' @export
setMethod(
  "[[", signature(x = "tileScheme", i = "character", j = "missing"),

  function(x, i, j, ..., exact=TRUE) {

    if(length(i) != 1 || !i %in% c('tiles', 'buffs', 'nbuffs', 'tile_name')){
      stop("Select one of the following: 'tiles', 'buffs', 'nbuffs', or 'tile_name")
    }

    if(i == "tile_name"){
      x@sf[["tile_name"]]
    }else{
      sf::st_geometry(x@sf) <- i
      x@sf[,c('row', 'col', 'tile_name')]
    }

  })

.subset_ts <- function(x, w){

  if(any(duplicated(w))) stop("Cannot take duplicated indices")

  x@sf <- x@sf[w,]

  return(x)
}


#' Get and set data
#'
#' @param x a 'tileScheme' object
#' @param name name of data column
#' @param value vector of new data values
#'
#' @rdname getdata
#' @export

setMethod(
  "$", "tileScheme", function(x, name) sf::st_drop_geometry(x@sf)[[name]])


#' Show
#'
#' Print information about a 'tileScheme' object
#'
#' @param object a 'tileScheme' object
#' @export

setMethod(
  "show", "tileScheme", function(object){

    cat(
      "class     : tileScheme", "\n",
      "extent    : ", paste(round(sf::st_bbox(object@sf$nbuffs),5), collapse = ", "),  " (xmin, ymin, xmax, ymax)", "\n",
      "CRS       : ", sf:::crs_parameters(sf::st_crs(object@sf$tiles) )$Name, "\n",
      "tiles     : ", nrow(object@sf), "\n",
      "nrow/ncol : ", length(unique(object@sf$row)), ",", length(unique(object@sf$col)), "\n",
      "buffer    : ", object@buffer, "\n",
      sep = ""
    )

  })


#' @export
st_crs.tileScheme = function(x, ...) sf::st_crs(x@sf)


#' Plot
#'
#' Plot a 'tileScheme' object
#'
#' @param x a 'tileScheme' object
#' @param y argument not used for 'tileScheme' objects
#' @param labels logical. Add row and column labels
#' @param add logical. Plot 'tileScheme' on top of existing plot
#' @param ... arguments t be passed to methods
#'
#' @export
#' @method plot tileScheme

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @rdname plot
#' @export

setMethod("plot", "tileScheme", function(x, labels = TRUE, add = FALSE, ...){

  plot(sf::st_geometry(x[["buffs"]]),  border = "red",  lty = 2, add = add)
  plot(sf::st_geometry(x[["tiles"]]),  border = "blue", lwd = 2, add = TRUE)
  plot(sf::st_geometry(x[["nbuffs"]]), border = "purple",        add = TRUE)

  # if(labels){
  #
  #   r <- x@sf[!duplicated(x$row), ]
  #   c <- x@sf[!duplicated(x$col), ]
  #
  #   for(rl in r$tile_name) text(par()$usr[1], x@tiles[[rl]]@labpt[2], paste0("R",  r[rl,][["row"]]), cex = 0.8, col = "darkgrey", pos = 4, offset = 0.1)
  #   for(cl in c$tile_name) text(x@tiles[[cl]]@labpt[1], par()$usr[3], paste0("C",  r[cl,][["col"]]), cex = 0.8, col = "darkgrey", pos = 3, offset = 0.1)
  #
  # }
})



#' Length
#'
#' Get the number of a cells contained in a 'tileScheme' object
#'
#' @param x 'tileScheme' object
#'
#' @export

setMethod("length", "tileScheme", function(x) nrow(x@sf))

#' Tile Scheme
#'
#' This function aims to provide an all-in-one tool for creating tiling schemes, which includes options for overlapping
#' buffers and methods for describing tile sizes in various ways (i.e. using either distance units or cell numbers).
#'
#' @section Non-overlapping buffers:
#'
#' When processing a tiled dataset, using buffered tiles can help remove the edge effects along the
#' individual tile borders. However, overlapping buffers generally need to be removed when recombining
#' a series of tiles back into a single raster. Although this can be accomplished by using the unbuffered
#' tile extent, this will also remove the buffered areas along the edge of the tile set. Once these
#' unbuffered tiles are reassembled, the resulting raster will then be smaller than the original
#' dataset before it was tiled.
#'
#' This may not be a desirable result. The polygons located in the \code{nbuffs} slot will produce a set of
#' polygons that correspond to the tile extents that conserve buffers only where they do not overlap onto
#' neighboring tiles (i.e.: along the edge of the tile set). These polygons are useful for cropping out
#' overlapping areas from buffered tiles in order to reassemble the tiles into a single raster.
#'
#'
#' @param x an object from which a SpatExtent can be extracted (SpatRaster, SpatVector, vector of numbers)
#' @param dim numeric. Defines the 'x' and 'y' dimensions of each tile. By default, dimensions are in map
#' units. If `cells` is set to TRUE, then dimensions are in number of cells
#' @param cells logical. If set to TRUE, \code{dim} and \code{buffer} dimensions will be in number of cells instead of
#' map units
#' @param buffer numeric. If set to >0, overlapping buffers will be created around each tile
#' @param spill logical. Default is \code{FALSE}, in which case the tiling grid will be pushed inwards so that the
#' buffers of the outer tiles are within the extent of \code{x}. If set to \code{TRUE}, the buffers will extend outside
#' of the extent of \code{x}
#' @param round numeric. Round the extent of the input Extent to the number of digits specified here.
#' @param round_dir character. The direction of the rounding, either \code{in} for inwards or \code{out} for outwards.
#' @param crs character. PROJ4 string defining output coordinate reference system (CRS). If set to NULL, the function will attempt to get
#' a CRS from \code{x} (only works if it is a raster). Set to NA to force the output to have no CRS.
#' @param origin numeric. Optional vector of two numbers corresponding to a pair of coordinates to which the tiling scheme will
#' be aligned. Cannot be used in conjunction with \code{cells}. The coordinates do not need to be within the extent of
#' \code{x}
#' @param remove_empty logical. Default is \code{FALSE}. If set to \code{TRUE}, tiles containing only \code{NA} cell values
#' will be removed from the tiling scheme. Can only be used when \code{x} is a Raster object.
#'
#' @return a 'tileScheme' object
#'
#' @examples
#' \dontrun{
#' ts1 <- tileScheme(CHMdemo, dim = c(50,50))
#'
#' ts2 <- tileScheme(CHMdemo, dim = c(100,120), cells = TRUE)
#'
#' ts3 <- tileScheme(CHMdemo, dim = 40, buffer = 5, origin = c(0.5, 0.5))
#' }
#' @export

tileScheme <- function(x, dim, cells = FALSE,
                       buffer = 0, spill = FALSE,
                       round = NA, round_dir = "out",
                       crs = NULL, origin = NULL, remove_empty = FALSE){

  ### CHECK INPUTS ----

    # Get the extent of the input
    inext <- terra::ext(x)

    # Get input class
    inclass <- class(x)[1]

    # Round extent
    if(is.numeric(round)){

      if(round <= 0) stop("'round' argument must be positive")
      if(!round_dir %in% c('in', 'out')) stop("'round_dir' argument must be set to 'in' or 'out'")

      inext <- .ext_round(inext, interval = round, direction = round_dir, snap = 0)
    }

    if(cells & (inclass != "SpatRaster")){
      stop("If 'cells' is set to TRUE, 'x' must be a 'SpatRaster' object.")}

    if(remove_empty & !(inclass %in% c("sf", "SpatRaster"))){
      stop("If 'remove_empty' is set to TRUE, 'x' must be a 'sf' object")}

    if(cells & !is.null(origin)){
      stop("If 'cells' is set to TRUE, the 'origin' argument cannot be used")}

    if(buffer < 0){stop("The value of 'buffer' cannot be negative.")}

    if(
      ( cells && buffer >= (min(c(nrow(x), ncol(x))) / 2)) |
      (!cells && buffer >= (min(c(inext[2] - inext[1], inext[4] - inext[3])) /2))

    ){stop("'buffer' cannot be equal to or larger than half of narrowest side of the input extent")}

    if(buffer >= (min(dim) / 2)){stop("'buffer' cannot be equal to or larger than half of the narrowest tile side")}

    # Extract projection from input
    if(is.null(crs)) crs <- terra::crs(x)

    # If a single number is input to 'dim', repeat it
    if(length(dim) == 1) dim <- rep(dim, 2)

    if(!is.null(origin) && length(origin) != 2) stop("'origin' should be a vector of two numbers")

  ### DIMENSIONS BY DISTANCE ----

    if(!cells){

      # If there is a buffer and "spill" is set to FALSE, shrink the input extent
      if(buffer != 0 & spill == FALSE) inext <-  inext - buffer

      ### CREATE SEQUENCE OF BREAKPOINTS

        # If no origin is set, then compute a sequence of breakpoints starting at the input's xmin and ymax (top-left corner)
        if(is.null(origin)){

          xSeq <- seq(inext[1], inext[2], by = dim[1])
          if(xSeq[length(xSeq)] < inext[2]) xSeq <- c(xSeq, inext[2])

          ySeq <- seq(inext[4], inext[3], by = -dim[2])
          if(ySeq[length(ySeq)] > inext[3]) ySeq <- c(ySeq, inext[3])

        # If a 'origin' coordinate is set, compute a sequence of breakpoints from the origin value, subset those breakpoints
        # according to those that are within the input extent, and then start the sequence with the input's
        # xmin and ymax
        }else{

          xSnap <- round(origin[1] %% dim[1], 5)

          xSeq <- seq(
            .easy_round(inext[1], dim[1], "up",   snap = xSnap),
            .easy_round(inext[2], dim[1], "down", snap = xSnap),
            by = dim[1])

          if(xSeq[1] > inext[1]) xSeq <- c(inext[1], xSeq)
          if(xSeq[length(xSeq)] < inext[2]) xSeq <- c(xSeq, inext[2])

          ySnap <- round(origin[2] %% dim[2], 5)

          ySeq <- seq(
            .easy_round(inext[3], dim[2], "up",   snap = ySnap),
            .easy_round(inext[4], dim[2], "down", snap = ySnap),
            by = dim[2])

          if(ySeq[1] > inext[3]) ySeq <- c(inext[3], ySeq)
          if(ySeq[length(ySeq)] < inext[4]) ySeq <- c(ySeq, inext[4])

          # Reverse the order of the y Sequence (max to min)
          ySeq <- rev(ySeq)

        }

      # Assemble break points into intervals
      xInt <- data.frame(xmin = xSeq[1:(length(xSeq) - 1)], xmax = xSeq[2:length(xSeq)])
      yInt <- data.frame(ymin = ySeq[2:length(ySeq)],       ymax = ySeq[1:(length(ySeq) - 1)])

      # Create series of row and col numbers
      tilesRC <- expand.grid(col = 1:nrow(xInt), row = 1:nrow(yInt))[,2:1]
      tilesRC$tile_name <- paste0("R", tilesRC[,"row"], "C", tilesRC[,"col"])
      row.names(tilesRC) <- tilesRC$tile_name

      # Join all combinations of intervals
      tile_exts <- lapply(1:nrow(tilesRC), function(i){

        terra::ext(as.numeric(cbind(xInt[tilesRC[i, "col"], ], yInt[tilesRC[i, "row"], ])))

      })
    }


  ### DIMENSIONS BY NUMBER OF CELLS ----

    if(cells){

      # Convert buffer to map units
      bufferCells <- buffer
      buffer <- buffer * terra::res(x)[1]

      # Set extent of raster in terms of rows and columns
      rasdim <- c(colmin = 1, colmax = terra::ncol(x), rowmin = 1, rowmax = terra::nrow(x))

      # If there is a buffer and "spill" is set to FALSE, shrink the input extent
      if(buffer != 0 & spill == FALSE){
        rasdim[c("colmin", "rowmin")] <- rasdim[c("colmin", "rowmin")] + bufferCells
        rasdim[c("colmax", "rowmax")] <- rasdim[c("colmax", "rowmax")] - bufferCells
      }

      # Compute sequence of break points
      colSeq <- seq(rasdim["colmin"], rasdim["colmax"], by = dim[1])
      if((rasdim["colmax"] - rasdim["colmin"]) %% dim[1] != 0){colSeq <- c(colSeq, rasdim["colmax"])}
      rowSeq <- seq(rasdim["rowmin"], rasdim["rowmax"], by = dim[2])
      if((rasdim["rowmax"] - rasdim["rowmin"]) %% dim[2] != 0){rowSeq <- c(rowSeq, rasdim["rowmax"])}

      # Assemble break points into intervals
      colInt <-  data.frame(colmin = colSeq[1:(length(colSeq) - 1)],
                            colmax = colSeq[2:length(colSeq)] - c(rep(1, length(colSeq) - 2), 0))
      rowInt <-  data.frame(rowmin = rowSeq[1:(length(rowSeq) - 1)],
                            rowmax = rowSeq[2:length(rowSeq)] - c(rep(1, length(rowSeq) - 2), 0))

      # Create series of tile row and tile col numbers
      tilesRC <- expand.grid(col = 1:nrow(colInt), row = 1:nrow(rowInt))[,2:1]
      tilesRC$tile_name <- paste0("R", tilesRC[,"row"], "C", tilesRC[,"col"])
      row.names(tilesRC) <- tilesRC$tile_name

      # Join all combinations of intervals
      tile_exts <- lapply(1:nrow(tilesRC), function(i){

        col <- tilesRC[i, "col"]
        row <- tilesRC[i, "row"]

        terra::ext(x[rowInt[row, "rowmin"]:rowInt[row, "rowmax"], colInt[col,"colmin"]:colInt[col,"colmax"], drop=FALSE])

      })

    }

  ### Combine ----

    # Apply buffer
    buff_exts <- lapply(tile_exts, function(tile_ext) tile_ext + buffer)

    # Combine into 'sf' object
    out_sf <- dplyr::bind_cols(

      .exts_to_polys(tile_exts, "tiles", crs),
      .exts_to_polys(buff_exts, "buffs", crs),
      tilesRC
    )

  ### REMOVE EMPTY TILES ----


    if(remove_empty){

      # Get vector if empty tiles
      empties <- if(inclass == 'sf'){

        # If the input is a polygon, empty tiles are those that do not intersect with its boundaries
        lengths(sf::st_intersects(out_sf, x)) == 0


      }else if(inclass == 'SpatRaster'){

        # If the input is a raster, empty tiles are those that contain only NA values
        sapply(1:nrow(out_sf), function(i){

          cropped_tile <- terra::crop(x,  out_sf[i, "tiles"])

          all(!is.finite(terra::values(cropped_tile)))
        })

      }else stop("Cannot remove empty tiles using format: '", inputClass, "'")

      # If no tiles contain any values, return empty object
      if(all(empties)) stop("All tiles were empty")

      # Remove empty tiles
      out_sf <- out_sf[!empties, ]

    }

  ### CREATE NON-OVERLAPPING POLYGONS ----

    out_sf <- .nbuffs(out_sf)


  ### RETURN OUTPUT ----

    new("tileScheme", sf = out_sf, buffer = buffer)
}



.nbuffs <- function(out_sf){

  neib_win <- expand.grid(c(-1,0,1), c(-1,0,1))[-5,]
  row.names(neib_win) <- c("topleft", "top", "topright", "left", "right", "bottomleft", "bottom", "bottomright")
  colnames(neib_win)  <- c("col", "row")

  neib_crn <- data.frame(
    corner = c("topleft", "topright", "bottomright", "bottomleft"),
    hor    = c("left", "right", "right", "left"),
    vert   = c("top", "top", "bottom", "bottom"),
    stringsAsFactors = FALSE)


  polys <- lapply(1:nrow(out_sf), function(j){

    tile_bbox <- sf::st_bbox(out_sf[["tiles"]][[j]])
    buff_bbox <- sf::st_bbox(out_sf[["buffs"]][[j]])

    # Get row/col of potential neighbors
    neib_rc <- cbind(
      neib_win["col"] + out_sf[["col"]][j],
      neib_win["row"] + out_sf[["row"]][j]
    )

    # Determine which neighbors exist
    neib_rc$exists <- utils::tail(duplicated(rbind(sf::st_drop_geometry(out_sf[,c("col", "row")]), neib_rc)),8)
    neib_crn_ex <- cbind(neib_crn, data.frame(
      cornerExists = neib_rc[neib_crn$corner, "exists"],
      horExists    = neib_rc[neib_crn$hor,    "exists"],
      vertExists   = neib_rc[neib_crn$vert,   "exists"])
      )

    # Get positions of tile sides
    dim_tile <- data.frame(
      buff   = buff_bbox[],
      unbuff = tile_bbox[],
      diff   = buff_bbox[] - tile_bbox[],
      row.names = c("left", "bottom", "right", "top"))

    crn_pts <- do.call(rbind, lapply(1:4, function(i){

      # Get corner
      crn <- neib_crn_ex[i,]

      dims.crn <- dim_tile[c(crn[,"hor"], crn[,"vert"]),]

      if((crn[,"horExists"] & crn[,"vertExists"]) | !crn[,"cornerExists"]) {

        # Straight corner
        return(dims.crn[cbind(1:2, as.numeric(c(crn[,"horExists"], crn[,"vertExists"])) + 1)])

      }else{

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
      }
    }))

    sf::st_polygon(list(
      rbind(crn_pts, crn_pts[1,])
    ))
  })

  # Combine with input sf
  dplyr::bind_cols(out_sf, nbuffs = sf::st_as_sfc(polys, crs = sf::st_crs(out_sf)))

}


.ext_round <- function (inext, interval, direction = "up", snap = 0) {

  if (sign(interval) != 1) stop("'interval' must be set to a positive inext")

  snap_base <- snap %% interval
  snap_base <- round(snap_base, 5)


  if(direction == "in"){
    min_dir <- "up"
    max_dir <- "down"
  }else if(direction == "out"){
    min_dir <- "down"
    max_dir <- "up"
  }

  terra::ext(
    .easy_round(inext[1], interval, min_dir, snap_base),
    .easy_round(inext[2], interval, max_dir, snap_base),
    .easy_round(inext[3], interval, min_dir, snap_base),
    .easy_round(inext[4], interval, max_dir, snap_base)
  )

}


.easy_round <- function (value, interval, direction, snap.base){

  if (snap.base == 0) {
    if (direction == "closest")
      return(interval * round2(value/interval, 0))
    if (direction == "up")
      return(interval * ceiling(value/interval))
    if (direction == "down")
      return(interval * floor(value/interval))
  }
  else {
    value.ceiling <- interval * ceiling(value/interval)
    int.snap <- round(c(value.ceiling - interval * 2, value.ceiling -
                          interval, value.ceiling) + snap.base, 5)
    if (0 %in% (value - int.snap)) {
      return(value)
    }
    else {
      int.snap <- int.snap[-which.max(abs(value - int.snap))]
      if (direction == "closest") {
        return(int.snap[which.min(abs(value - int.snap))])
      }
      if (direction == "up") {
        return(int.snap[2])
      }
      if (direction == "down") {
        return(int.snap[1])
      }
    }
  }
}


.exts_to_polys <- function(exts, name, crs){

  polys <- dplyr::bind_rows(lapply(exts,  function(x){
    sf::st_sf(sf::st_as_sfc(sf::st_bbox(x)))
  }))

  sf::st_crs(polys) <- crs
  sf::st_geometry(polys) <- name
  return(polys)
}
