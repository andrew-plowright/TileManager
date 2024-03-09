if(basename(getwd()) != "testthat") setwd(file.path(getwd(), "tests", "testthat"))

context("Tests for 'tileScheme'")

chm <- terra::rast(system.file("ex/chm.tif", package="TileManager"))
xt <- terra::ext(chm)
downtown <- sf::st_read(system.file("ex/downtown.shp", package="TileManager"), quiet =T)

test_that("'SpatRaster' input", {

  ts <- tileScheme(chm, dim = c(30,40), buffer = 4, remove_empty = TRUE)

  tile_areas <- as.numeric(sf::st_area(ts[["tiles"]]))
  buff_areas <- as.numeric(sf::st_area(ts[["buffs"]]))
  nbuff_areas <- as.numeric(sf::st_area(ts[["nbuffs"]]))

  # Expected size of a the first tile
  expect_identical(tile_areas[1],  1200)
  expect_identical(buff_areas[1],  1824)
  expect_identical(nbuff_areas[1], 1496)

  # Expectecd size of non-overlapping buffer tiles
  expect_equal(nbuff_areas[3],  1488, tolerance = 0.000001)
  expect_equal(nbuff_areas[7],  1480, tolerance = 0.000001)
  expect_equal(nbuff_areas[12], 690.375, tolerance = 0.000001)

  # Expected size of the entire tile set
  expect_equal(sum(tile_areas),  12401.38, tolerance = 0.000001)
  expect_equal(sum(buff_areas),  19355.38, tolerance = 0.000001)
  expect_equal(sum(nbuff_areas), 14419.38, tolerance = 0.000001)

  # Expected number of tiles
  expect_equal(length(ts),  12)

  # Expected tile order
  expect_equal(ts[1]@sf$tile_name,   "R1C1")
  expect_equal(ts[2]@sf$tile_name,   "R1C2")
  expect_equal(ts[1,1]@sf$tile_name, "R1C1")
  expect_equal(ts[1,2]@sf$tile_name, "R1C2")
  expect_equal(ts[2,1]@sf$tile_name, "R2C1")

  # CRS
  expect_equal(sf:::crs_parameters(sf::st_crs(ts@sf))$Name,  "WGS 84 / UTM zone 11N")
})

test_that("'SpatExtent' input", {

  ts <- tileScheme(xt, dim = c(30,30))

  expect_equal(length(ts), 20)
})


test_that("'sf' input", {

  ts <- tileScheme(downtown, dim = c(500,500), buffer = 40, spill = FALSE, remove_empty = TRUE)

  tile_areas <- as.numeric(sf::st_area(ts[["tiles"]]))
  buff_areas <- as.numeric(sf::st_area(ts[["buffs"]]))
  nbuff_areas <- as.numeric(sf::st_area(ts[["nbuffs"]]))

  # Expected size of the entire tile set
  expect_equal(sum(tile_areas),  13242465, tolerance = 0.000001)
  expect_equal(sum(buff_areas),  17959660, tolerance = 0.000001)
  expect_equal(sum(nbuff_areas), 13935358, tolerance = 0.000001)

  expect_equal(length(ts), 56)
})


test_that("'cells'=TRUE with no buffers", {

  ts <- tileScheme(chm, dim = c(200,300), cells = TRUE, remove_empty = TRUE)

  tile_areas <- as.numeric(sf::st_area(ts[["tiles"]]))
  buff_areas <- as.numeric(sf::st_area(ts[["buffs"]]))
  nbuff_areas <- as.numeric(sf::st_area(ts[["nbuffs"]]))

  # Expected size of a the first tile
  expect_identical(tile_areas[1],  3750)
  expect_identical(buff_areas[1],  3750)
  expect_identical(nbuff_areas[1], 3750)

  # Expected size of the entire tile set
  expect_equal(sum(tile_areas),  13591.88, tolerance = 0.000001)
  expect_equal(sum(buff_areas),  13591.88, tolerance = 0.000001)
  expect_equal(sum(nbuff_areas), 13591.88, tolerance = 0.000001)

  # Expected number of tiles
  expect_equal(length(ts),  5)

  # Expected tile order
  expect_equal(ts[1]@sf$tile_name,   "R1C1")
  expect_equal(ts[2]@sf$tile_name,   "R1C2")
  expect_equal(ts[1,1]@sf$tile_name, "R1C1")
  expect_equal(ts[1,2]@sf$tile_name, "R1C2")
  expect_equal(ts[2,1]@sf$tile_name, "R2C1")
})

test_that("'cells'=TRUE with buffers", {

  ts <- tileScheme(chm, dim = c(100,100), cells = TRUE, buffer = 5, remove_empty = TRUE)

  tile_areas <- as.numeric(sf::st_area(ts[["tiles"]]))
  buff_areas <- as.numeric(sf::st_area(ts[["buffs"]]))
  nbuff_areas <- as.numeric(sf::st_area(ts[["nbuffs"]]))

  # Expected size of a the first tile
  expect_equal(tile_areas[1],   625)
  expect_equal(buff_areas[1],   756.25)
  expect_equal(nbuff_areas[1],  721.0938, tolerance = 0.000001)

  # Expected size of various unbuffered tyiles
  expect_equal(nbuff_areas[2],  720.3125, tolerance = 0.000001)
  expect_equal(nbuff_areas[3],  656.2500, tolerance = 0.000001)
  expect_equal(nbuff_areas[17], 433.1250, tolerance = 0.000001)

  # Expected size of the entire tile set
  expect_equal(sum(tile_areas),  11885.00, tolerance = 0.000001)
  expect_equal(sum(buff_areas),  14576.25, tolerance = 0.000001)
  expect_equal(sum(nbuff_areas), 12591.88, tolerance = 0.000001)

  # Expected number of tiles
  expect_equal(length(ts),  22)
})



