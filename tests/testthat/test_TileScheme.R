library(TileManager)

context("Tests for TileScheme")

### LOAD TEST DATA

data(CHMdemo)
emptyRas <- raster::setValues(CHMdemo, NA)
extDemo <- raster::extent(CHMdemo)

### PERFORM TESTS

test_that("'tileScheme' performs as expected using buffers", {

  tile.bydist <- tileScheme(CHMdemo, tiledim = c(30,40), buffer = 4, removeEmpty = TRUE)

  # Expected size of a the first tile
  expect_identical(tile.bydist@tiles[[1]]@area,  1200)
  expect_identical(tile.bydist@buffs[[1]]@area,  1824)
  expect_identical(tile.bydist@nbuffs[[1]]@area, 1496)

  # Expectecd size of non-overlapping buffer tiles
  expect_equal(tile.bydist@nbuffs[[3]]@area,  1488, tolerance = 0.000001)
  expect_equal(tile.bydist@nbuffs[[7]]@area,  1480, tolerance = 0.000001)
  expect_equal(tile.bydist@nbuffs[[12]]@area, 690.375, tolerance = 0.000001)

  # Expected size of the entire tile set
  expect_equal(rgeos::gArea(tile.bydist[["tiles"]]),  12401.38, tolerance = 0.000001)
  expect_equal(rgeos::gArea(tile.bydist[["buffs"]]),  19355.38, tolerance = 0.000001)
  expect_equal(rgeos::gArea(tile.bydist[["nbuffs"]]), 14419.38, tolerance = 0.000001)

  # Expected number of tiles
  expect_equal(length(tile.bydist@tiles),  12)
  expect_equal(length(tile.bydist@buffs),  12)
  expect_equal(length(tile.bydist@nbuffs), 12)

  # Expected tile order
  expect_equal(tile.bydist[1]@data$tileName,   "R1C1")
  expect_equal(tile.bydist[2]@data$tileName,   "R1C2")
  expect_equal(tile.bydist[1,1]@data$tileName, "R1C1")
  expect_equal(tile.bydist[1,2]@data$tileName, "R1C2")
  expect_equal(tile.bydist[2,1]@data$tileName, "R2C1")

})

test_that("'tileScheme' performs as expected using an Extent object", {

  tile.fromext <- tileScheme(extDemo, tiledim = c(30,30))

  expect_equal(length(tile.fromext@tiles), 20)
})


test_that("'tileScheme' performs as expected using 'cells' with no buffers", {

  tile.bycell <- tileScheme(CHMdemo, tiledim = c(200,300), cells = TRUE, removeEmpty = TRUE)

  # Expected size of a the first tile
  expect_identical(tile.bycell@tiles[[1]]@area,  3750)
  expect_identical(tile.bycell@buffs[[1]]@area,  3750)
  expect_identical(tile.bycell@nbuffs[[1]]@area, 3750)

  # Expected size of the entire tile set
  expect_equal(rgeos::gArea(tile.bycell[["tiles"]]),  13591.88, tolerance = 0.000001)
  expect_equal(rgeos::gArea(tile.bycell[["buffs"]]),  13591.88, tolerance = 0.000001)
  expect_equal(rgeos::gArea(tile.bycell[["nbuffs"]]), 13591.88, tolerance = 0.000001)

  # Expected number of tiles
  expect_equal(length(tile.bycell@tiles),  5)
  expect_equal(length(tile.bycell@buffs),  5)
  expect_equal(length(tile.bycell@nbuffs), 5)

  # Expected tile order
  expect_equal(tile.bycell[1]@data$tileName,   "R1C1")
  expect_equal(tile.bycell[2]@data$tileName,   "R1C2")
  expect_equal(tile.bycell[1,1]@data$tileName, "R1C1")
  expect_equal(tile.bycell[1,2]@data$tileName, "R1C2")
  expect_equal(tile.bycell[2,1]@data$tileName, "R2C1")

})

test_that("'tileScheme' performs as expected using 'cells' with buffers", {

  tile.bycell2 <- tileScheme(CHMdemo, tiledim = c(100,100), cells = TRUE, buffer = 5, removeEmpty = TRUE)

  # Expected size of a the first tile
  expect_identical(tile.bycell2@tiles[[1]]@area, 625)
  expect_identical(tile.bycell2@buffs[[1]]@area, 756.25)

  # Expectecd size of non-overlapping buffer tiles
  expect_equal(tile.bycell2@nbuffs[[1]]@area,  721.0938, tolerance = 0.000001)
  expect_equal(tile.bycell2@nbuffs[[3]]@area,  656.25,   tolerance = 0.000001)
  expect_equal(tile.bycell2@nbuffs[[17]]@area, 433.125,  tolerance = 0.000001)
  expect_equal(tile.bycell2@nbuffs[[2]]@area,  720.3125, tolerance = 0.000001)

  # Expected size of the entire tile set
  expect_equal(rgeos::gArea(tile.bycell2[["tiles"]]),  11885,    tolerance = 0.000001)
  expect_equal(rgeos::gArea(tile.bycell2[["buffs"]]),  14576.25, tolerance = 0.000001)
  expect_equal(rgeos::gArea(tile.bycell2[["nbuffs"]]), 12591.88, tolerance = 0.000001)

  # Expected number of tiles
  expect_equal(length(tile.bycell2@tiles),  22)
  expect_equal(length(tile.bycell2@buffs),  22)
  expect_equal(length(tile.bycell2@nbuffs), 22)

})



