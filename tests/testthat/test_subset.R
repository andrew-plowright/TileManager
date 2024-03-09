
context("Subset tests")
chm <- terra::rast(system.file("ex/chm.tif", package="TileManager"))
ts <- tileScheme(chm, dim = c(50,60), cells = TRUE, buffer = 5, remove_empty = TRUE)


test_that("Subset one row", {

  sub <- ts[2,]

  expect_equal(length(sub),  5)
  expect_equal(nrow(sub@sf), 5)

  expect_equal(length(unique(sub@sf$row)), 1)
  expect_equal(length(unique(sub@sf$col)), 5)

})

test_that("Subset multiple rows", {

  sub <- ts[c(1,4),]

  expect_equal(length(sub),  9)
  expect_equal(nrow(sub@sf), 9)

  expect_equal(length(unique(sub@sf$row)), 2)
  expect_equal(length(unique(sub@sf$col)), 8)

})

test_that("Subset by row/col", {

  sub <- ts[4,7]

  expect_equal(length(sub),  1)
  expect_equal(nrow(sub@sf), 1)

  expect_equal(length(unique(sub@sf$row)), 1)
  expect_equal(length(unique(sub@sf$col)), 1)

})

test_that("Subset multiple rows/cols", {

  sub <- ts[2:3, c(1,3,4)]

  expect_equal(length(sub),  5)
  expect_equal(nrow(sub@sf), 5)

  expect_equal(length(unique(sub@sf$row)), 2)
  expect_equal(length(unique(sub@sf$col)), 3)

})


test_that("Subset by tile number and tile name", {

  sub <- ts[c(1,10,15)]

  expect_equal(length(sub),  3)
  expect_equal(nrow(sub@sf), 3)

  expect_equal(length(unique(sub@sf$row)), 3)
  expect_equal(length(unique(sub@sf$col)), 2)

  sub2 <- ts[c("R1C1", "R3C4", "R4C1")]

  expect_equal(sub@sf, sub2@sf)
})


test_that("Expected errors", {

  expect_error(ts[1000],        "Tile number index is out of bounds")
  expect_error(ts[c(1,2,NA)],   "Tile number index is out of bounds")

  expect_error(ts[1,1000],      "Column number is out of bounds")
  expect_error(ts[1,c(1,2,NA)], "Column number is out of bounds")

  expect_error(ts[1000,1],      "Row number is out of bounds")
  expect_error(ts[c(1,2,NA),1], "Row number is out of bounds")

  expect_error(ts[c("R1C1", "R?C?")], "Could not find input tile name")

  expect_error(ts[c(T,F,T), c(F,T,F)],           "object of type 'S4' is not subsettable")
  expect_error(ts[c("R1", "R2"), c("C1", "C2")], "Cannot use second index when subsetting using tile name")
})

