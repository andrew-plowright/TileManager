library(TileManager)

context("Subset tests")

ts <- tileScheme(CHMdemo, tiledim = c(50,60), cells = TRUE, buffer = 5, removeEmpty = TRUE)


test_that("Subset one row", {

  sub <- ts[2,]

  expect_equal(nrow(sub@data), 5)
  expect_equal(length(sub@tiles), 5)

  expect_equal(length(unique(sub@data$row)), 1)
  expect_equal(length(unique(sub@data$col)), 5)

})

test_that("Subset multiple rows", {

  sub <- ts[c(1,4),]

  expect_equal(nrow(sub@data),    9)
  expect_equal(length(sub@tiles), 9)

  expect_equal(length(unique(sub@data$row)), 2)
  expect_equal(length(unique(sub@data$col)), 8)

})

test_that("Subset by row/col", {

  sub <- ts[4,7]

  expect_equal(nrow(sub@data),    1)
  expect_equal(length(sub@tiles), 1)

  expect_equal(length(unique(sub@data$row)), 1)
  expect_equal(length(unique(sub@data$col)), 1)

})

test_that("Subset multiple rows/cols", {

  sub <- ts[2:3, c(1,3,4)]

  expect_equal(nrow(sub@data),    5)
  expect_equal(length(sub@tiles), 5)

  expect_equal(length(unique(sub@data$row)), 2)
  expect_equal(length(unique(sub@data$col)), 3)

})


test_that("Subset by tile number and tile name", {

  sub <- ts[c(1,10,15)]

  expect_equal(nrow(sub@data),    3)
  expect_equal(length(sub@tiles), 3)

  expect_equal(length(unique(sub@data$row)), 3)
  expect_equal(length(unique(sub@data$col)), 2)

  sub2 <- ts[c("R1C1", "R3C4", "R4C1")]

  expect_equal(sub@data, sub2@data)
  expect_equal(sub@tiles, sub2@tiles)

})


test_that("Get and set data",{

  expect_equal(length(ts$tileName), 63)
  expect_equal(length(ts$row),      63)
  expect_equal(length(ts$col),      63)

  expect_equal(head(ts$tileName, 1), "R1C1")
  expect_equal(tail(ts$tileName, 1), "R8C12")

  expect_true(all(grepl("^R2", ts[2,]$tileName)))
  expect_true(all(grepl("^R4", ts[4,]$tileName)))

  expect_true(all(grepl("C3$", ts[,3]$tileName)))
  expect_true(all(grepl("C5$", ts[,5]$tileName)))

  expect_equal(length(ts[9]$tileName), 1)

  ts$new_attribute <- letters[1:length(ts)]

  expect_equal(ts$new_attribute[1],  "a")
  expect_equal(ts$new_attribute[26], "z")
  expect_equal(ts$new_attribute[27], NA_character_)

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

  expect_error(ts$tileName <- "can't rename", "Cannot modify")

})

