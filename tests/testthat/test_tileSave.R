library(TileManager)

context("Tests for 'tileSave'")

### LOAD TEST DATA

  data(CHMdemo)

  # Create original tileScheme
  ts1 <- tileScheme(CHMdemo, tiledim = c(20,20), buffer = 3, removeEmpty = TRUE, origin = c(0.5, 0.5))

  # Set temporary directory
  tempFile <- tempfile(fileext = ".shp")


### PERFORM TESTS

  test_that("'tileSave' and 'tileLoad' performs as expected", {

    tileSave(ts1, tempFile)

    ts2 <- tileLoad(tempFile)

    expect_true(identical(ts1, ts2))
  })


