# library(TileManager)
#
# context("Tests for 'tileDetector'")
#
# ### LOAD TEST DATA
#
#   data(CHMdemo)
#
#   # Create original tileScheme
#   ts1 <- tileScheme(CHMdemo, tiledim = c(20,20), buffer = 3, removeEmpty = TRUE, origin = c(0.5, 0.5))
#
#   # Set temporary directory
#   tempDir <- file.path(tempdir(), "tileDetector_test")
#   dir.create(tempDir)
#
#   # Save tileScheme to a series of rasters
#   for(i in 1:length(ts1)){
#     raster::crop(CHMdemo, ts1[i][["buffs"]], filename = tempfile(tmpdir = tempDir, fileext = ".tif"))
#   }
#
#   # Get list of rasters
#   rasterList <-  list.files(tempDir, pattern = "\\.tif$", full.names = TRUE)
#
# ### PERFORM TESTS
#
#
#   test_that("'tileDetector' performs as expected", {
#
#     ts2 <- tileDetector(rasterList)
#
#     expect_true(identical(ts1, ts2))
#   })
#
#   unlink(tempDir, recursive = TRUE)
#
