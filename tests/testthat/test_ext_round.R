test_that("round extents", {

  x <- terra::ext(10.1, 14.9, 20.1, 24.9)

  # Round out
  expect_equal(
    terra::ext(10, 15, 20, 25),
    .ext_round(x, interval = 1, direction = "out", snap = 0)
  )

  # Round in
  expect_equal(
    terra::ext(11, 14, 21, 24),
    .ext_round(x, interval = 1, direction = "in",  snap = 0)
  )

  # Snap
  expect_equal(
    terra::ext(10.25, 14.25, 20.25, 24.25),
    .ext_round(x, interval = 1, direction = "in",  snap = 10.25)
  )

  # Large interval
  expect_equal(
    terra::ext(12, 14, 22, 24),
    .ext_round(x, interval = 2, direction = "in",  snap = 0)
  )
  expect_equal(
    terra::ext(10, 20, 20, 30),
    .ext_round(x, interval = 10, direction = "out", snap = 0)
  )
})


