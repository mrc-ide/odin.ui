context("module: batch")

test_that("batch_focal", {
  expect_null(control_focal_result(NULL, NULL, NULL, NULL))

  name <- "x"
  pct <- 10
  n <- 10
  user <- list(x = 1, y = 2)

  ## Any missing value prevents running
  expect_null(control_focal_result(name, pct, n, NULL))
  expect_null(control_focal_result(name, pct, NULL, user))
  expect_null(control_focal_result(name, NULL, n, user))
  expect_null(control_focal_result(NULL, pct, n, user))

  ## All in and we run
  expect_equal(
    control_focal_result(name, pct, n, user),
    list(base = user, name = "x", value = 1, n = 10, from = 0.9, to = 1.1))
})


test_that("pct_to_range", {
  expect_equal(
    control_focal_pct_to_range(10, 15),
    list(from = 8.5, to = 11.5))
})


test_that("range_to_pct", {
  expect_equal(
    control_focal_range_to_pct(10, 8.5, 11.5), 15)
})


test_that("pct_to_range - empty cases", {
  empty <- list(from = NA, to = NA)
  expect_equal(control_focal_pct_to_range(NULL, NULL), empty)
  expect_equal(control_focal_pct_to_range(NULL, 15), empty)
  expect_equal(control_focal_pct_to_range(10, NULL), empty)
})
