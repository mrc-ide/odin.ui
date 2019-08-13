context("control focal")

test_that("batch_focal", {
  expect_null(control_focal_result(NULL, NULL, NULL, NULL))

  name <- "x"
  pct <- 10
  n <- 10
  user <- list(x = 1, y = 2)
  scale <- "Arithmetic"
  type <- "Percentage"
  from <- to <- NA

  ## Any missing value prevents running
  expect_null(
    control_focal_result(name, scale, type, pct, from, to, n, NULL))
  expect_null(
    control_focal_result(name, scale, type, pct, from, to, NULL, user))
  expect_null(
    control_focal_result(name, scale, type, NULL, from, to, n, user))
  expect_null(
    control_focal_result(NULL, scale, type, pct, from, to, n, user))

  ## All in and we run
  expect_equal(
    control_focal_result(name, scale, type, pct, from, to, n, user),
    list(base = user, name = "x", value = 1, n = 10, pct = 10,
         from = 0.9, to = 1.1, logarithmic = FALSE))
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
