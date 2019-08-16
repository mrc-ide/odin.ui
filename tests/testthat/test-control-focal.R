context("control focal")


test_that("batch_focal - not read cases", {
  name <- scale <- type <- pct <- from <- to <- n <- user <- NULL

  expect_error(
    control_focal_result(name, scale, type, pct, from, to, n, user),
    "Please select a valid parameter")

  name <- "x"
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, n, user),
    "Enter a valid value for the parameter x")

  user <- list(x = 1)
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, n, user),
    "Number of runs must be given")
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, 1, user),
    "At least 2 runs are needed")
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, -10, user),
    "At least 2 runs are needed")
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, 100, user),
    "At most 20 runs are possible")

  n <- 10
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, n, user),
    "Please select a valid value for the scale type")

  scale <- "Arithmetic"
  expect_error(
    control_focal_result(name, scale, type, pct, from, to, n, user),
    "Please select a valid value for the variation type")

  expect_error(
    control_focal_result(name, scale, "Percentage", pct, from, to, n, user),
    "'Variation %' is missing")

  expect_error(
    control_focal_result(name, scale, "Range", pct, from, to, n, user),
    "'From' is missing")
  expect_error(
    control_focal_result(name, scale, "Range", pct, 1, to, n, user),
    "'To' is missing")
})


test_that("batch_focal", {
  name <- "x"
  pct <- 10
  n <- 10
  user <- list(x = 1, y = 2)
  scale <- "Arithmetic"
  from <- 1
  to <- 10

  expect_equal(
    control_focal_result(name, scale, "Percentage", pct, from, to, n, user),
    list(base = user, name = "x", value = 1, n = 10, pct = 10,
         from = 0.9, to = 1.1, logarithmic = FALSE,
         values = seq(0.9, 1.1, length.out= 10)))
  expect_equal(
    control_focal_result(name, scale, "Range", pct, from, to, n, user),
    list(base = user, name = "x", value = 1, n = 10, pct = 450,
         from = 1, to = 10, logarithmic = FALSE,
         values = seq(1, 10, length.out= 10)))
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
