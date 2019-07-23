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
