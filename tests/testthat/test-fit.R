context("fit")


## still heaps of things to get right and test here!
test_that("objective", {
  configuration <- example_data_fit()$configuration

  t <- configuration$data$data$t
  y <- configuration$data$data$a
  model <- configuration$model$model
  user <- list(a = 0, b = 0)
  vary <- c("a", "b")
  objective <- odin_fit_objective(t, y, model, "x", user, vary, compare_sse)
  expect_equal(
    objective(c(1, 2)),
    compare_sse(
      model(user = list(a = 1, b = 2))$run(c(0, t))[-1, "x"],
      configuration$data$data$a))

  vary <- "a"
  objective <- odin_fit_objective(t, y, model, "x", user, vary, compare_sse)
  expect_equal(
    objective(c(1)),
    compare_sse(
      model(user = list(a = 1, b = 0))$run(c(0, t))[-1, "x"],
      configuration$data$data$a))
})


test_that("fit", {
  configuration <- example_data_fit()$configuration

  t <- configuration$data$data$t
  y <- configuration$data$data$a
  model <- configuration$model$model
  user <- list(a = 0, b = 0)
  vary <- c("a", "b")
  i <- match(vary, configuration$pars$name)
  lower <- configuration$pars$min[i]
  upper <- configuration$pars$max[i]

  res <- odin_fit_model(t, y, model, "x", user, vary, lower, upper)
  expect_equal(res$par, c(a = 1.664666, b = 1.109152), tolerance = 1e-4)
})


test_that("fit", {
  configuration <- example_data_fit()$configuration

  t <- configuration$data$data$t
  y <- configuration$data$data$a
  model <- configuration$model$model
  user <- list(a = 0, b = 0)
  vary <- c("a", "b")
  i <- match(vary, configuration$pars$name)
  lower <- configuration$pars$min[i]
  upper <- configuration$pars$max[i]

  expect_error(
    odin_fit_model(t, y, model, "x", user, character(0), lower, upper),
    "Select at least one parameter to vary")
  expect_error(
    odin_fit_model(t, y, model, "x", c(a = 1, b = NA), vary, lower, upper),
    "Starting parameter value needed for b")
  expect_error(
    odin_fit_model(t, y, model, "x", c(a = NA, b = NA), vary, lower, upper),
    "Starting parameter value needed for a, b")

  res <- odin_fit_model(t, y, model, "x", user, vary, lower, upper)
  expect_equal(res$par, c(a = 1.664666, b = 1.109152), tolerance = 1e-4)
})


test_that("fit: vary algo", {
  configuration <- example_data_fit()$configuration

  t <- configuration$data$data$t
  y <- configuration$data$data$a
  model <- configuration$model$model
  user <- list(a = 0, b = 0)
  vary <- c("a", "b")
  i <- match(vary, configuration$pars$name)
  lower <- configuration$pars$min[i]
  upper <- configuration$pars$max[i]

  res_optim <- odin_fit_model(
    t, y, model, "x", user, vary, lower, upper, method = "optim")
  res_subplex <- odin_fit_model(
    t, y, model, "x", user, vary, lower, upper, method = "subplex")
  res_hjkb <- odin_fit_model(
    t, y, model, "x", user, vary, lower, upper, method = "hjkb")

  expect_equal(res_optim$par, c(a = 1.664666, b = 1.109152), tolerance = 1e-4)
  expect_equal(res_subplex$par, c(a = 1.664666, b = 1.109152), tolerance = 1e-4)
  expect_equal(res_hjkb$par, c(a = 1.664666, b = 1.109152), tolerance = 1e-4)

  expect_equal(names(res_optim), names(res_subplex))
  expect_equal(names(res_optim), names(res_hjkb))
})
