context("module: fit")

test_that("fit configuration", {
  d <- example_data_fit()
  model <- d$model
  data <- d$data
  link <- d$link

  configuration <- fit_configuration(model, data, link)

  cmp <- common_model_data_configuration(model, data, link)
  expect_equal(
    configuration$pars,
    cbind(cmp$pars, id_vary = c("par_vary_b", "par_vary_a"), vary = FALSE,
          stringsAsFactors = FALSE))
  expect_equal(
    configuration$vars,
    cbind(cmp$vars, include = c(TRUE, TRUE, FALSE)))

  configuration[c("pars", "vars")] <- cmp[c("pars", "vars")]
  expect_equal(configuration, cmp)
})


test_that("fit control paramters: unconfigured", {
  expect_null(fit_control_target(NULL))
})


test_that("fit control paramters: unconfigured", {
  expect_null(fit_control_parameters(NULL))
})
