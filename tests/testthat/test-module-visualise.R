context("module: visualise")

test_that("download filename", {
  expect_match(vis_download_filename(NULL, "combined"),
               "^odin-visualise-combined-[0-9]{8}-[0-9]{6}.csv$")
  expect_match(vis_download_filename("", "parameters"),
               "^odin-visualise-parameters-[0-9]{8}-[0-9]{6}.csv$")
  expect_equal(vis_download_filename("x", "parameters"), "x.csv")
  expect_equal(vis_download_filename("x.csv", "parameters"), "x.csv")
})


test_that("download data", {
  path <- tempfile()
  simulation <- list(smooth = data_frame(x = 1),
                     combined = data_frame(x = 2),
                     user = data_frame(x = 3))
  vis_download_data(path, simulation, "modelled")
  expect_equal(read_csv(path), simulation$smooth)

  vis_download_data(path, simulation, "combined")
  expect_equal(read_csv(path), simulation$combined)

  vis_download_data(path, simulation, "parameters")
  expect_equal(read_csv(path), simulation$user)
})


test_that("configuration", {
  model <- list(result = list(
                  success = TRUE,
                  info = list(pars = data_frame(
                                name = c("a", "b"),
                                default_value = I(as.list(1:2))),
                              vars = data_frame(
                                name = c("X", "Y")))))

  data <- list(configured = TRUE,
               name_vars = c("x", "y"))
  link <- NULL

  expect_null(vis_configuration(NULL, NULL, NULL))
  expect_null(vis_configuration(model, NULL, NULL))
  expect_null(vis_configuration(NULL, data, NULL))

  res <- vis_configuration(model, data, link)
  expect_equal(res$data, data)
  expect_equal(res$model, model)
  expect_equal(res$link, link)
  expect_equal(res$pars, cbind(model$result$info$pars,
                               value = 1:2,
                               id_value = paste0("par_value_", c("a", "b")),
                               stringsAsFactors = FALSE))
  expect_equal(res$vars, cbind(model$result$info$vars,
                               id_y2 = paste0("var_y2_", c("X", "Y")),
                               stringsAsFactors = FALSE))
  expect_equal(res$link, link)
  expect_equal(res$cols, odin_colours(c("X", "Y"), c("x", "y"), link))

  link <- list(X = "x")
  res <- vis_configuration(model, data, link)
  expect_equal(res$data, data)
  expect_equal(res$model, model)
  expect_equal(res$link, link)
  expect_equal(res$pars, cbind(model$result$info$pars,
                               value = 1:2,
                               id_value = paste0("par_value_", c("a", "b")),
                               stringsAsFactors = FALSE))
  expect_equal(res$vars, cbind(model$result$info$vars,
                               id_y2 = paste0("var_y2_", c("X", "Y")),
                               stringsAsFactors = FALSE))
  expect_equal(res$link, link)
  expect_equal(res$cols, odin_colours(c("X", "Y"), c("x", "y"), link))
})


test_that("run model", {
  expect_null(vis_run_model(NULL))

  model <- list(result = list(model = odin::odin({
    deriv(x) <- a
    initial(x) <- 1
    a <- user(1)
  })))
  t <- 0:10
  data <- list(name_time = "t", data = data_frame(t = t, x = sin(t)))
  configuration <- list(model = model, data = data)
  user <- list(a = 2)

  mod <- model$result$model(user = user)

  res <- vis_run_model(configuration, user)
  expect_identical(res$configuration, configuration)
  expect_equal(res$simulation$user, data_frame(name = "a", value = 2))
  expect_equal(res$simulation$data, mod$run(t))
  expect_equal(res$simulation$data[, "x"], seq(1, length.out = 11, by = 2))
  expect_equal(res$simulation$smooth, mod$run(seq(0, 10, length.out = 501)))
  expect_equal(res$simulation$combined, cbind(mod$run(t), data$data))
})


test_that("run model copes with missing zero", {
  model <- list(result = list(model = odin::odin({
    deriv(x) <- a
    initial(x) <- 1
    a <- user(1)
  })))
  t <- 1:10
  data <- list(name_time = "t", data = data_frame(t = t, x = sin(t)))
  configuration <- list(model = model, data = data)
  user <- list(a = 2)

  mod <- model$result$model(user = user)

  res <- vis_run_model(configuration, user)
  expect_identical(res$configuration, configuration)
  expect_equal(res$simulation$user, data_frame(name = "a", value = 2))
  tmp <- mod$run(c(0, t))[-1, , drop = FALSE]
  expect_equal(res$simulation$data, tmp)
  expect_equal(res$simulation$combined, cbind(tmp, data$data))
  expect_equal(res$simulation$smooth, mod$run(seq(0, 10, length.out = 501)))
})


test_that("plot", {
  model <- list(result = list(model = odin::odin({
    deriv(x) <- a
    initial(x) <- 1
    a <- user(1)
  })))
  t <- 1:10
  data <- list(name_time = "t", data = data_frame(t = t, x = sin(t)))
  configuration <- list(model = model, data = data, link = c("x" = "x"),
                        cols = odin_colours("x", "x", c("x" = "x")))
  user <- list(a = 2)

  res <- vis_run_model(configuration, user)
  p <- vis_plot(res, c(x = FALSE), FALSE)
  ## For now, not really any test here.  But soon we'll find a common
  ## way of doing this with a "plot over a set of series" that will
  ## limit how much is not easily testable.
  expect_is(p, "plotly")
})
