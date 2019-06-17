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


test_that("run model", {
  expect_null(vis_run_model(NULL))

  model <- list(result = list(model = odin::odin({
    deriv(x) <- a
    initial(x) <- 1
    a <- user(1)
  }, verbose = FALSE)))
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
  }, verbose = FALSE)))
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
  ## Lots of data to set up here:
  model <- list(result = list(model = odin::odin({
    deriv(x) <- a
    initial(x) <- 1
    deriv(y) <- 2 * a
    initial(y) <- 2
    a <- user(1)
  }, verbose = FALSE)))
  t_data <- 1:10
  data <- list(name_time = "t",
               name_vars = c("x", "y"),
               data = data_frame(t = t_data, x = sin(t_data), y = cos(t_data)))
  cols <- odin_colours(c("x", "y"), c("x", "y"), c("x" = "x"))
  configuration <- list(model = model, data = data, link = c("x" = "x"),
                        cols = odin_colours(c("x", "y"), c("x", "y"),
                                            c("x" = "x")))
  user <- list(a = 2)
  res <- vis_run_model(configuration, user)
  y2_model <- c(x = FALSE, y = FALSE)

  ## and the expected output:
  t_model <- seq(0, 10, length.out = 501)
  expected <- list(
    list(x = t_model, y = 1 + t_model * 2, name = "x", yaxis = "y1",
         line = list(color = cols$model[["x"]])),
    list(x = t_model, y = 2 + t_model * 4, name = "y", yaxis = "y1",
         line = list(color = cols$model[["y"]])),
    list(x = t_data, y = data$data$x, name = "x", yaxis = "y1",
         marker = list(color = cols$data[["x"]])),
    list(x = t_data, y = data$data$y, name = "y", yaxis = "y1",
         marker = list(color = cols$data[["y"]])))

  series <- vis_plot_series(res, y2_model)
  expect_equal(series, expected)

  ## y2 is shared across fields:
  series2 <- vis_plot_series(res, c(x = TRUE, y = FALSE))
  expected2 <- expected
  expected2[[1]]$yaxis <- "y2"
  expected2[[3]]$yaxis <- "y2"
  expect_equal(series2, expected2)

  series3 <- vis_plot_series(res, c(x = FALSE, y = TRUE))
  expected3 <- expected
  expected3[[2]]$yaxis <- "y2"
  expect_equal(series3, expected3)

  ## This can't be helpfully tested at this point
  expect_is(vis_plot(res, y2_model, FALSE), "plotly")
})
