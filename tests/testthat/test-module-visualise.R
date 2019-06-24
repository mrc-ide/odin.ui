context("module: visualise")


test_that("run model: empty", {
  expect_null(vis_run(NULL))
})

test_that("run model, with missing zero time", {
  code <- c("deriv(x) <- 1",
            "initial(x) <- a",
            "a <- user(3, min = 0)",
            "b <- user(max = 1)",
            "deriv(y) <- 2",
            "initial(y) <- b")
  model <- common_odin_compile_from_code(code)
  d <- data.frame(t = 1:10, a = runif(10), b = runif(10), c = runif(10))
  data <- odin_data_source(d, "file.csv", "t")
  link <- link_result(list(a = "x", c = "y"))
  configuration <- common_model_data_configuration(model, data, link)

  user <- list(a = 2, b = 1)
  res <- vis_run(configuration, user = user)
  expect_equal(res$configuration, configuration)
  expect_setequal(names(res$simulation),
                  c("data", "combined", "smooth", "user"))
  expect_is(res$simulation$data, "matrix")

  fx <- function(t) 2 + t
  fy <- function(t) 1 + 2 * t
  t <- seq(1, 10, length.out = 501)

  expect_equal(res$simulation$data,
               cbind(t = d$t, x = fx(d$t), y = fy(d$t)))
  expect_equal(res$simulation$smooth,
               cbind(t = t, x = fx(t), y = fy(t)))
  expect_equal(res$simulation$combined,
               cbind(res$simulation$data, d))
  expect_equal(res$simulation$user, list_to_df(user))
})


test_that("plot", {
  code <- c("deriv(x) <- 1",
            "initial(x) <- a",
            "a <- user(3, min = 0)",
            "b <- user(max = 1)",
            "deriv(y) <- 2",
            "initial(y) <- b")
  model <- common_odin_compile_from_code(code)
  d <- data.frame(t = 1:10, a = runif(10), b = runif(10), c = runif(10))
  data <- odin_data_source(d, "file.csv", "t")
  link <- link_result(list(a = "x", c = "y"))
  configuration <- common_model_data_configuration(model, data, link)
  user <- list(a = 2, b = 1)
  res <- vis_run(configuration, user = user)
  y2 <- list(x = FALSE, y = TRUE)
  series <- vis_plot_series(res, NULL, y2)

  expect_equal(length(series), 5)
  ## Lines for modelled data:
  expect_equal(
    series[[1]],
    plot_plotly_series(res$simulation$smooth[, 1], res$simulation$smooth[, 2],
                       "x", configuration$cols$model[[1]], legendgroup = "x"))
  expect_equal(
    series[[2]],
    plot_plotly_series(res$simulation$smooth[, 1], res$simulation$smooth[, 3],
                       "y", configuration$cols$model[[2]], legendgroup = "y",
                       y2 = TRUE))

  ## Points for real data:
  expect_equal(
    series[[3]],
    plot_plotly_series(d$t, d$a, "a", configuration$cols$data[[1]],
                       points = TRUE))
  expect_equal(
    series[[4]],
    plot_plotly_series(d$t, d$b, "b", configuration$cols$data[[2]],
                       points = TRUE))
  expect_equal(
    series[[5]],
    plot_plotly_series(d$t, d$c, "c", configuration$cols$data[[3]],
                       points = TRUE, y2 = TRUE))

  ## This can't be helpfully tested at this point
  expect_is(vis_plot(res, NULL, y2, FALSE), "plotly")
})


test_that("vis status", {
  expect_null(vis_status(NULL))
  expect_equal(
    vis_status(list(error = "an error")),
    simple_panel("danger", "Error running model", "an error"))
})
