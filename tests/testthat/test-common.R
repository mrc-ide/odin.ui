context("common")

test_that("odin_colours_model", {
  expect_equal(odin_colours_model(c("a", "b")),
               c(a = "#2E5CB8", b = "#CC0044"))
})


test_that("odin_colours_data", {
  expect_equal(odin_colours_data(c("a", "b")),
               c(a = "#E41A1C", b = "#F781BF"))
})


test_that("odin_colours", {
  model <- c("x", "y")
  data <- toupper(model)
  col_model <- odin_colours_model(model)
  col_data <- odin_colours_data(data)

  expect_equal(
    odin_colours(model, data, NULL),
    list(model = col_model, data = col_data))

  expect_equal(
    odin_colours(model, data, c(X = "x")),
    list(model = c(x = col_data[["X"]], y = col_model[["y"]]),
         data = col_data))
  expect_equal(
    odin_colours(model, data, c(Y = "y")),
    list(model = c(x = col_model[["x"]], y = col_data[["Y"]]),
         data = col_data))
  expect_equal(
    odin_colours(model, data, c(c(X = "x", Y = "y"))),
    list(model = c(x = col_data[["X"]], y = col_data[["Y"]]),
         data = col_data))
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

  expect_null(common_model_data_configuration(NULL, NULL, NULL))
  expect_null(common_model_data_configuration(model, NULL, NULL))
  expect_null(common_model_data_configuration(NULL, data, NULL))

  res <- common_model_data_configuration(model, data, link)
  expect_equal(res$data, data)
  expect_equal(res$model, model)
  expect_equal(res$link, link)
  expect_equal(
    res$pars,
    cbind(model$result$info$pars,
          value = 1:2,
          id_value = paste0("par_value_", c("a", "b")),
          stringsAsFactors = FALSE))
  expect_equal(
    res$vars,
    cbind(model$result$info$vars,
          id_graph_option = paste0("var_graph_option_", c("X", "Y")),
          stringsAsFactors = FALSE))
  expect_equal(res$link, link)
  expect_equal(res$cols, odin_colours(c("X", "Y"), c("x", "y"), link))

  configure <- list(link = list(x = "X"))
  res <- common_model_data_configuration(model, data, configure)
  expect_equal(res$data, data)
  expect_equal(res$model, model)
  expect_equal(res$link, configure$link)
  expect_equal(
    res$pars,
    cbind(model$result$info$pars,
          value = 1:2,
          id_value = paste0("par_value_", c("a", "b")),
          stringsAsFactors = FALSE))
  expect_equal(
    res$vars,
    cbind(model$result$info$vars,
          id_graph_option = paste0("var_graph_option_", c("X", "Y")),
          stringsAsFactors = FALSE))
  expect_equal(res$link, configure$link)
  expect_equal(res$cols,
               odin_colours(c("X", "Y"), c("x", "y"), configure$link))
})
