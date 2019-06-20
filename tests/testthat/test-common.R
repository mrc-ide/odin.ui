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


test_that("odin_data_source handles unconfigured data", {
  d <- data_frame(a = 1:10, b = runif(10), c = runif(10))
  f <- "myfile.csv"
  cmp <- list(data = d, filename = f, configured = FALSE)
  expect_equal(odin_data_source(d, f, NULL), cmp)
  expect_equal(odin_data_source(d, f, NA), cmp)
  expect_equal(odin_data_source(d, f, ""), cmp)
  expect_equal(odin_data_source(d, f, "x"), cmp)
})


test_that("odin_data_source handles configured data", {
  d <- data_frame(a = 1:10, b = runif(10), c = runif(10))
  f <- "myfile.csv"
  expect_equal(
    odin_data_source(d, f, "b"),
    list(data = d, filename = f, configured = TRUE, name_time = "b",
         name_vars = c("a", "c"), cols = odin_colours_data(c("a", "c"))))
})


test_that("code validation failure", {
  res <- common_odin_validate("")
  expect_false(res$success)
  expect_null(res$result)
  expect_is(res$error, "character")
})


test_that("odin code validation success", {
  code <- "deriv(a) <- 1; initial(a) <- 1"
  res <- common_odin_validate(code)
  expect_true(res$success)
  expect_is(res$result, "json")
  expect_null(res$error)
  expect_equal(res$code, code)
  expect_equal(length(res$nodes), 0)
})


test_that("odin code validation notes", {
  code <- "deriv(a) <- 1; initial(a) <- 1; b <- 1"
  res <- common_odin_validate(code)
  expect_true(res$success)
  expect_is(res$result, "json")
  expect_null(res$error)
  expect_equal(res$code, code)
  expect_equal(length(res$messages), 1)
})


test_that("odin code compilation success", {
  code <- "deriv(a) <- 1; initial(a) <- b; b <- user(min = 0)"
  res <- common_odin_compile(common_odin_validate(code))
  expect_true(res$success)
  expect_is(res$elapsed, "numeric")
  expect_is(res$model, "odin_generator")
  expect_is(res$ir, "json")
  expect_null(res$error)
  expect_equal(res$code, code)
  expect_true(res$is_current)
  expect_equal(res$info$pars, coef(res$model))
  expect_equal(res$info$vars,
               data_frame(name = "a", rank = 0, type = "variable"))
})


test_that("odin code compilation failure", {
  code <- "deriv(a) <- 1; b <- user(min = 0)"
  res <- common_odin_compile(common_odin_validate(code))
  expect_false(res$success)
  expect_null(res$model)
  expect_equal(res$code, code)
  expect_true(res$is_current)
})


test_that("odin_model has sensible output on null input", {
  code <- "code"
  expect_equal(odin_model(NULL, code),
               list(success = FALSE, code = code, is_current = TRUE))
})


test_that("model build success", {
  code <- "deriv(a) <- b; initial(a) <- 1; b <- user(2, min = 0)"
  result <- odin::odin_build(odin::odin_parse(code))
  res <- odin_model(result, code)
  expect_equal(res$info, model_info(result$model))
  expect_equal(res$code, code)
  expect_true(res$is_current)
  expect_equal(res$model, result$model)
  expect_equal(res$ir, result$ir)
})


test_that("validate: error", {
  expect_equal(common_odin_validate(""),
               list(success = FALSE,
                    result = NULL,
                    error = "Did not find a deriv() or an update() call",
                    messages = character(0),
                    code = ""))
})


test_that("validate: success", {
  res <- common_odin_validate(c("initial(x) <- 1", "update(x) <- 1"))
  expect_true(res$success)
  expect_null(res$error)
  expect_equal(res$messages, character(0))
  expect_is(res$result, "json")
})


test_that("validate: note", {
  res <- common_odin_validate(c("a <- 1", "initial(x) <- 1", "update(x) <- 1"))
  expect_true(res$success)
  expect_null(res$error)
  expect_equal(res$messages, "Unused equation: a\n\ta <- 1 # (line 1)")
  expect_is(res$result, "json")
})
