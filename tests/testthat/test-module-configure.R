context("module: configure")


test_that("ui requires a configuration", {
  expect_null(configure_link_ui(NULL, identity))
})


test_that("link result with empty inputs", {
  expect_equal(configure_result(NULL),
               list(map = NULL, label = character(0), configured = FALSE))
  empty <- set_names(list(), character())
  expect_equal(configure_result(list(a = NA)),
               list(map = empty, label = character(0), configured = FALSE))
  expect_equal(configure_result(list(a = NULL, b = NULL)),
               list(map = empty, label = character(0), configured = FALSE))
})


test_that("link result with nonempty inputs", {
  expect_equal(
    configure_result(list(a = NA, b = "Y", c = "Z")),
    list(map = list(b = "Y", c = "Z"),
         label = c("b ~ Y", "c ~ Z"),
         configured = TRUE))
})


test_that("configuration", {
  expect_null(configure_configuration(NULL, NULL))
})


test_that("configuration: nonemtpy", {
  code <- c("deriv(x) <- 1",
            "initial(x) <- 1",
            "deriv(y) <- 2",
            "initial(y) <- 2")
  model <- common_odin_compile_from_code(code)

  d <- data.frame(t = 1:10, a = runif(10), b = runif(10), c = runif(10))
  data <- odin_data_source(d, "file.csv", "t")

  expect_equal(
    configure_configuration(data, model),
    list(data = data, model = model,
         vars = list(
           data = c("a", "b", "c"),
           model = c("x", "y"),
           id = sprintf("link_data_%s", c("a", "b", "c")))))

  ## Don't set configuration for all these cases:
  expect_null(configure_configuration(NULL, NULL))
  expect_null(configure_configuration(NULL, model))
  expect_null(configure_configuration(data, NULL))

  data2 <- modifyList(data, list(configured = FALSE))
  model2 <- modifyList(model, list(success = FALSE))
  expect_null(configure_configuration(data2, model))
  expect_null(configure_configuration(data, model2))
})


test_that("status", {
  expect_equal(
    configure_status(NULL, NULL),
    module_status("danger", "Model/Data link is not configured", NULL))
  expect_equal(
    configure_status(NULL, "solution"),
    module_status("danger", "Model/Data link is not configured", "solution"))

  result <- configure_result(list(a = NA, b = "Y", c = "Z"))
  expect_equal(
    configure_status(result, NULL),
    module_status("success", "Model/Data link is configured",
                  "b ~ Y & c ~ Z"))
  expect_equal(
    configure_status(result, NULL),
    configure_status(result, "solution"))
})


test_that("link update", {
  skip("Disabled")
  vars_data <- c("A", "B")
  vars_model <- c("a", "b", "c")
  data <- list(name_vars = vars_data, configured = TRUE)
  model <- list(result = list(info = list(vars = list(name = vars_model))))

  ## Nothing selected:
  expect_equal(
    configure_link_ui_update(NULL, NULL, data, model, NULL),
    list(
      map = c(A = "link_data_A", B = "link_data_B"),
      selected = list(A = NULL, B = NULL),
      vars_data = vars_data,
      vars_model = vars_model))
})
