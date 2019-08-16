context("module: link")


test_that("ui requires a configuration", {
  expect_null(link_ui(NULL, NULL, identity))
})


test_that("link result with empty inputs", {
  expect_equal(link_result(NULL, NULL),
               list(map = NULL, label = character(0), configured = FALSE))
  empty <- set_names(list(), character())
  expect_equal(link_result(list(NA), "a"),
               list(map = empty, label = character(0), configured = FALSE))
  expect_equal(link_result(list(NULL, NULL), c("a", "b")),
               list(map = empty, label = character(0), configured = FALSE))
})


test_that("link result with nonempty inputs", {
  expect_equal(
    link_result(list(NA, "Y", "Z"), c("a", "b", "c")),
    list(map = list(b = "Y", c = "Z"),
         label = c("b ~ Y", "c ~ Z"),
         configured = TRUE))
})


test_that("configuration", {
  expect_null(link_configuration(NULL, NULL))
})


test_that("configuration: nonemtpy", {
  code <- c("deriv(x) <- 1",
            "initial(x) <- 1",
            "deriv(y) <- 2",
            "initial(y) <- 2")
  model <- editor_result(common_odin_compile_from_code(code), NULL)

  d <- data.frame(t = 1:10, a = runif(10), b = runif(10), c = runif(10))
  data <- odin_data_source(d, "file.csv", "t")

  expect_equal(
    link_configuration(data, model),
    list(data = data,
         model = model,
         vars = list(
           data = c("a", "b", "c"),
           model = c("x", "y"),
           id = sprintf("link_data_%s", c("a", "b", "c")))))

  ## Don't set configuration for all these cases:
  expect_null(link_configuration(NULL, NULL))
  expect_null(link_configuration(NULL, model))
  expect_null(link_configuration(data, NULL))

  data2 <- modifyList(data, list(configured = FALSE))
  model2 <- modifyList(model, list(success = FALSE))
  expect_null(link_configuration(data2, model))
  expect_null(link_configuration(data, model2))
})


test_that("status", {
  expect_equal(
    link_status(NULL, NULL),
    module_status("danger", "Model/Data link is not configured", NULL))
  expect_equal(
    link_status(NULL, "solution"),
    module_status("danger", "Model/Data link is not configured", "solution"))

  result <- link_result(list(NA, "Y", "Z"), c("a", "b", "c"))
  expect_equal(
    link_status(result, NULL),
    module_status("success", "Model/Data link is configured",
                  "b ~ Y & c ~ Z"))
  expect_equal(
    link_status(result, NULL),
    link_status(result, "solution"))
})
