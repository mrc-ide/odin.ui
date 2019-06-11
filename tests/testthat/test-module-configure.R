context("module: configure")

test_that("data summary: no callback", {
  m <- matrix(0, 3, 4)
  expect_equal(configure_data_summary(NULL, NULL),
               simple_panel("danger", "Please upload data", NULL))
  expect_equal(
    configure_data_summary(list(configured = FALSE, data = m), NULL),
    simple_panel("danger", "Please select time variable for your data", NULL))
  expect_equal(
    configure_data_summary(list(configured = TRUE, data = m), NULL),
    simple_panel("success", "3 rows of data have been uploaded", NULL))
})


test_that("data summary: with callback", {
  m <- matrix(0, 3, 4)

  ns <- shiny::NS("module")
  session <- NULL
  data_tab <- goto_module("Data tab", NULL, "navbar", "Data")
  body <- shiny::tagList(
    "Return to the",
    shiny::actionLink(ns("goto_data"), data_tab$link_text))

  expect_equal(configure_data_summary(NULL, data_tab, ns),
               simple_panel("danger", "Please upload data", body))
  expect_equal(
    configure_data_summary(list(configured = FALSE, data = m), data_tab, ns),
    simple_panel("danger","Please select time variable for your data", body))
  expect_equal(
    configure_data_summary(list(configured = TRUE, data = m), data_tab, ns),
    simple_panel("success", "3 rows of data have been uploaded", NULL))
})


test_that("model summary", {
  expect_equal(configure_model_summary(NULL),
               "Please compile a model")

  x <- list(result = list(info = list(pars = matrix(0, 3, 0),
                                      vars = matrix(0, 4, 0))))
  expect_equal(configure_model_summary(x),
               "Model with 3 parameters and 4 variables/outputs")
})


test_that("model status", {
  expect_null(configure_model_status(NULL))
  expect_null(configure_model_status(list(is_current = TRUE)))
  expect_equal(configure_model_status(list(is_current = FALSE)),
               simple_panel("warning", "Warning: model is out of date",
                            "Consider recompiling the model"))
})


test_that("link status", {
  expect_equal(configure_link_status(character(0)), "No linked variables")
  expect_equal(configure_link_status("a"), "a")
  expect_equal(configure_link_status(c("a", "b")), "a & b")
})


test_that("link update", {
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
