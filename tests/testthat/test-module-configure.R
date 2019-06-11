context("module: configure")

test_that("data status: no callback", {
  m <- matrix(0, 3, 4)
  expect_equal(configure_data_status(NULL, NULL),
               simple_panel("danger", "Please upload data", NULL))
  expect_equal(
    configure_data_status(list(configured = FALSE, data = m), NULL),
    simple_panel("danger", "Please select time variable for your data", NULL))
  expect_equal(
    configure_data_status(list(configured = TRUE, data = m), NULL),
    simple_panel("success", "3 rows of data have been uploaded", NULL))
})


test_that("data status: with callback", {
  m <- matrix(0, 3, 4)

  ns <- shiny::NS("module")
  session <- NULL
  data_tab <- goto_module("Data tab", NULL, "navbar", "Data")
  body <- shiny::tagList(
    "Return to the",
    shiny::actionLink(ns("goto_data"), data_tab$link_text))

  expect_equal(configure_data_status(NULL, data_tab, ns),
               simple_panel("danger", "Please upload data", body))
  expect_equal(
    configure_data_status(list(configured = FALSE, data = m), data_tab, ns),
    simple_panel("danger","Please select time variable for your data", body))
  expect_equal(
    configure_data_status(list(configured = TRUE, data = m), data_tab, ns),
    simple_panel("success", "3 rows of data have been uploaded", NULL))
})


test_that("model status: no callback", {
  m <- list(result = list(info = list(pars = matrix(0, 3, 0),
                                      vars = matrix(0, 4, 0))),
            is_current = FALSE)
  expect_equal(
    configure_model_status(NULL, NULL),
    simple_panel("danger", "Please compile a model", NULL))

  expect_equal(
    configure_model_status(m, NULL),
    simple_panel(
      "warning", "Model with 3 parameters and 4 variables/outputs",
      "Warning: model is out of date, consider recompiling the model."))

  m$is_current <- TRUE
  expect_equal(
    configure_model_status(m, NULL),
    simple_panel("success", "Model with 3 parameters and 4 variables/outputs",
                 NULL))
})


test_that("model status: with callback", {
  m <- list(result = list(info = list(pars = matrix(0, 3, 0),
                                      vars = matrix(0, 4, 0))),
            is_current = FALSE)
  ns <- shiny::NS("module")
  session <- NULL
  model_tab <- goto_module("Model tab", NULL, "navbar", "Model")

  body <- shiny::tagList(
    "Return to the",
    shiny::actionLink(ns("goto_model"), model_tab$link_text))
  expect_equal(
    configure_model_status(NULL, model_tab, ns),
    simple_panel("danger", "Please compile a model", body))

  body <- shiny::tagList(
    "Warning: model is out of date, consider recompiling the model.",
    "Return to the",
    shiny::actionLink(ns("goto_model"), model_tab$link_text))
  expect_equal(
    configure_model_status(m, model_tab, ns),
    simple_panel(
      "warning", "Model with 3 parameters and 4 variables/outputs", body))

  m$is_current <- TRUE
  expect_equal(
    configure_model_status(m, model_tab, ns),
    simple_panel("success", "Model with 3 parameters and 4 variables/outputs",
                 NULL))
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
