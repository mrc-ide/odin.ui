context("module: configure")

test_that("data summary", {
  expect_equal(configure_data_summary(NULL),
               "Please upload data")
  expect_equal(configure_data_summary(list()),
               "Please select time variable for your data")
  expect_equal(configure_data_summary(list(configured = TRUE,
                                           data = matrix(0, 3, 4))),
               "3 rows of data have been uploaded")
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
