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
