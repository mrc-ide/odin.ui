context("util (assert)")


test_that("assert_file_exists", {
  expect_silent(assert_file_exists(odin_ui_file("minimal_model.R")))
  expect_error(assert_file_exists("nosuchfile.R"),
               "File does not exist: 'nosuchfile.R'")
})
