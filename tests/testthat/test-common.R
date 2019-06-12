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
