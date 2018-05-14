context("util")


test_that("transparent colour utility works", {
  ## two easy cases
  expect_equal(transp("#123456", 0), "#12345600")
  expect_equal(transp("#123456", 1), "#123456FF")
  ## and one real one
  expect_equal(transp("#123456", .5), "#12345680")
  ## empty input
  expect_equal(
    transp(character(0), 0.5),
    character(0))
  ## vector output
  expect_equal(
    transp(c("black", "red", "green", "blue", "white"), .5),
    c("#00000080", "#FF000080", "#00FF0080", "#0000FF80", "#FFFFFF80"))
})


test_that("system file helper fails when file is missing", {
  expect_error(odin_ui_file("asdfas.R"))
  expect_silent(p <- odin_ui_file("minimal_model.R"))
  expect_true(file.exists(p))
})


test_that("read_text gives scalar character", {
  txt <- read_text(odin_ui_file("minimal_model.R"))
  expect_is(txt, "character")
  expect_equal(length(txt), 1L)
  ## Has trailing newline:
  expect_match(txt, "\n$")
})


test_that("run_app can pass app through", {
  expect_null(run_app(NULL, FALSE))
})


test_that("typed vapply", {
  x <- list(TRUE, FALSE, NA)
  expect_identical(vlapply(x, identity), c(TRUE, FALSE, NA))
  expect_error(vlapply(x, as.character))
  expect_identical(vcapply(x, as.character), c("TRUE", "FALSE", NA))
  expect_error(vcapply(x, identity))
})


test_that("round_time", {
  expect_equal(round_time(pi), 3.14)

  x <- seq(0, 10, length.out = 100)
  expect_equal(round_time(x), round(x, 2))
  expect_equal(round_time(numeric(0)), numeric(0))
})
