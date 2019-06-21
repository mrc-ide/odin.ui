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


test_that("write_csv does not add row labels", {
  d <- data.frame(a = 1:2, b = 3:4)
  path <- tempfile()
  on.exit(unlink(path))
  write_csv(d, path)
  expect_equal(readLines(path)[[2]], c("1,3"))
})


test_that("drop_null", {
  empty <- set_names(list(), character())
  expect_null(drop_null(NULL))
  expect_equal(drop_null(list()), list())
  expect_equal(drop_null(list(a = NULL)), empty)
  expect_equal(drop_null(list(a = NULL, b = NULL)), empty)
  expect_equal(drop_null(list(a = 1, b = NULL)), list(a = 1))
  expect_equal(drop_null(list(a = 1, b = 2)), list(a = 1, b = 2))
})


test_that("list_to_numeric", {
  expect_equal(list_to_numeric(list(a = 1, b = 2)), c(1, 2))
  expect_equal(list_to_numeric(list(a = 1, b = 2), TRUE), c(a = 1, b = 2))
})


test_that("list_to_logical", {
  expect_equal(list_to_logical(list(a = TRUE, b = FALSE)),
               c(TRUE, FALSE))
  expect_equal(list_to_logical(list(a = TRUE, b = FALSE), TRUE),
               c(a = TRUE, b = FALSE))
})


test_that("list_to_character", {
  expect_equal(list_to_character(list(a = "a", b = "2")), c("a", "2"))
  expect_equal(list_to_character(list(a = "a", b = "2"), TRUE),
               c(a = "a", b = "2"))
})


test_that("list_to_df", {
  x <- list(a = 1, b = 2)
  y <- list_to_df(x)
  expect_equal(y, data_frame(name = c("a", "b"), value = c(1, 2)))
  expect_equal(df_to_list(y), x)
})


test_that("constrain", {
  expect_equal(constrain(1, 0, 2), 1)
  expect_equal(constrain(-1, 0, 2), 0)
  expect_equal(constrain(3, 0, 2), 2)
})


test_that("with_success", {
  f <- function(x) {
    if (x < 0) {
      stop("x must be positive")
    }
    sqrt(x)
  }

  expect_equal(
    with_success(f(-1)),
    list(success = FALSE, value = NULL, error = "x must be positive"))
  expect_equal(
    with_success(f(4)),
    list(success = TRUE, value = 2, error = NULL))
})


test_that("protect", {
  f <- function(x) {
    if (x < 0) {
      stop("x must be positive")
    }
    sqrt(x)
  }

  g <- protect(f, NA)
  expect_equal(g(4), 2)
  expect_equal(g(-4), NA)
})
