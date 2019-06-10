context("module: csv")

test_that("read simple csv", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), c = runif(5))
  write_csv(d, path)

  expect_equal(
    csv_process(path, min_rows = 1),
    list(success = TRUE,
         data = d,
         error = NULL,
         vars = names(d),
         guess = NA))
})


test_that("invalid csv: too short", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), c = runif(5))
  write_csv(d, path)

  expect_equal(
    csv_process(path, min_rows = 10, min_cols = 1),
    list(success = FALSE,
         data = NULL,
         error = "Expected at least 10 rows"))
  expect_equal(
    csv_process(path, min_cols = 5, min_rows = 1),
    list(success = FALSE,
         data = NULL,
         error = "Expected at least 5 columns"))
})


test_that("invalid csv: duplicate names", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), a = runif(5))
  write_csv(d, path)

  expect_equal(
    csv_process(path, min_rows = 1, min_cols = 1),
    list(success = FALSE,
         data = NULL,
         error = "Data contains duplicate names ('a')"))
})


test_that("Non numeric data", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), c = letters[1:5])
  write_csv(d, path)
  expect_equal(
    csv_process(path, min_rows = 1, min_cols = 1),
    list(success = FALSE,
         data = NULL,
         error = "All columns must be numeric ('c')"))
})


test_that("Completely incorrect data", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = letters[1:5], a = runif(5))
  write_csv(d, path)

  expect_equal(
    csv_process(path, min_rows = 15, min_cols = 7),
    list(success = FALSE,
         data = NULL,
         error = c("Data contains duplicate names ('a')",
                   "All columns must be numeric ('b')",
                   "Expected at least 7 columns",
                   "Expected at least 15 rows")))
})


test_that("Invalid csv", {
  path <- tempfile()
  on.exit(unlink(path))
  writeLines(c("a,b", "1,2,3,4"), path)

  expected <- tryCatch(read_csv(path), error = identity)$message
  expect_equal(csv_process(path),
               list(success = FALSE,
                    data = NULL,
                    error = expected))
})


test_that("guess time column", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), c = runif(5))

  f <- function(x) {
    names(d)[[1]] <- x
    write_csv(d, path)
    csv_process(path, min_rows = 1, min_cols = 1)
  }

  expect_equal(f("t")$guess, "t")
  expect_equal(f("time")$guess, "time")
  expect_equal(f("day")$guess, "day")
  expect_equal(f("date")$guess, "date")
  expect_equal(f("week")$guess, "week")
  expect_equal(f("year")$guess, "year")

  expect_equal(f("Day")$guess, "Day")
  expect_equal(f("wEek")$guess, "wEek")

  names(d)[1:2] <- c("day", "week")
  write_csv(d, path)
  expect_equal(csv_process(path, min_rows = 1, min_cols = 1)$guess, NA)
})


test_that("csv configure", {
  path <- tempfile()
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), c = runif(5))
  write_csv(d, path)

  data <- csv_process(path, min_rows = 1)

  expect_equal(csv_configure(data, "b"),
               c(data, list(name_time = "b",
                            name_vars = c("a", "c"),
                            configured = TRUE)))

  expect_equal(csv_configure(data, ""),
               c(data, list(name_vars = c("a", "b", "c"),
                            configured = FALSE)))
  expect_equal(csv_configure(data, NULL),
               c(data, list(name_vars = c("a", "b", "c"),
                            configured = FALSE)))
})


test_that("csv summary: errors", {
  res <- csv_summary(list(success = FALSE, error = c("a", "b")))
  expect_equal(res$children[[1]]$attribs$class,
               "panel panel-danger")
  expect_equal(res$children[[1]]$children[[1]]$children[[2]],
               "Error loading csv")
  reasons <- res$children[[1]]$children[[2]]$children[[1]]
  expect_equal(reasons,
               shiny::tags$ul(list(shiny::tags$li("a"), shiny::tags$li("b"))))
})


test_that("csv summary: configured", {
  res <- csv_summary(list(success = TRUE, configured = TRUE,
                          data = matrix(0, 3, 4), name_vars = c("a", "b")))
  expect_equal(res$children[[1]]$attribs$class,
               "panel panel-success")
  expect_equal(res$children[[1]]$children[[1]]$children[[2]],
               "Uploaded 3 rows and 4 columns")
  expect_equal(res$children[[1]]$children[[2]]$children[[1]],
               "Response variables: a, b")
})


test_that("csv summary: unconfigured", {
  res <- csv_summary(list(success = TRUE, configured = FALSE,
                          data = matrix(0, 3, 4), name_vars = c("a", "b")))
  expect_equal(res$children[[1]]$attribs$class,
               "panel panel-info")
  expect_equal(res$children[[1]]$children[[1]]$children[[2]],
               "Uploaded 3 rows and 4 columns")
  expect_equal(res$children[[1]]$children[[2]]$children[[1]],
               "Select a time variable to view plot")
})
