context("module: csv")

test_that("read simple csv", {
  path <- tempfile()
  filename <- "myfile.csv"
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = 1:5, c = 1:5)
  write_csv(d, path)

  cmp <- list(success = TRUE,
              value = list(data = d, filename = filename,
                           info = list(choices = names(d), selected = NA)),
              error = NULL)

  ## Happy path all the same:
  expect_equal(
    csv_import_result(d, filename), cmp)
  expect_equal(
    csv_validate(d, filename, 2, 1), cmp)
  expect_equal(
    csv_import(path, filename, min_rows = 1), cmp)
})


test_that("invalid csv: too short", {
  path <- tempfile()
  filename <- "myfile.csv"
  on.exit(unlink(path))

  d <- data_frame(a = 1:5, b = runif(5), c = runif(5))
  write_csv(d, path)

  expect_equal(
    csv_import(path, filename, min_rows = 10, min_cols = 1),
    list(success = FALSE,
         value = NULL,
         error = "Expected at least 10 rows"))
  expect_equal(
    csv_import(path, filename, min_cols = 5, min_rows = 1),
    list(success = FALSE,
         value = NULL,
         error = "Expected at least 5 columns"))
})


test_that("invalid csv: duplicate names", {
  d <- data_frame(a = 1:5, b = runif(5), a = runif(5))
  filename <- "myfile.csv"
  expect_equal(
    csv_validate(d, filename, min_rows = 1, min_cols = 1),
    list(success = FALSE,
         value = NULL,
         error = "Data contains duplicate names ('a')"))
})


test_that("Non numeric data", {
  d <- data_frame(a = 1:5, b = runif(5), c = letters[1:5])
  filename <- "myfile.csv"
  expect_equal(
    csv_validate(d, filename, min_rows = 1, min_cols = 1),
    list(success = FALSE,
         value = NULL,
         error = "All columns must be numeric ('c')"))
})


test_that("Completely incorrect data", {
  d <- data_frame(a = 1:5, b = letters[1:5], a = runif(5))
  filename <- "myfile.csv"
  expect_equal(
    csv_validate(d, filename, min_rows = 15, min_cols = 7),
    list(success = FALSE,
         value = NULL,
         error = c("Data contains duplicate names ('a')",
                   "All columns must be numeric ('b')",
                   "Expected at least 7 columns",
                   "Expected at least 15 rows")))
})


test_that("Invalid csv", {
  path <- tempfile()
  filename <- "myfile.csv"
  on.exit(unlink(path))
  writeLines(c("a,b", "1,2,3,4"), path)

  expected <- tryCatch(read_csv(path), error = identity)$message
  expect_equal(csv_import(path, filename),
               list(success = FALSE,
                    value = NULL,
                    error = expected))
})


test_that("guess time column", {
  d <- data_frame(a = 1:5, b = runif(5), c = runif(5))
  expect_equal(csv_guess_time(cbind(d, d = 6:10)),
               list(choices = c("a", "d"), selected = NA))
  expect_equal(csv_guess_time(d),
               list(choices = "a", selected = "a"))

  d[] <- 1:5

  f <- function(x) {
    names(d)[seq_along(x)] <- x
    csv_guess_time(d)
  }

  expect_equal(f("t"), list(choices = c("t", "b", "c"), selected = "t"))

  ## Then just test the selected:
  expect_equal(f("time")$selected, "time")
  expect_equal(f("day")$selected, "day")
  expect_equal(f("date")$selected, "date")
  expect_equal(f("week")$selected, "week")
  expect_equal(f("year")$selected, "year")

  expect_equal(f("Day")$selected, "Day")
  expect_equal(f("wEek")$selected, "wEek")

  expect_equal(f(c("day", "week"))$selected, NA)
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
  d <- data.frame(a = 1:4, b = 1:4, c = 1:4)
  f <- "myfile.csv"
  imported <- csv_validate(d, f, 1, 1)
  result <- odin_data_source(d, f, "a")

  expect_equal(
    csv_summary(imported, result),
    simple_panel("success",
                 "Uploaded 4 rows and 3 columns",
                 "Response variables: b, c"))
})


test_that("can't import a csv with no increasing data", {
  n <- 1000
  d <- data.frame(a = runif(n), b = runif(n), c = runif(n))
  f <- "myfile.csv"
  imported <- csv_validate(d, f, 1, 1)
  expect_equal(
    imported,
    csv_import_error("None of the columns are strictly increasing"))
})


test_that("csv summary: unconfigured", {
  d <- data.frame(a = 1:4, b = 1:4, c = 1:4)
  f <- "myfile.csv"
  imported <- csv_validate(d, f, 1, 1)
  result <- odin_data_source(d, f, NULL)

  expect_equal(
    csv_summary(imported, result),
    simple_panel("info",
                 "Uploaded 4 rows and 3 columns",
                 "Select a time variable to view plot"))
})


test_that("csv summary: no data", {
  expect_equal(
    csv_summary(NULL, NULL),
    simple_panel("info", "Upload a data set to begin", NULL))
})


test_that("csv summary: errors", {
  d <- data.frame(a = 1:4, b = 1:4, c = 1:4)
  f <- "myfile.csv"
  imported <- csv_validate(d, f, 9, 10)
  result <- NULL

  expect_equal(
    csv_summary(imported, result),
    simple_panel("danger", "Error loading csv", unordered_list(imported$error)))
})


test_that("csv status", {
  d <- data.frame(a = 1:4, b = 1:4, c = 1:4)
  f <- "myfile.csv"
  result <- odin_data_source(d, f, NULL)

  expect_equal(
    csv_status(NULL, NULL),
    module_status("danger", "Data not present", NULL))
  expect_equal(
    csv_status(NULL, "solution"),
    module_status("danger", "Data not present", "solution"))

  expect_equal(
    csv_status(odin_data_source(d, f, NULL), NULL),
    module_status("danger", "Please select time variable for your data", NULL))
  expect_equal(
    csv_status(odin_data_source(d, f, NULL), "solution"),
    module_status("danger", "Please select time variable for your data",
                  "solution"))

  expect_equal(
    csv_status(odin_data_source(d, f, "a"), NULL),
    module_status("success", "4 rows of data have been uploaded", NULL))
  expect_equal(
    csv_status(odin_data_source(d, f, "a"), "solution"),
    module_status("success", "4 rows of data have been uploaded", NULL))
})


test_that("csv plot: unconfigured", {
  d <- data.frame(a = 1:4, b = 1:4, c = 1:4)
  f <- "myfile.csv"
  expect_null(csv_plot_series(odin_data_source(d, f, NULL)))
  expect_null(csv_plot(odin_data_source(d, f, NULL)))
})


test_that("csv plot: configured", {
  d <- data.frame(a = 1:4, b = 2:5, c = 3:6)
  f <- "myfile.csv"
  result <- odin_data_source(d, f, "b")
  series <- csv_plot_series(result)
  expect_equal(length(series), 2)
  expect_equal(vcapply(series, "[[", "name"), c("a", "c"))
  expect_equal(vcapply(series, function(x) x$marker$color),
               unname(odin_colours_data(c("a", "c"))))
  expect_equal(series[[1]]$x, 2:5)
  expect_equal(series[[2]]$x, 2:5)

  expect_equal(series[[1]]$y, 1:4)
  expect_equal(series[[2]]$y, 3:6)

  expect_is(csv_plot(result), "plotly")
})


test_that("csv result returns an odin_data_source", {
  d <- data.frame(a = 1:10, b = runif(10))
  filename <- "myfile.csv"
  result <- csv_import_result(d, filename)
  expect_equal(csv_result(result$value, "b"),
               odin_data_source(d, filename, "b"))
})


test_that("can't use files with missing column names", {
  d <- data.frame("x" = 1:10, b = runif(10), c = runif(10))
  names(d)[[1]] <- ""
  expect_equal(
    csv_validate(d, "file.csv", 1, 1),
    csv_import_error("Data contains blank column names (1)"))
  names(d)[[3]] <- ""
  expect_equal(
    csv_validate(d, "file.csv", 1, 1),
    csv_import_error(
      c("Data contains duplicate names ('')",
        "Data contains blank column names (1, 3)")))
})
