context("module: editor")

test_that("validate initial code", {
  expect_equal(editor_validate_initial_code(character(0)), "")
  expect_equal(editor_validate_initial_code(c("x", "y")), "x\ny\n")
  expect_equal(editor_validate_initial_code(c("x\ny")), "x\ny\n")
  expect_equal(editor_validate_initial_code(NULL),
               "deriv(x) <- x * r\ninitial(x) <- 1\nr <- user(1)\n")
  expect_error(editor_validate_initial_code(1),
               "'initial_code' must be a character vector")
})


test_that("editor_border", {
  expect_equal(editor_border(NULL), "normal")
  expect_equal(editor_border(list(error = "x")), "alert")
  expect_equal(editor_border(list(error = NULL)), "normal")
})


test_that("editor validation info: empty", {
  expect_null(editor_validation_info(NULL))
})


test_that("editor validation info: error", {
  validation <- common_odin_validate("")
  expect_equal(
    editor_validation_info(validation),
    simple_panel("danger", "Validation: error", shiny::pre(validation$error)))
})


test_that("editor validation info: notes", {
  validation <- common_odin_validate(
    "deriv(a) <- 1; initial(a) <- 1; b <- 1; c <- 1")
  expect_equal(
    editor_validation_info(validation),
    simple_panel("info", "Validation: note",
                 shiny::pre(paste(validation$messages, collapse = "\n\n"))))
})


test_that("editor validation info: success", {
  validation <- common_odin_validate(
    "deriv(a) <- 1; initial(a) <- 1")
  expect_equal(
    editor_validation_info(validation),
    simple_panel("success", "Validation: success", NULL))
})


test_that("editor compilation info: empty", {
  expect_null(editor_result_info(NULL))
})


test_that("editor compilation info: success", {
  d1 <- list(
    result = list(success = TRUE,
                  elapsed = list(elapsed = 1.2),
                  output = c("a", "b"),
                  error = NULL),
    is_current = TRUE)
  expect_equal(editor_compilation_info(d1),
               simple_panel(
                 "success",
                 "Compilation: success, 1.20 s elapsed",
                 shiny::pre("a\nb")))

  d1$is_current <- FALSE
  expect_equal(editor_compilation_info(d1),
               simple_panel(
                 "default",
                 paste("Compilation: success, 1.20 s elapsed",
                       "(code has changed since this was run)"),
                 shiny::pre("a\nb"),
                 "check-circle"))

  d2 <- list(
    result = list(success = FALSE,
                  elapsed = list(elapsed = 1.2),
                  output = NULL,
                  error = "failure"),
    is_current = TRUE)
  expect_equal(editor_compilation_info(d2),
               simple_panel(
                 "danger",
                 "Compilation: error, 1.20 s elapsed",
                 shiny::pre("failure"),
                 "times-circle"))

  d2$is_current <- FALSE
  expect_equal(editor_compilation_info(d2),
               simple_panel(
                 "warning",
                 paste("Compilation: error, 1.20 s elapsed",
                       "(code has changed since this was run)"),
               shiny::pre("failure"),
               "times-circle"))
})


test_that("model status: no callback", {
  m <- list(result = list(info = list(pars = matrix(0, 3, 0),
                                      vars = matrix(0, 4, 0))),
            is_current = FALSE)
  expect_equal(
    editor_status(NULL, NULL),
    module_status("danger", "Please compile a model", NULL))
  expect_equal(
    editor_status(NULL, "solution"),
    module_status("danger", "Please compile a model", "solution"))

  expect_equal(
    editor_status(m, NULL),
    module_status(
      "warning", "Model with 3 parameters and 4 variables/outputs",
      "Warning: model is out of date, consider recompiling the model."))
  expect_equal(
    editor_status(m, "solution"),
    module_status(
      "warning", "Model with 3 parameters and 4 variables/outputs",
      shiny::tagList(
        "Warning: model is out of date, consider recompiling the model.",
        "solution")))

  m$is_current <- TRUE
  expect_equal(
    editor_status(m, NULL),
    module_status("success", "Model with 3 parameters and 4 variables/outputs",
                  NULL))
  expect_equal(
    editor_status(m, "solution"),
    module_status("success", "Model with 3 parameters and 4 variables/outputs",
                  NULL))
})


test_that("read code", {
  p <- tempfile()
  on.exit(unlink(p))

  file.create(p)
  expect_equal(editor_read_code(p), "\n")

  writeLines(c("a", "b"), p)
  expect_equal(editor_read_code(p), "a\nb\n")

  ## silent in the face of no trailing newline:
  writeBin(charToRaw("a\nb"), p)
  expect_equal(editor_read_code(p), "a\nb\n")
  expect_silent(editor_read_code(p))
})
