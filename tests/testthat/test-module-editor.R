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


## TODO: compilation failure after validation success

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
  expect_null(editor_model_info(NULL))
})


test_that("editor compilation info: success", {
  id <- ids::random_id()
  d1 <- list(success = TRUE,
             elapsed = list(elapsed = 1.2),
             output = c("a", "b"),
             error = NULL,
             is_current = TRUE)
  expect_equal(editor_model_info(d1, id = id),
               panel_collapseable(
                 "success",
                 "Compilation: success, 1.20 s elapsed",
                 shiny::pre("a\nb"),
                 collapsed = TRUE, id = id))

  d1$is_current <- FALSE
  expect_equal(editor_model_info(d1, id = id),
               panel_collapseable(
                 "default",
                 paste("Compilation: success, 1.20 s elapsed",
                       "(code has changed since this was run)"),
                 shiny::pre("a\nb"),
                 "check-circle", collapsed = TRUE, id = id))

  d1$output <- character(0)
  expect_equal(editor_model_info(d1, id = id),
               panel_collapseable(
                 "default",
                 paste("Compilation: success, 1.20 s elapsed",
                       "(code has changed since this was run)"),
                 NULL,
                 "check-circle", collapsed = TRUE, id = id))

  d2 <- list(success = FALSE,
             elapsed = list(elapsed = 1.2),
             output = NULL,
             error = "failure",
             is_current = TRUE)
  expect_equal(editor_model_info(d2, id = id),
               panel_collapseable(
                 "danger",
                 "Compilation: error, 1.20 s elapsed",
                 shiny::pre("failure"),
                 "times-circle", collapsed = FALSE, id = id))

  d2$is_current <- FALSE
  expect_equal(editor_model_info(d2, id = id),
               panel_collapseable(
                 "warning",
                 paste("Compilation: error, 1.20 s elapsed",
                       "(code has changed since this was run)"),
               shiny::pre("failure"),
               "times-circle", collapsed = FALSE, id = id))
})


test_that("model status: no callback", {
  m <- list(info = list(pars = matrix(0, 3, 0),
                        vars = matrix(0, 4, 0)),
            model = TRUE,
            is_current = FALSE)
  expect_equal(
    editor_status(NULL, NULL),
    module_status("danger", "Please compile a model", NULL))
  expect_equal(
    editor_status(NULL, "solution"),
    module_status("danger", "Please compile a model", "solution"))

  code <- c("deriv(x) <- 1",
            "initial(x) <- a",
            "a <- user(3, min = 0)",
            "b <- user(max = 1)",
            "deriv(z) <- 1",
            "initial(z) <- 1",
            "deriv(y) <- 2",
            "initial(y) <- b")
  m <- editor_result(common_odin_compile_from_code(code), NULL)
  expect_equal(
    editor_status(m, NULL),
    module_status("success", "Model with 2 parameters and 3 variables/outputs",
                  NULL))
  expect_equal(
    editor_status(m, "solution"),
    module_status("success", "Model with 2 parameters and 3 variables/outputs",
                  NULL))

  m$is_current <- FALSE
  expect_equal(
    editor_status(m, NULL),
    module_status(
      "warning", "Model with 2 parameters and 3 variables/outputs",
      "Warning: model is out of date, consider recompiling the model."))
  expect_equal(
    editor_status(m, "solution"),
    module_status(
      "warning", "Model with 2 parameters and 3 variables/outputs",
      shiny::tagList(
        "Warning: model is out of date, consider recompiling the model.",
        "solution")))
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
