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


test_that("editor validation info", {
  expect_null(editor_validation_info(NULL))

  res <- editor_validation_info(list(error = "failure"))
  expect_equal(res$children[[1]]$attribs$class, "panel panel-danger")
  expect_match(res$children[[1]]$children[[1]]$children[[1]]$attribs$class,
               "exclamation-circle")
  expect_equal(as.character(res$children[[1]]$children[[2]]$children[[1]]),
               "<pre>failure</pre>")

  res <- editor_validation_info(list(messages = c("a", "b")))
  expect_equal(res$children[[1]]$attribs$class, "panel panel-info")
  expect_match(res$children[[1]]$children[[1]]$children[[1]]$attribs$class,
               "info-circle")
  expect_equal(as.character(res$children[[1]]$children[[2]]$children[[1]]),
               "<pre>a\n\nb</pre>")

  res <- editor_validation_info(list())
  expect_equal(res$children[[1]]$attribs$class, "panel panel-success")
  expect_match(res$children[[1]]$children[[1]]$children[[1]]$attribs$class,
               "check-circle")
  expect_null(res$children[[1]]$children[[2]])
})


test_that("editor compilation info", {
  expect_null(editor_compilation_info(NULL))

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


test_that("validate: error", {
  expect_equal(editor_validate(""),
               list(success = FALSE,
                    result = NULL,
                    error = "Did not find a deriv() or an update() call",
                    messages = character(0)))
})


test_that("validate: success", {
  res <- editor_validate(c("initial(x) <- 1", "update(x) <- 1"))
  expect_true(res$success)
  expect_null(res$error)
  expect_equal(res$messages, character(0))
  expect_is(res$result, "json")
})


test_that("validate: note", {
  res <- editor_validate(c("a <- 1", "initial(x) <- 1", "update(x) <- 1"))
  expect_true(res$success)
  expect_null(res$error)
  expect_equal(res$messages, "Unused equation: a\n\ta <- 1 # (line 1)")
  expect_is(res$result, "json")
})


test_that("editor compile: failure", {
  res <- editor_compile("")
  expect_equal(
    res,
    list(validation = editor_validate(""),
         compilation = list(code = "", result = NULL, is_current = TRUE)))
})


test_that("editor compile: success", {
  code <- c("initial(x) <- a", "deriv(x) <- 1", "a <- user(1)")
  res <- editor_compile(code)
  expect_equal(res$validation, editor_validate(code))
  expect_equal(res$compilation$code, code)
  expect_true(res$compilation$is_current)

  expect_true(res$compilation$result$success)
  expect_is(res$compilation$result$elapsed[["elapsed"]], "numeric")
  expect_is(res$compilation$result$output, "character")
  expect_is(res$compilation$result$model, "odin_generator")
  expect_identical(res$compilation$result$ir, res$validation$result)
  expect_null(res$compilation$result$error)
  expect_equal(res$compilation$result$info$pars,
               coef(res$compilation$result$model))
  expect_equal(res$compilation$result$info$vars,
               data_frame(name = "x", rank = 0, type = "variable"))
})



test_that("model status: no callback", {
  m <- list(result = list(info = list(pars = matrix(0, 3, 0),
                                      vars = matrix(0, 4, 0))),
            is_current = FALSE)
  expect_equal(
    editor_status(NULL, NULL),
    simple_panel("danger", "Please compile a model", NULL))

  expect_equal(
    editor_status(m, NULL),
    simple_panel(
      "warning", "Model with 3 parameters and 4 variables/outputs",
      "Warning: model is out of date, consider recompiling the model."))

  m$is_current <- TRUE
  expect_equal(
    editor_status(m, NULL),
    simple_panel("success", "Model with 3 parameters and 4 variables/outputs",
                 NULL))
})


test_that("model status: with callback", {
  m <- list(result = list(info = list(pars = matrix(0, 3, 0),
                                      vars = matrix(0, 4, 0))),
            is_current = FALSE)
  ns <- shiny::NS("module")
  session <- NULL
  editor_tab <- goto_module("Model tab", NULL, "navbar", "Model")

  body <- shiny::tagList(
    "Return to the",
    shiny::actionLink(ns("goto_editor"), editor_tab$link_text))
  expect_equal(
    editor_status(NULL, editor_tab, ns),
    simple_panel("danger", "Please compile a model", body))

  body <- shiny::tagList(
    "Warning: model is out of date, consider recompiling the model.",
    "Return to the",
    shiny::actionLink(ns("goto_editor"), editor_tab$link_text))
  expect_equal(
    editor_status(m, editor_tab, ns),
    simple_panel(
      "warning", "Model with 3 parameters and 4 variables/outputs", body))

  m$is_current <- TRUE
  expect_equal(
    editor_status(m, editor_tab, ns),
    simple_panel("success", "Model with 3 parameters and 4 variables/outputs",
                 NULL))
})
