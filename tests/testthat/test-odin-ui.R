context("odin.ui")


test_that("generated parameter interface includes all parameters", {
  pars <- data_frame(name = c("N0", "K", "r"),
                     has_default = TRUE,
                     default_value = I(list(1, 100, 0.5)),
                     rank = 0L)
  gen <- mock_model(pars)

  parameters <- validate_model_parameters(gen, NULL)
  p <- mod_model_control_parameters(parameters, identity)

  expect_is(p, "list")
  expect_setequal(names(p), c("name_map", "tags"))

  id <- vapply(p$tags$children[[2]]$children[[1]]$children[[1]],
               function(x) x$children[[2]]$attribs$id, "")
  expect_equal(id, unname(p$name_map))

  expect_equal(unname(p$name_map), paste0("pars_", names(p$name_map)))
})


test_that("parameters must be scalar", {
  pars <- data_frame(name = "v",
                     has_default = FALSE,
                     default_value = I(list(NULL)),
                     rank = 1L)
  gen <- mock_model(pars)
  expect_error(validate_model_parameters(gen, NULL),
               "Only scalar parameters are currently supported")
})


test_that("parameters must have defaults", {
  pars <- data_frame(name = "v",
                     has_default = FALSE,
                     default_value = I(list(NULL)),
                     rank = 0L)
  gen <- mock_model(pars)
  expect_error(validate_model_parameters(gen, NULL),
               "All parameters must have defaults")
})


test_that("models can have no parameters", {
  pars <- data_frame(name = character(0),
                     has_default = logical(0),
                     default_value = I(list()),
                     rank = integer(0))
  gen <- mock_model(pars)
  parameters <- validate_model_parameters(gen, NULL)
  expect_identical(mod_model_control_parameters(parameters, identity),
                   list(tags = list(), name_map = character(0)))
})


test_that("time can be length 1", {
  res <- mod_model_control_time(10, list(discrete = FALSE), identity)
  expect_false(res$has_start_time)

  tags <- res$tags$children[[2]]$children[[1]]$children[[1]]
  expect_equal(length(tags), 2L)

  expect_equal(tags[[1]]$children[[2]]$attribs$id, "time_end")
})


test_that("time can be length 2", {
  res <- mod_model_control_time(c(0, 10), list(discrete = FALSE), identity)
  expect_true(res$has_start_time)

  tags <- res$tags$children[[2]]$children[[1]]$children[[1]]
  expect_equal(length(tags), 3L)
  expect_equal(tags[[1]]$children[[2]]$attribs$id, "time_start")
  expect_equal(tags[[2]]$children[[2]]$attribs$id, "time_end")
})


test_that("time must be length 1-2", {
  expect_error(mod_model_control_time(numeric(), identity),
               "'default_time' must be length 1 or 2", fixed = TRUE)
  expect_error(mod_model_control_time(1:3, identity),
               "'default_time' must be length 1 or 2", fixed = TRUE)
})
