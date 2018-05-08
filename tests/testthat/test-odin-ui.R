context("odin.ui")


test_that("generated parameter interface includes all parameters", {
  pars <- data_frame(name = c("N0", "K", "r"),
                     has_default = TRUE,
                     default_value = c(1, 100, 0.5),
                     rank = 0L)
  gen <- mock_model(pars)

  p <- odin_ui_parameters(gen)

  expect_is(p, "list")
  expect_setequal(names(p), c("name_map", "tags"))
  expect_equal(length(p$tags), 4L)

  id <- vapply(p$tags[-1], function(x) x$children[[2]]$attribs$id, "")
  expect_equal(id, unname(p$name_map))

  expect_equal(unname(p$name_map), paste0("pars_", names(p$name_map)))
})


test_that("parameters must be scalar", {
  pars <- data_frame(name = "v",
                     has_default = FALSE,
                     default_value = NA_real_,
                     rank = 1L)
  gen <- mock_model(pars)
  expect_error(odin_ui_parameters(gen),
               "Only scalar parameters are currently supported")
})


test_that("parameters must have defaults", {
  pars <- data_frame(name = "v",
                     has_default = FALSE,
                     default_value = NA_real_,
                     rank = 0L)
  gen <- mock_model(pars)
  expect_error(odin_ui_parameters(gen),
               "All parameters must have defaults")
})
