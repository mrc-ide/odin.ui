context("module: configure")

test_that("link status", {
  expect_equal(configure_link_status(character(0)), "No linked variables")
  expect_equal(configure_link_status("a"), "a")
  expect_equal(configure_link_status(c("a", "b")), "a & b")
})


test_that("link update", {
  vars_data <- c("A", "B")
  vars_model <- c("a", "b", "c")
  data <- list(name_vars = vars_data, configured = TRUE)
  model <- list(result = list(info = list(vars = list(name = vars_model))))

  ## Nothing selected:
  expect_equal(
    configure_link_ui_update(NULL, NULL, data, model, NULL),
    list(
      map = c(A = "link_data_A", B = "link_data_B"),
      selected = list(A = NULL, B = NULL),
      vars_data = vars_data,
      vars_model = vars_model))
})


test_that("configure status", {
  expect_equal(
    configure_status(NULL, NULL),
    module_status("danger", "Model/Data link is not configured", NULL))
  expect_equal(
    configure_status(NULL, "reason"),
    module_status("danger", "Model/Data link is not configured", "reason"))

  expect_equal(
    configure_status(FALSE, NULL),
    module_status("danger", "Model/Data link is not configured", NULL))
  expect_equal(
    configure_status(FALSE, "reason"),
    module_status("danger", "Model/Data link is not configured", "reason"))

  expect_equal(
    configure_status(TRUE, NULL),
    module_status("success", "Model/Data link is configured", NULL))
  expect_equal(
    configure_status(TRUE, "reason"),
    module_status("success", "Model/Data link is configured", NULL))
})
