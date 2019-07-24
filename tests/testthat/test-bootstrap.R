context("bootstrap")

test_that("unordered list", {
  expect_null(unordered_list(NULL))
  expect_equal(unordered_list("a"),
               shiny::tags$ul(list(shiny::tags$li("a"))))
  expect_equal(unordered_list(c("a", "b")),
               shiny::tags$ul(list(shiny::tags$li("a"),
                                   shiny::tags$li("b"))))
  expect_equal(unordered_list(list("a", shiny::tags$i("b"))),
               shiny::tags$ul(list(shiny::tags$li("a"),
                                   shiny::tags$li(shiny::tags$i("b")))))
})


## Rubbish tests, but they get there...
test_that("simple_numeric_input", {
  expect_equal(
    simple_numeric_input("name", "label", value = 1),
    horizontal_form_group(shiny::span("name"),
                          raw_numeric_input("label", value = 1)))
})


test_that("simple_select_input", {
  choices <- c("a", "b", "c")
  selected <- NULL
  expect_equal(
    simple_select_input("name", "label",
                        choices = choices, selected = selected),
    horizontal_form_group(shiny::span("name"),
                          raw_select_input("label", choices = choices,
                                           selected = selected)))
})


## Hard to test these in any meaningful way:
test_that("button row", {
  expect_equal(
    button_row("label", "id", "button"),
    horizontal_form_group("label", shiny::actionButton("id", "button")))
  expect_equal(
    button_row("label", "id", "button", class = "cl"),
    horizontal_form_group("label",
                          shiny::actionButton("id", "button", class = "cl")))
})


test_that("simple slider", {
  expect_equal(
    simple_slider_input("name", "id", 0.5, list(from = 0, to = 1)),
    list(
      shiny::div(
        class = "form-group slider-label",
        shiny::div(class = "col-xs-12", shiny::tags$label("name"))),
      shiny::div(
        class = "form-group",
        shiny::div(class = "col-xs-12 slider",
                   shiny::sliderInput("id", label = NULL, min = 0, max = 1,
                                      value = 0.5)))))
})
