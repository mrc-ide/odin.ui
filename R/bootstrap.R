simple_panel <- function(class, title, body, icon_name = NULL) {
  icon_name <- icon_name %||% switch(
    class,
    danger = "exclamation-circle",
    success = "check-circle",
    "info-circle")
  icon <- shiny::icon(sprintf("%s fa-lg", icon_name))
  head <- shiny::div(class = "panel-heading", icon, title)
  if (!is.null(body) && !identical(body, "")) {
    body <- shiny::div(class = "panel-body", body)
  } else {
    body <- NULL
  }
  shiny::div(
    class = "panel-group",
    shiny::div(
      class = sprintf("panel panel-%s", class), head, body))
}


unordered_list <- function(els) {
  if (length(els) == 0L) {
    return(NULL)
  }
  shiny::tags$ul(lapply(els, shiny::tags$li))
}


simple_numeric_input <- function(name, ...) {
  horizontal_form_group(shiny::span(name), raw_numeric_input(...))
}


simple_select_input <- function(name, ...) {
  horizontal_form_group(shiny::span(name), raw_select_input(...))
}


button_row <- function(label, ..., label_width = 6) {
  horizontal_form_group(label, shiny::actionButton(...))
}
