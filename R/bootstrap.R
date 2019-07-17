simple_panel <- function(class, title, body, icon_name = NULL) {
  icon <- panel_icon(icon_name, class)
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


panel_collapseable <- function(class, title, body, icon_name = NULL,
                               collapsed = FALSE, id = NULL) {
  id <- id %||% ids::random_id()
  icon <- panel_icon(icon_name, class)

  head <- shiny::div(class = "panel-heading", icon, title,
                     "data-toggle" = "collapse",
                     "data-target" = paste0("#", id))
  body_class <- paste0("panel-body", if (collapsed) " collapse" else "")
  body <- shiny::div(class = body_class, id = id, body)

  shiny::div(
    class = "panel-group",
    shiny::div(
      class = sprintf("panel panel-%s", class), head, body))
}


panel_icon <- function(icon_name, class) {
  if (is.null(icon_name)) {
    icon_name <- switch(
      class,
      danger = "exclamation-circle",
      success = "check-circle",
      "info-circle")
  }
  shiny::icon(sprintf("%s fa-lg", icon_name))
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


simple_slider_input <- function(name, id, value, range) {
  list(shiny::div(
    class = "form-group slider-label",
    shiny::div(class = "col-xs-12", shiny::tags$label(name))),
    shiny::div(
      class = "form-group",
      shiny::div(class = "col-xs-12 slider",
                 shiny::sliderInput(id,
                                    label = NULL,
                                    min = range$from,
                                    max = range$to,
                                    value = value))))
}


button_row <- function(label, ..., label_width = 6) {
  horizontal_form_group(label, shiny::actionButton(...))
}
