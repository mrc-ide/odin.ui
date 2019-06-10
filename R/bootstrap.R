simple_panel <- function(class, title, body) {
  icon_name <- switch(
    class,
    danger = "exclamation-circle",
    success = "check-circle",
    "info-circle")
  icon <- shiny::icon(sprintf("%s fa-lg", icon_name))
  head <- shiny::div(class = "panel-heading", icon, title)
  if (!is.null(body) && nzchar(body)) {
    body <- shiny::div(class = "panel-body", body)
  } else {
    body <- NULL
  }
  shiny::div(
    class = "panel-group",
    shiny::div(
      class = sprintf("panel panel-%s", class), head, body))
}
