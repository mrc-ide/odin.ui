simple_panel <- function(class, title, body) {
  if (class == "danger") {
    icon_name <- "exclamation-circle"
  } else {
    icon_name <- "info-circle"
  }
  icon <- shiny::icon(sprintf("%s fa-lg", icon_name))
  shiny::div(
    class = "panel-group",
    shiny::div(
      class = sprintf("panel panel-%s", class),
      shiny::div(class = "panel-heading", icon, title),
      shiny::div(class = "panel-body", body)))
}
