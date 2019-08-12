odin_sidebar <- function(buttons, controls, status = NULL) {
  shiny::div(
    class = "col-sm-4 col-lg-3",
    shiny::div(class = "odin-sidebar-buttons", buttons),
    shiny::tags$form(
      class = "form-horizontal",
      shiny::div(class = "list-group odin-status", status),
      shiny::div(class = "list-group odin-options", controls)))
}
