mod_status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"), inline = TRUE)
}


mod_status_server <- function(input, output, session, status) {
  icons <- names(status)
  output$ui <- shiny::renderUI({
    class <- vcapply(status, function(x) text_module_status(x()$status))
    Map(function(name, class) shiny::icon(name, class = class),
        icons, class)
  })
}
