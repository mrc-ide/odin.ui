mod_help_ui <- function(id, title = "Help", class = NULL) {
  ns <- shiny::NS(id)
  shiny::div(
    class = class,
    shiny::actionButton(ns("button"), title, class = "btn-warning",
                        icon = shiny::icon("question-circle")))
}


mod_help_server <- function(input, output, session, path) {
  help <- help_modal(path)
  shiny::observeEvent(input$button, shiny::showModal(help))
}


help_modal <- function(path) {
  shiny::modalDialog(shiny::includeMarkdown(path), title = NULL,
                     size = "l", easyClose = TRUE, fade = FALSE)
}
