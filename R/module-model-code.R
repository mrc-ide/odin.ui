mod_model_code_ui <- function(id, title = "Model code") {
  ns <- shiny::NS(id)
  target <- ns("container")
  icon <- shiny::icon(sprintf("%s fa-lg", "file-alt"))
  head <- shiny::div(class = "panel-heading", icon, title,
                     "data-toggle" = "collapse",
                     "data-target" = paste0("#", target))
  body <- shiny::div(class = "panel-body collapse", id = target,
                     shiny::uiOutput(ns("code")))
  shiny::div(
    class = "row",
    shiny::div(
      class = "col-md-10 no-padding panel-group",
      shiny::div(
        class = "panel panel-info", head, body)))
}


mod_model_code_server <- function(input, output, session, model) {
  output$code <- shiny::renderUI({
    model_code_block(model()$code)
  })
}


model_code_block <- function(code) {
  if (!is_missing(code)) {
    shiny::tags$pre(code, class = "model-code")
  }
}
