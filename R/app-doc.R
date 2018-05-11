##' Display model with documentation
##'
##' @title Display model with documentation
##' @param path_config Path to a yml file with configuration
##' @inheritParams odin_ui_app
##' @export
odin_ui_doc_app <- function(path_config, ..., run = TRUE) {
  config <- mod_doc_config(path_config)
  app <- shiny::shinyApp(
    ui = odin_ui_doc_ui(config),
    server = odin_ui_doc_server(config))
  run_app(app, run, ...)
}


odin_ui_doc_ui <- function(config) {
  shiny::shinyUI(
    shiny::fluidPage(
      mod_doc_ui("odin_ui_doc", config)))
}


odin_ui_doc_server <- function(config) {
  function(input, output, session) {
    shiny::callModule(mod_doc_server, "odin_ui_doc", config)
  }
}
