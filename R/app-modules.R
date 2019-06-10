## These are not real apps but things that are for driving modules
## individually.
odin_ui_csv_app <- function() {
  shiny::shinyApp(
    ui = shiny::shinyUI(
      shiny::fluidPage(
        mod_csv_ui("odin_csv"))),
    server = function(input, output, session) {
      shiny::callModule(mod_csv_server, "odin_csv")
    })
}


odin_ui_editor_simple <- function(initial_code = character(0),
                                  path_docs = NULL) {
  shiny::shinyApp(
    ui = shiny::shinyUI(
      shiny::fluidPage(
        mod_editor_simple_ui("editor", initial_code, path_docs))),
    server = function(input, output, session) {
      shiny::callModule(mod_editor_simple_server, "editor", initial_code)
    })
}
