## These are not real apps but things that are for driving modules
## individually.
testing_csv_app <- function() {
  ui <- shiny::shinyUI(
    shiny::navbarPage(
      "csv app",
      id = "odin_ui_navbar",
      shiny::tabPanel(
        "Data",
        icon = shiny::icon("table"),
        mod_csv_ui("odin_csv")),
      shiny::tabPanel(
        shiny::tagList(
          "Load/Save",
          mod_status_ui("status")),
        icon = shiny::icon("list"),
        mod_state_ui("state"))))
  shiny::shinyApp(
    ui = ui,
    server = function(input, output, session) {
      data <- shiny::callModule(mod_csv_server, "odin_csv", NULL)
      modules <- submodules(data = data)
      state <- shiny::callModule(
        mod_state_server, "state", modules, "prototype")
      status <- shiny::callModule(
        mod_status_server, "status", list(table = data$result))
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
