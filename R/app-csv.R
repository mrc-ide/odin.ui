odin_ui_csv_app <- function() {
  shiny::shinyApp(ui = odin_ui_csv_app_ui(),
                  server = odin_ui_csv_app_server())
}


odin_ui_csv_app_ui <- function() {
  shiny::shinyUI(
    shiny::fluidPage(
      mod_csv_ui("odin_csv")))
}


odin_ui_csv_app_server <- function() {
  function(input, output, session) {
    shiny::callModule(mod_csv_server, "odin_csv")
  }
}
