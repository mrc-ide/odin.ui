##' Run a model over varying parameters
##' @title Run a model over varying parameters
##' @inheritParams odin_ui_app
##' @export
odin_ui_parameter_app <- function(model, default_time, title = "odin ui",
                                  parameters = NULL, extra = NULL, ...,
                                  run = TRUE) {
  app <- shiny::shinyApp(
    ui = odin_ui_parameter_app_ui(title),
    server = odin_ui_parameter_app_server(model, default_time, parameters,
                                          extra))
  run_app(app, run, ...)
}


odin_ui_parameter_app_ui <- function(title) {
  shiny::shinyUI(
    shiny::fluidPage(
      mod_parameter_input("odin_parameter", title)))
}


odin_ui_parameter_app_server <- function(model, default_time, parameters,
                                         extra) {
  function(input, output, session) {
    shiny::callModule(mod_parameter_server, "odin_parameter",
                      model, default_time, parameters, extra)
  }
}
