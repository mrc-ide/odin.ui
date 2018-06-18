##' Construct a shiny app from an existing \code{odin} model.
##'
##' @title Interactive odin app
##'
##' @param model An odin model (a generator function) created by
##'   \code{odin::odin}.
##'
##' @param default_time The default time - this can be either a scalar
##'   number (in which case time is assumed to default to \code{[0,
##'   default_time]} and the starting time is not configurable) or a
##'   length-two vector in which case both the start and end times are
##'   configurable.
##'
##' @param title String to use for the application title
##'
##' @param parameters A list of parameter information
##'
##' @param ... Additional paramters passed to \code{shiny::runApp}
##'
##' @param run Logical, indicating if the app should be run.  If
##'   \code{false} then the app is returned rather than run, and all
##'   arguments in \code{...} are ignored.
##'
##' @export
##' @importFrom odin odin
odin_ui_app <- function(model, default_time, title = "odin ui",
                        parameters = NULL, ..., run = TRUE) {
  app <- shiny::shinyApp(
    ui = odin_ui_app_ui(title),
    server = odin_ui_app_server(model, default_time, parameters))
  run_app(app, run, ...)
}


odin_ui_app_ui <- function(title) {
  shiny::shinyUI(
    shiny::fluidPage(
      mod_model_ui("odin_ui", title)))
}


odin_ui_app_server <- function(model, default_time, parameters) {
  function(input, output, session) {
    shiny::callModule(mod_model_server, "odin_ui",
                      model, default_time, parameters)
  }
}
