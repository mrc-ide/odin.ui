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
##' @param ... Additional arguments passed through to \code{shiny::runApp}
##'
##' @export
##' @importFrom odin odin
odin_ui_app <- function(model, default_time, title = "odin ui", ...) {
  ## TODO: this needs a better name - this is pretty poor
  app <- shiny::shinyApp(
    ui = odin_ui_app_ui(title),
    server = odin_ui_app_server(model, default_time))
  shiny::runApp(app, ...)
}


odin_ui_app_ui <- function(title) {
  path_css <- system.file("styles.css", package = "odin.ui", mustWork = TRUE)
  shiny::shinyUI(
    shiny::fluidPage(
      odin_ui_model_ui("odin_ui", title)))
}


odin_ui_app_server <- function(model, default_time) {
  function(input, output, session) {
    shiny::callModule(odin_ui_model, "odin_ui", model, default_time)
  }
}
