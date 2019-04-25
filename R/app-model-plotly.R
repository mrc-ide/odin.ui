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
##' @param extra A function to apply over the model output, in order
##'   to produce additional derived columns.  This is designed to
##'   allow more flexible post-run transformations than can easily be
##'   allowed in odin and might be particularly useful in discrete
##'   time stochastic models that are replicated (e.g., to summarise
##'   across model realisations).  Must be provided as a named list of
##'   functions.  The name of the element is the name of the new
##'   variable in the output.
##'
##' @param ... Additional paramters passed to \code{shiny::runApp}
##'
##' @param run Logical, indicating if the app should be run.  If
##'   \code{false} then the app is returned rather than run, and all
##'   arguments in \code{...} are ignored.
##'
##' @export
##' @importFrom odin odin
odin_plotly_app <- function(model, default_time, title = "odin ui",
                            parameters = NULL, extra = NULL, ...,
                            run = TRUE) {
  app <- shiny::shinyApp(
    ui = odin_plotly_app_ui(title),
    server = odin_plotly_app_server(model, default_time, parameters, extra))
  run_app(app, run, ...)
}


odin_plotly_app_ui <- function(title) {
  shiny::shinyUI(
    shiny::tagList(
      odin_css(),
      shiny::fluidPage(
        mod_plotly_ui("odin_plotly_ui", title))))
}


odin_plotly_app_server <- function(model, default_time, parameters, extra) {
  output_control <- NULL
  default_replicates <- 1L
  time_scale <- NULL
  function(input, output, session) {
    shiny::callModule(mod_plotly_server, "odin_plotly_ui",
                      model, default_time, parameters, extra,
                      output_control, default_replicates, time_scale)
  }
}
