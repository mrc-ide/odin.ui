##' Shiny app for interactively creating odin models
##'
##' This shiny app is subject to all the restrictions that the
##' \code{\link{odin_ui_app}} shiny app is subject to (no vector
##' inputs or outputs).
##'
##' @title Shiny app for interactively creating odin models
##'
##' @param initial_code Optional character vector of odin code to be
##'   used as the contents of the editor on startup.  If omitted (or
##'   \code{NULL}) then a very simple default model is used
##'   (exponential growth).
##'
##' @inheritParams odin_ui_app
##' @export
odin_ui_editor_app <- function(initial_code = NULL, ..., run = TRUE) {
  app <- shiny::shinyApp(
    ui = odin_ui_editor_ui(initial_code),
    server = odin_ui_editor_server(initial_code))
  run_app(app, run, ...)
}


odin_ui_editor_ui <- function(initial_code) {
  ## TODO: consider a navbar page as the build tab is "different" to
  ## the other types.
  shiny::shinyUI(
    shiny::fluidPage(
      shiny::tabsetPanel(
        id = "models",
        shiny::tabPanel(
          "Build",
          mod_editor_ui("odin_editor", initial_code)))))
}


odin_ui_editor_server <- function(initial_code) {
  function(input, output, session) {
    models <- shiny::reactiveValues(data = list())

    model <- shiny::callModule(
      mod_editor_server, "odin_editor", initial_code)

    shiny::observe({
      m <- model()
      shiny::isolate({
        if (!is.null(m)) {
          new_tab <- !(m$title %in% names(models$data))
          models$data[[m$title]] <- m$generator
          if (new_tab) {
            shiny::appendTab(
              "models",
              shiny::tabPanel(m$title, mod_model_ui(m$model_id, m$title)))
          }
          shiny::callModule(
            mod_model_server, m$model_id,
            models$data[[m$title]], m$default_time)
          shiny::updateTabsetPanel(session, "models", m$title)
        }
      })
    })
  }
}
