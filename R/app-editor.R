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
  shiny::shinyUI(
    shiny::navbarPage(
      "odin editor",
      id = "odin_ui_navbar",
      inverse = TRUE,
      shiny::tabPanel(
        "Write and build",
        icon = shiny::icon("edit"),
        mod_editor_ui("odin_editor", initial_code)),
      shiny::tabPanel(
        "Interact",
        icon = shiny::icon("search"),
        shiny::tabsetPanel(id = "models")),
      shiny::tabPanel(
        "Help",
        icon = shiny::icon("question"),
        shiny::includeMarkdown(odin_ui_file("md/editor.md")))))
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
              shiny::tabPanel(m$title, mod_model_ui(m$id, m$title)))
          }
          parameters <- NULL
          shiny::callModule(
            mod_model_server, m$id,
            models$data[[m$title]], m$default_time, parameters)
          shiny::updateNavbarPage(session, "odin_ui_navbar", "Interact")
          shiny::updateTabsetPanel(session, "models", m$title)
        }
      })
    })
  }
}
