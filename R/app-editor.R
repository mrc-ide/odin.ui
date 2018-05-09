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
##' @param ... Additional paramters passed to \code{shiny::runApp}
##' @export
odin_ui_editor_app <- function(initial_code, ...) {
  app <- shiny::shinyApp(
    ui = odin_ui_editor_ui(initial_code), server = odin_ui_editor_server)
  shiny::runApp(app, ...)
}


odin_ui_editor_ui <- function(initial_code) {
  if (is.null(initial_code)) {
    initial_code <- read_text(odin_ui_file("minimal_model.R"))
  } else {
    initial_code <- paste(initial_code, collapse = "\n")
  }

  ## The ace editor setting "showPrintMargin" is the one to control
  ## the 80 char bar but I don't see how to get that through here.
  ## https://github.com/ajaxorg/ace/wiki/Configuring-Ace

  shiny::shinyUI(
    shiny::fluidPage(
      shiny::tabsetPanel(
        id = "models",
        shiny::tabPanel(
          "Build",
          shiny::textInput("title", "Model name", "model"),
          shinyAce::aceEditor("editor", mode = "r", value = initial_code,
                              debounce = 10),
          shiny::actionButton("go_button", "Compile",
                              shiny::icon("cogs"),
                              class = "btn-primary"),
          shiny::htmlOutput("status"),
          shiny::verbatimTextOutput("messages"),
          shiny::verbatimTextOutput("input_code"),
          shiny::verbatimTextOutput("compiler_output")))))
}


odin_ui_editor_server <- function(input, output, session) {
  models <- shiny::reactiveValues(data = list())

  shiny::observeEvent(
    input$go_button,  {
      code <- input$editor
      title <- input$title
      model_id <- gsub(" -", "_", tolower(title))

      res <- withProgress(
        message = "Compiling model...",
        detail = "some detail", value = 1, {
          compile_model(code, tempfile(), skip_cache = TRUE)
        })

      msg <- sprintf("%s, %.2f s elapsed",
                     if (res$success) "Success" else "Error",
                     res$elapsed[["elapsed"]])
      cls <- if (res$success) "bg-success" else "bg-danger"
      output$status <- shiny::renderUI(shiny::p(class = cls, msg))

      if (res$success) {
        output$compiler_output <- shiny::renderText(NULL)
        output$messages <- shiny::renderText(NULL)

        new_tab <- !(title %in% names(models$data))

        models$data[[title]] <- res$model

        if (new_tab) {
          shiny::appendTab(
            "models",
            shiny::tabPanel(title, odin_ui_model_ui(model_id, title)))
        }

        default_time <- 10
        shiny::callModule(odin_ui_model, model_id, models$data[[title]],
                          default_time)
        shiny::updateTabsetPanel(session, "models", model_id)
      } else {
        output$messages <- shiny::renderText(paste0(res$error))
        output$compiler_output <-
          shiny::renderText(paste0(res$output, collapse = "\n"))
      }
    })
}
