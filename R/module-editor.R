odin_ui_editor_ui <- function(id) {
  ns <- shiny::NS(id)

  initial_code <- read_text(odin_ui_file("dummy_model.R"))

  ## The ace editor setting "showPrintMargin" is the one to control
  ## the 80 char bar but I don't see how to get that through here.
  ## https://github.com/ajaxorg/ace/wiki/Configuring-Ace
  shiny::tagList(
    shinyAce::aceEditor(ns("editor"), mode = "r", value = initial_code,
                        debounce = 10),
    shiny::actionButton(ns("go_button"), "Compile",
                        shiny::icon("cogs"),
                        class = "btn-primary"),
    shiny::htmlOutput(ns("status")),
    shiny::verbatimTextOutput(ns("messages")),
    shiny::verbatimTextOutput(ns("input_code")),
    shiny::verbatimTextOutput(ns("compiler_output")))
}


odin_ui_editor <- function(input, output, session) {
  ns <- session$ns

  model <- shiny::reactiveValues(data = NULL)
  ret <- shiny::reactive(model$data)

  shiny::observeEvent(
    input$go_button,  {
      code <- input$editor

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
      } else {
        output$messages <- shiny::renderText(paste0(res$error))
        output$compiler_output <-
          shiny::renderText(paste0(res$output, collapse = "\n"))
      }

      model$data <- res$model
    })

  return(ret)
}


odin_editor_app <- function(...) {
  ui <- shiny::shinyUI(
    shiny::fluidPage(
      odin_ui_editor_ui("odin_editor")))
  server <- function(input, output, session) {
    shiny::callModule(odin_ui_editor, "odin_editor")
  }
  app <- shiny::shinyApp(
    ui = ui, server = server)
  shiny::runApp(app, ...)
}
