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
odin_ui_editor_app <- function(initial_code = NULL, ...) {
  initial_code <- validate_initial_code(initial_code)
  app <- shiny::shinyApp(
    ui = odin_ui_editor_ui(initial_code),
    server = odin_ui_editor_server(initial_code))
  shiny::runApp(app, ...)
}


odin_ui_editor_ui <- function(initial_code) {
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
          ## This should not run over all the grid columns and perhaps
          ## should display some helper text on the right panel.  That
          ## would be super useful for a teaching situation.
          shinyAce::aceEditor("editor", mode = "r", value = initial_code,
                              debounce = 100),
          shiny::actionButton("go_button", "Compile",
                              shiny::icon("cogs"),
                              class = "btn-primary"),
          shiny::actionButton("reset_button", "Reset",
                              shiny::icon("refresh"),
                              class = "btn-danger"),

          ## Ideally these would be aligned further right
          shiny::downloadButton("download_button", "Save"),
          shiny::fileInput("uploaded_file",
                           "Upload model file",
                           multiple = FALSE,
                           accept = c("text/plain", ".R")),

          ## And these should go elsewhere too
          shiny::actionButton("validate_button", "Validate",
                              shiny::icon("check"), class = "btn-success"),
          shiny::checkboxInput("auto_validate", "Auto validate", value = FALSE),

          ## TODO: this disables _all_ progress - ideally we'd do this
          ## just for this id, which is going to be a slightly more
          ## clever css rule.
          shiny::tags$style(".shiny-file-input-progress {display: none}"),

          shiny::verbatimTextOutput("validation_info"),

          shiny::htmlOutput("status"),
          shiny::verbatimTextOutput("messages"),
          shiny::verbatimTextOutput("input_code"),
          shiny::verbatimTextOutput("compiler_output")))))
}


## If I pull the editor code into a module, most of the bits here
## really come out into the controlling app becaues they are logic
## that needs to get involved with multiple model instances.
odin_ui_editor_server <- function(initial_code) {
  force(initial_code)

  function(input, output, session) {
    models <- shiny::reactiveValues(data = list(), errors = NULL)
    validation <- shiny::reactiveValues(status = NULL)

    shiny::observeEvent(
      input$reset_button, {
        ## NOTE: this does not reset the rest of the interface
        ## (e.g. tabs) or the rest of the *inputs* e.g. the model
        ## name.  These are probably worthwhile things to get done at
        ## some point.
        shinyAce::updateAceEditor(session, "editor", value = initial_code)
      })

    output$download_button <- shiny::downloadHandler(
      filename = function() {
        title_to_filename(input$title)
      },
      content = function(con) {
        writeLines(input$editor, con)
      })

    shiny::observeEvent(
      input$uploaded_file, {
        if (!is.null(input$uploaded_file)) {
          code <- read_text(input$uploaded_file$datapath)
          title <- filename_to_title(input$uploaded_file$name)

          ## TODO: This probably needs considerable santisation!
          shinyAce::updateAceEditor(session, "editor", value = code)
          shiny::updateTextInput(session, "title", value = title)
        }
      })

    shiny::observeEvent(
      input$validate_button, {
        validation$status <- odin::odin_validate_model(input$editor, "text")
      })

    shiny::observe({
      status <- validation$status

      if (is.null(status) || status$success) {
        border <- "normal"
      } else {
        border <- "alert"
      }
      shinyAce::updateAceEditor(session, "editor", border = border)
      if (!is.null(status$error$message)) {
        out <- status$error$message
      } else {
        out <- paste(vcapply(validation$status$messages, "[[", "message"),
                     collapse = "\n\n")
      }
      output$validation_info <- shiny::renderText(out)
    })

    shiny::observe({
      if (input$auto_validate) {
        validation$status <- odin::odin_validate_model(input$editor, "text")
      }
    })

    shiny::observeEvent(
      input$go_button, {
        code <- input$editor
        title <- input$title
        model_id <- gsub(" -", "_", tolower(title))

        res <- shiny::withProgress(
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
}


validate_initial_code <- function(initial_code) {
  if (is.null(initial_code)) {
    initial_code <- readLines(odin_ui_file("minimal_model.R"))
  } else if (!is.character(initial_code)) {
    stop("'initial_code' must be a character vector", call. = FALSE)
  }
  initial_code <- paste(initial_code, collapse = "\n")
  if (!grepl("\\n$", initial_code)) {
    initial_code <- paste0(initial_code, "\n")
  }
  initial_code
}


title_to_filename <- function(name) {
  paste0(gsub(" ", "_", name), ".R")
}


filename_to_title <- function(filename) {
  gsub("[_-]", " ", sub("\\.R$", "", filename))
}
