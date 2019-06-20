## A simpler editor than the main one
mod_editor_simple_ui <- function(id, initial_code, path_docs) {
  ns <- shiny::NS(id)

  initial_code <- editor_validate_initial_code(initial_code)
  docs <- shiny::includeMarkdown(path_docs %||% odin_ui_file("md/editor.md"))
  path_editor_css <- odin_ui_file("css/styles-editor.css")

  editor <- shiny::tagList(
    odin_css(),
    shiny::includeCSS(path_editor_css),
    shiny::titlePanel("Editor"),
    shiny::fluidRow(
      shiny::column(6),
      shiny::column(6,
          file_input(ns("uploaded_file"),
                    "Upload model file",
                    multiple = FALSE,
                    accept = c("text/plain", ".R"),
                    button_class = "btn-grey")
      )),
    ## The ace editor setting "showPrintMargin" is the one to control
    ## the 80 char bar but I don't see how to get that through here.
    ## https://github.com/ajaxorg/ace/wiki/Configuring-Ace
    shinyAce::aceEditor(ns("editor"), mode = "r", value = initial_code,
                        debounce = 100),
    shiny::actionButton(ns("compile"), "Compile",
                        shiny::icon("cogs"),
                        class = "btn-blue"),
    shiny::actionButton(ns("reset_button"), "Reset",
                        shiny::icon("refresh"),
                        class = "btn-grey"),

    shiny::div(class = "pull-right",
               shiny::downloadButton(
                 ns("download_button"), "Save", class = "btn-blue")),

    ## And these should go elsewhere too
    shiny::actionButton(ns("validate_button"), "Validate",
                        shiny::icon("check"), class = "btn-success"),
    shiny::checkboxInput(ns("auto_validate"), "Auto validate",
                         value = FALSE),

    ## TODO: this disables _all_ progress - ideally we'd do this
    ## just for this id, which is going to be a slightly more
    ## clever css rule.
    shiny::tags$style(".shiny-file-input-progress {display: none}"),

    shiny::uiOutput(ns("validation_info")),
    shiny::uiOutput(ns("result_info")))

  shiny::fluidRow(
    shiny::column(6, editor),
    shiny::column(6, docs))
}


mod_editor_simple_server <- function(input, output, session, initial_code,
                                     editor_status_body) {
  ns <- session$ns
  rv <- shiny::reactiveValues()

  initial_code <- editor_validate_initial_code(initial_code)

  output$validation_info <- shiny::renderUI({
    editor_validation_info(rv$validation)
  })

  output$result_info <- shiny::renderUI({
    editor_result_info(rv$result)
  })

  shiny::observe({
    rv$status <- editor_status(rv$result, editor_status_body)
  })

  shiny::observe({
    shinyAce::updateAceEditor(session, ns("editor"),
                              border = editor_border(rv$validation))
  })

  shiny::observeEvent(
    input$uploaded_file, {
      if (!is.null(input$uploaded_file)) {
        code <- editor_read_code(input$uploaded_file$datapath)
        shinyAce::updateAceEditor(session, ns("editor"), value = code)
        rv$validation <- common_odin_validate(code)
      }
    })

  ## Manual validation
  shiny::observeEvent(
    input$validate_button, {
      rv$validation <- common_odin_validate(input$editor)
    })

  ## Realtime validation
  shiny::observe({
    if (!is.null(rv$result)) {
      rv$result$is_current <- identical(rv$result$code, input$editor)
    }
    if (input$auto_validate) {
      rv$validation <- common_odin_validate(input$editor)
    }
  })

  ## Here is the first part of the exit route out of the module
  shiny::observeEvent(
    input$compile, {
      rv$validation <- common_odin_validate(input$editor)
      rv$result <- shiny::withProgress(
        message = "Compiling...", value = 1,
        common_odin_compile(rv$validation))
    })

  shiny::observeEvent(
    input$reset_button, {
      shinyAce::updateAceEditor(session, ns("editor"), value = initial_code)
      rv$result <- NULL
      rv$validation <- NULL
    })

  output$download_button <- shiny::downloadHandler(
    filename = "odin.R", # TODO: customisable?
    content = function(con) {
      writeLines(input$editor, con)
    })

  get_state <- function() {
    list(editor = input$editor,
         result = if (!is.null(rv$result$model)) rv$result$code)
  }

  set_state <- function(state) {
    if (!is.null(state$result)) {
      rv$result <- common_odin_compile(common_odin_validate(state$result))
    }
    shinyAce::updateAceEditor(session, ns("editor"), value = state$editor)
    rv$validation <- common_odin_validate(state$editor)
  }

  list(result = shiny::reactive(add_status(rv$result, rv$status)),
       get_state = get_state,
       set_state = set_state)
}


editor_validation_info <- function(status) {
  if (is.null(status)) {
    panel <- NULL
  } else {
    if (!is.null(status$error)) {
      body <- shiny::pre(status$error)
      result <- "error"
      class <- "danger"
    } else if (length(status$messages) > 0L) {
      body <- shiny::pre(paste(status$messages, collapse = "\n\n"))
      result <- "note"
      class <- "info"
    } else {
      body <- NULL
      result <- "success"
      class <- "success"
    }
    title <- sprintf("Validation: %s", result)
    panel <- simple_panel(class, title, body)
  }

  panel
}


editor_result_info <- function(result) {
  if (is.null(result)) {
    panel <- NULL
  } else {
    success <- isTRUE(result$success)
    title <- sprintf("%s, %.2f s elapsed",
                     if (success) "success" else "error", result$elapsed)
    is_current <- result$is_current
    if (!is_current) {
      title <- paste(title, "(code has changed since this was run)")
    }
    if (success) {
      class <- if (is_current) "success" else "default"
      icon_name <- "check-circle"
      ## TODO: this should be hideable, and hidden by default
      ## TODO: only do this if it's nonempty
      if (is_missing(result$output)) {
        body <- NULL
      } else {
        body <- shiny::pre(paste(result$output, collapse = "\n"))
      }
    } else {
      class <- if (is_current) "danger" else "warning"
      icon_name <- "times-circle"
      body <- shiny::pre(result$error)
    }

    title <- sprintf("Compilation: %s", title)
    panel <- simple_panel(class, title, body, icon_name)
  }

  panel
}


editor_border <- function(validation) {
  if (!is.null(validation$error)) {
    "alert"
  } else {
    "normal"
  }
}


editor_read_code <- function(path) {
  paste0(readLines(path, warn = FALSE), "\n", collapse = "")
}


editor_validate_initial_code <- function(initial_code) {
  if (is.null(initial_code)) {
    initial_code <- readLines(odin_ui_file("editor_default.R"))
  } else if (!is.character(initial_code)) {
    stop("'initial_code' must be a character vector", call. = FALSE)
  }
  initial_code <- paste(initial_code, collapse = "\n")
  if (nzchar(initial_code) && !grepl("\\n$", initial_code)) {
    initial_code <- paste0(initial_code, "\n")
  }
  initial_code
}


editor_status <- function(model, body) {
  if (is.null(model)) {
    class <- "danger"
    title <- "Please compile a model"
  } else {
    np <- nrow(model$result$info$pars)
    nv <- nrow(model$result$info$vars)
    title <- sprintf("Model with %d parameters and %d variables/outputs",
                     np, nv)
    if (model$is_current) {
      class <- "success"
      body <- NULL
    } else {
      class <- "warning"
      msg <- "Warning: model is out of date, consider recompiling the model."
      if (is.null(body)) {
        body <- msg
      } else {
        body <- shiny::tagList(msg, body)
      }
    }
  }
  module_status(class, title, body)
}
