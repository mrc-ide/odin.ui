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
    shiny::actionButton(ns("go_button"), "Compile",
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

    shiny::htmlOutput(ns("validation_info")),
    shiny::htmlOutput(ns("compilation_info")))

  shiny::fluidRow(
    shiny::column(6, editor),
    shiny::column(6, docs))
}


mod_editor_simple_server <- function(input, output, session, initial_code) {
  ns <- session$ns
  data <- shiny::reactiveValues(validation = NULL,
                                compilation = NULL)

  initial_code <- editor_validate_initial_code(initial_code)

  shiny::observeEvent(
    input$reset_button, {
      shinyAce::updateAceEditor(session, ns("editor"), value = initial_code)
      data$compilation <- NULL
      data$validation <- NULL
    })

  output$download_button <- shiny::downloadHandler(
    filename = "odin.R", # TODO: customisable?
    content = function(con) {
      writeLines(input$editor, con)
    })

  shiny::observeEvent(
    input$uploaded_file, {
      if (!is.null(input$uploaded_file)) {
        code <- editor_read_code(input$uploaded_file$datapath)
        shinyAce::updateAceEditor(session, ns("editor"), value = code)
        data$validation <- editor_validate(code)
      }
    })

  ## Manual validation
  shiny::observeEvent(
    input$validate_button, {
      data$validation <- editor_validate(input$editor)
    })

  ## Realtime validation
  shiny::observe({
    if (!is.null(data$compilation)) {
      data$compilation$is_current <-
        identical(data$compilation$code, input$editor)
    }
    if (input$auto_validate) {
      data$validation <- editor_validate(input$editor)
    }
  })

  output$validation_info <- shiny::renderUI({
    editor_validation_info(data$validation)
  })

  output$compilation_info <- shiny::renderUI({
    editor_compilation_info(data$compilation)
  })

  shiny::observe({
    shinyAce::updateAceEditor(session, ns("editor"),
                              border = editor_border(data$validation))
  })

  ## Here is the first part of the exit route out of the module
  shiny::observeEvent(
    input$go_button, {
      res <- shiny::withProgress(
        message = "Compiling...", value = 1, editor_compile(input$editor))
      data$validation <- res$validation
      data$compilation <- res$compilation
    })

  get_state <- function() {
    shiny::isolate({
      ## TODO: for completeness, it might be worth getting the model
      ## filename and the auto_validate state too, but for now we don't.
      list(code = input$editor,
           compiled = if (data$compilation$success) data$compilation$code)
    })
  }

  set_state <- function(state) {
    if (!is.null(state$compiled)) {
      res <- editor_compile(state$compiled)
      data$validation <- res$validation
      data$compilation <- res$compilation
    }
    shinyAce::updateAceEditor(session, ns("editor"), value = state$code)
  }

  return(list(result = shiny::reactive(data$compilation),
              get_state = get_state,
              set_state = set_state))
}


editor_validation_info <- function(status) {
  if (is.null(status)) {
    panel <- NULL
  } else {
    if (!is.null(status$error)) {
      body <- shiny::pre(status$error$message)
      result <- "error"
      class <- "danger"
    } else if (length(status$messages) > 0L) {
      body <- shiny::pre(paste(vcapply(status$messages, "[[", "message"),
                               collapse = "\n\n"))
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


editor_compilation_info <- function(status) {
  if (is.null(status)) {
    panel <- NULL
  } else {
    success <- isTRUE(status$result$success)
    result <- sprintf("%s, %.2f s elapsed",
                      if (success) "success" else "error",
                      status$result$elapsed[["elapsed"]])
    is_current <- status$is_current
    if (!is_current) {
      result <- paste(result, "(code has changed since this was run)")
    }
    if (success) {
      class <- if (is_current) "success" else "default"
      icon_name <- "check-circle"
      ## TODO: this should be hideable, and hidden by default
      body <- shiny::pre(paste(status$result$output, collapse = "\n"))
    } else {
      class <- if (is_current) "danger" else "warning"
      icon_name <- "times-circle"
      body <- shiny::pre(status$result$error)
    }

    title <- sprintf("Compilation: %s", result)
    panel <- simple_panel(class, title, body, icon_name)
  }

  panel
}


editor_border <- function(status) {
  if (!is.null(status$error)) {
    "alert"
  } else {
    "normal"
  }
}


editor_read_code <- function(path) {
  paste0(readLines(path), "\n", collapse = "")
}


editor_validate <- function(code) {
  res <- odin::odin_validate(code, "text")
  if (!is.null(res$error)) {
    res$error <- res$error$message
  }
  res$messages <- vcapply(res$messages, function(x) x$message)
  res
}


editor_compile <- function(code) {
  validation <- editor_validate(code)
  if (validation$success) {
    result <- odin::odin_build(validation$result)
    result$info <- model_info(result$model)
  } else {
    result <- NULL
  }
  compilation <- list(code = code, result = result, is_current = TRUE)
  list(validation = validation, compilation = compilation)
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
