mod_editor_ui <- function(id, initial_code) {
  ns <- shiny::NS(id)

  initial_code <- mod_editor_validate_initial_code(initial_code)
  docs <- shiny::includeMarkdown(odin_ui_file("md/editor.md"))
  path_editor_css <- odin_ui_file("css/styles-editor.css")

  editor <- shiny::tagList(
    odin_css(),
    shiny::includeCSS(path_editor_css),
    shiny::fluidRow(
      shiny::column(6,
          shiny::div(class="form-group",
              shiny::tags$label("Model name"), raw_text_input(ns("title"), "model", style="width: 200px"))),
      shiny::column(6,
          file_input(ns("uploaded_file"),
                    "Upload model file",
                    multiple = FALSE,
                    accept = c("text/plain", ".R"),
                    button_class = "btn-grey")
      )
    ),
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

    shiny::div(class="pull-right", shiny::downloadButton(ns("download_button"), "Save", class="btn-blue")),

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


mod_editor_server <- function(input, output, session, initial_code) {
  ns <- session$ns
  data <- shiny::reactiveValues(model = NULL,
                                validation = NULL,
                                compilation = NULL)

  initial_code <- mod_editor_validate_initial_code(initial_code)

  shiny::observeEvent(
    input$reset_button, {
      shinyAce::updateAceEditor(session, ns("editor"), value = initial_code)
      data$compilation <- NULL
      data$validation <- NULL
    })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      mod_editor_title_to_filename(input$title)
    },
    content = function(con) {
      writeLines(input$editor, con)
    })

  shiny::observeEvent(
    input$uploaded_file, {
      if (!is.null(input$uploaded_file)) {
        code <- read_text(input$uploaded_file$datapath)
        title <- mod_editor_filename_to_title(input$uploaded_file$name)

        ## TODO: This probably needs considerable santisation!
        shinyAce::updateAceEditor(session, ns("editor"), value = code)
        shiny::updateTextInput(session, "title", value = title)
      }
    })

  ## Manual validation
  shiny::observeEvent(
    input$validate_button, {
      data$validation <- odin::odin_validate_model(input$editor, "text")
    })

  ## Realtime validation
  shiny::observe({
    if (isTRUE(data$compilation$is_current) &&
        !identical(data$compilation$code, input$editor)) {
      data$compilation$is_current <- FALSE
    }
    if (input$auto_validate) {
      data$validation <- odin::odin_validate_model(input$editor, "text")
    }
  })

  shiny::observe({
    res <- mod_editor_validation_info(data$validation)
    output$validation_info <- res$panel
    shinyAce::updateAceEditor(session, ns("editor"), border = res$border)
  })

  shiny::observe({
    res <- mod_editor_compilation_info(data$compilation)
    output$compilation_info <- res$panel
    shinyAce::updateAceEditor(session, ns("editor"), border = res$border)
  })

  ## Here is the first parr of the exit route out of the module
  shiny::observeEvent(
    input$go_button, {
      code <- input$editor
      res <- shiny::withProgress(
        message = "Compiling model...",
        detail = "some detail", value = 1, {
          compile_model(code, tempfile(), skip_cache = TRUE)
        })

      data$compilation <- list(code = code, result = res, is_current = TRUE)

      if (res$success) {
        pars <- coef(res$model)
        shiny::showModal(editor_metadata_modal(pars, NULL, TRUE, ns))
      }
    })

  ## This is the second part of the exit route out
  shiny::observeEvent(input$editor_metadata_ok, {
    model <- data$compilation$result$model
    pars <- coef(model)
    res <- editor_metadata_validate(pars, input)
    if (res$success) {
      shiny::removeModal()
      title <- input$title
      model_id <- gsub(" -", "_", tolower(title))
      ## This is needed to avoid a bit of zealous checking about
      ## contents of the parameter list used in model.R
      parameters <- lapply(
        res$data[names(res$data) != "t"], function(el)
          el[intersect(names(el), c("description", "default", "range"))])
      data$model <- list(generator = model,
                         title = title,
                         default_time = res$data$t$range,
                         id = model_id,
                         parameters = parameters)
    } else {
      shiny::showModal(editor_metadata_modal(pars, res$data, FALSE, ns))
    }
  })

  return(shiny::reactive(data$model))
}


## Support functions:
mod_editor_validate_initial_code <- function(initial_code) {
  if (is.null(initial_code)) {
    initial_code <- readLines(odin_ui_file("editor_default.R"))
  } else if (!is.character(initial_code)) {
    stop("'initial_code' must be a character vector", call. = FALSE)
  }
  initial_code <- paste(initial_code, collapse = "\n")
  if (!grepl("\\n$", initial_code)) {
    initial_code <- paste0(initial_code, "\n")
  }
  initial_code
}


mod_editor_title_to_filename <- function(name) {
  paste0(gsub(" ", "_", name), ".R")
}


mod_editor_filename_to_title <- function(filename) {
  gsub("[_-]", " ", sub("\\.R$", "", filename))
}


## The functions below here (a third of the file!) just do the error
## reporting.  It feels like something that could be done more
## efficiently!
mod_editor_validation_info <- function(status) {
  if (is.null(status)) {
    panel <- shiny::renderUI(NULL)
    border <- "normal"
  } else {
    if (!is.null(status$error$message)) {
      info <- status$error$message
      result <- "error"
    } else if (length(status$messages) > 0L) {
      info <- paste(vcapply(status$messages, "[[", "message"),
                    collapse = "\n\n")
      result <- "note"
    } else {
      info <- ""
      result <- "success"
    }

    success <- result != "error"
    border <- if (success) "normal" else "alert"

    if (result == "success") {
      class <- "success"
      icon <- "check-circle"
    } else if (result == "note") {
      class <- "info"
      icon <- "info-circle"
    } else {
      class <- "danger"
      icon <- "times-circle"
    }
    panel <- mod_editor_status_panel("Validation", result, info, class, icon)
  }

  list(panel = panel, border = border)
}


mod_editor_compilation_info <- function(data) {
  if (is.null(data)) {
    panel <- shiny::renderUI(NULL)
    border <- "normal"
  } else {
    x <- data$result
    is_current <- data$is_current

    success <- x$success
    border <- if (success) "normal" else "alert"

    result <- sprintf("%s, %.2f s elapsed",
                      if (x$success) "success" else "error",
                      x$elapsed[["elapsed"]])
    if (!is_current) {
      result <- paste(result, "(code has changed since this was run)")
    }
    if (success) {
      class <- if (is_current) "success" else "default"
      icon <- "check-circle"
      info <- paste(x$output, collapse = "\n")
    } else {
      class <- if (is_current) "danger" else "warning"
      icon <- "times-circle"
      info <- x$error
    }
    panel <- mod_editor_status_panel("Compilation", result, info, class, icon)
  }

  list(panel = panel, border = border)
}


mod_editor_status_panel <- function(header, result, info, class, icon) {
  if (nzchar(info)) {
    body <- shiny::div(class = "panel-body", shiny::pre(info))
  } else {
    body <- NULL
  }
  shiny::renderUI(
    shiny::div(
      class = "panel-group",
      shiny::div(
        class = sprintf("panel panel-%s", class),
        shiny::div(
          class = "panel-heading",
          shiny::icon(paste(icon, "fa-lg")),
          sprintf("%s: %s", header, result)),
        body)))
}
