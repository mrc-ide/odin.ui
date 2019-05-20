## A simpler editor than the main one
mod_editor_simple_ui <- function(id, initial_code, path_docs) {
  ns <- shiny::NS(id)

  initial_code <- mod_editor_validate_initial_code(initial_code)
  docs <- shiny::includeMarkdown(path_docs %||% odin_ui_file("md/editor.md"))
  path_editor_css <- odin_ui_file("css/styles-editor.css")

  editor <- shiny::tagList(
    odin_css(),
    shiny::includeCSS(path_editor_css),
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
    filename = "odin.R", # TODO: customisable?
    content = function(con) {
      writeLines(input$editor, con)
    })

  shiny::observeEvent(
    input$uploaded_file, {
      if (!is.null(input$uploaded_file)) {
        code <- read_text(input$uploaded_file$datapath)
        ## TODO: This probably needs considerable santisation!
        shinyAce::updateAceEditor(session, ns("editor"), value = code)
        data$validation <- odin::odin_validate(code, "text")
      }
    })

  ## Manual validation
  shiny::observeEvent(
    input$validate_button, {
      data$validation <- odin::odin_validate(input$editor, "text")
    })

  ## Realtime validation
  shiny::observe({
    if (isTRUE(data$compilation$is_current) &&
        !identical(data$compilation$code, input$editor)) {
      data$compilation$is_current <- FALSE
    }
    if (input$auto_validate) {
      data$validation <- odin::odin_validate(input$editor, "text")
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

  ## Here is the first part of the exit route out of the module
  shiny::observeEvent(
    input$go_button, {
      code <- input$editor
      res <- shiny::withProgress(
        message = "Compiling model...",
        detail = "some detail", value = 1, {
          data$validation <- odin::odin_validate(input$editor, "text")
          odin::odin_build(data$validation$result)
        })

      data$compilation <- list(code = code, result = res, is_current = TRUE)
      data$result <- list(generator = res$model, ir = res$ir)
    })

  return(shiny::reactive(data$result))
}
