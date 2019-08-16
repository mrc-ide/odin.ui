mod_model_static_ui <- function(id, docs = NULL) {
  ns <- shiny::NS(id)

  editor <- shiny::tagList(
    odin_css(),
    shiny::includeCSS(odin_ui_file("css/styles-editor.css")),
    shiny::titlePanel(shiny::textOutput(ns("title"))),
    ## The ace editor setting "showPrintMargin" is the one to control
    ## the 80 char bar but I don't see how to get that through here.
    ## https://github.com/ajaxorg/ace/wiki/Configuring-Ace
    shinyAce::aceEditor(ns("editor"), mode = "r", readOnly = TRUE),
    shiny::div(class = "pull-right",
               shiny::downloadButton(
                 ns("download_button"), "Save", class = "btn-blue")),
    shiny::tags$div(style = "clear:both;"))

  if (!is.null(docs)) {
    docs <- shiny::withMathJax(shiny::includeMarkdown(docs))
  }

  status <- shiny::tagList(
    docs,
    model_static_info(),
    shiny::uiOutput(ns("status")))

  shiny::fluidRow(
    shiny::column(6, editor),
    shiny::column(6, status))
}


mod_model_static_server <- function(input, output, session, code,
                                    name = NULL, name_short = NULL,
                                    parameter_ranges = NULL, order = NULL) {
  rv <- shiny::reactiveValues()

  data <- model_static_setup(code, name, name_short,
                             parameter_ranges = parameter_ranges)
  data$model$is_static <- TRUE
  order <- variable_order_result(order = order %||% data$model$info$vars$name)
  output$title <- shiny::renderText(data$model$name)

  shiny::observe({
    shinyAce::updateAceEditor(session, session$ns("editor"), data$model$code)
  })

  shiny::observe({
    rv$result <- editor_result(data$model, order)
  })

  shiny::observe({
    rv$status <- editor_status(rv$result, NULL)
  })

  output$download_button <- shiny::downloadHandler(
    filename = "odin.R", # TODO: customisable?
    content = function(con) {
      writeLines(input$editor, con)
    })

  list(result = shiny::reactive(add_status(rv$result, rv$status)))
}


model_static_setup <- function(code, name, name_short,
                               parameter_ranges = NULL) {
  if (length(code) == 1 && file.exists(code)) {
    code <- readLines(code)
  }
  validation <- common_odin_validate(editor_validate_initial_code(code))
  model <- common_odin_compile(validation, name, name_short)
  stopifnot(model$success)

  if (!is.null(parameter_ranges)) {
    i <- match(model$info$pars$name, names(parameter_ranges))
    model$info$pars$range <- I(parameter_ranges[i])
  }

  list(validation = validation, model = model)
}


model_static_docs <- function(path_docs) {
  path_docs %||% odin_ui_file("md/model_static.md")
}


model_static_info <- function() {
  simple_panel("info", "Fixed model", "This model is not editable")
}
