model_static_ui <- function(id, code, path_docs = NULL, title = "Code") {
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
                 ns("download_button"), "Save", class = "btn-blue")))

  shiny::fluidRow(
    shiny::column(6, editor),
    shiny::column(6, shiny::includeMarkdown(model_static_docs(path_docs))))
}


model_static_server <- function(input, output, session, code,
                                name = NULL, name_short = NULL,
                                parameter_ranges = NULL) {
  data <- model_static_setup(code, name, name_short,
                             parameter_ranges = parameter_ranges)

  output$title <- shiny::renderText(data$result$name)

  shiny::observe({
    shinyAce::updateAceEditor(session, session$ns("editor"), data$result$code)
  })

  output$download_button <- shiny::downloadHandler(
    filename = "odin.R", # TODO: customisable?
    content = function(con) {
      writeLines(input$editor, con)
    })

  list(result = shiny::reactive(add_status(data$result, data$status)))
}


model_static_setup <- function(code, name, name_short,
                               show = NULL, include = NULL,
                               parameter_ranges = NULL) {
  if (length(code) == 1 && file.exists(code)) {
    code <- readLines(code)
  }
  validation <- common_odin_validate(editor_validate_initial_code(code))
  model <- common_odin_compile(validation, name, name_short)
  stopifnot(model$success)

  show <- show %||% model$info$vars$name
  include <- include %||% model$info$vars$name
  result <- editor_result(model, show, include)

  if (!is.null(parameter_ranges)) {
    i <- match(result$info$pars$name, names(parameter_ranges))
    result$info$pars$range <- I(parameter_ranges[i])
  }

  status <- editor_status(result, NULL)

  list(validation = validation, result = result, status = status)
}


model_static_docs <- function(path_docs) {
  path_docs %||% odin_ui_file("md/model_static.md")
}
