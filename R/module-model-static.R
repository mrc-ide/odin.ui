mod_model_static_ui <- function(id, code, path_docs = NULL, title = "Code") {
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
    shiny::tags$div(style = "clear:both;"),
    mod_variable_order_ui(ns("order")),
    shiny::uiOutput(ns("status")))

  shiny::fluidRow(
    shiny::column(6, editor),
    shiny::column(6, shiny::includeMarkdown(model_static_docs(path_docs))))
}


mod_model_static_server <- function(input, output, session, code,
                                    name = NULL, name_short = NULL,
                                    parameter_ranges = NULL) {
  data <- model_static_setup(code, name, name_short,
                             parameter_ranges = parameter_ranges)
  rv <- shiny::reactiveValues()

  order <- shiny::callModule(
    mod_variable_order_server, "order", shiny::reactive(data$model$info$vars))
  modules <- submodules(order = order)

  output$title <- shiny::renderText(data$model$name)

  shiny::observe({
    shinyAce::updateAceEditor(session, session$ns("editor"), data$model$code)
  })

  shiny::observe({
    rv$result <- editor_result(data$model, order$result())
  })

  shiny::observe({
    rv$status <- editor_status(rv$result, NULL)
  })

  output$download_button <- shiny::downloadHandler(
    filename = "odin.R", # TODO: customisable?
    content = function(con) {
      writeLines(input$editor, con)
    })

  get_state <- function() {
    list(modules = modules$get_state())
  }

  set_state <- function(state) {
    modules$set_state(state$modules)
  }

  list(result = shiny::reactive(add_status(rv$result, rv$status)),
       get_state = get_state,
       set_state = set_state)
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
