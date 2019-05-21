mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Configure"),
    shiny::h3("Data"),
    shiny::textOutput(ns("data_summary")),
    shiny::selectInput(ns("data_time_variable"),
                       "Select time variable",
                       character(0)),
    shiny::h3("Model"),
    shiny::uiOutput(ns("model_status")),
    shiny::textOutput(ns("model_summary")))
}


mod_configure_server <- function(input, output, session, data, model) {
  output$data_summary <- shiny::renderText({
    if (is.null(data())) {
      ## Ideally this would point us back to the data tab with a link
      ## but that requires passing in the parent session:
      ## https://stackoverflow.com/a/54751068
      msg <- "Please upload data"
    } else {
      msg <- sprintf("%d rows of data have been uploaded", nrow(data()))
      vars <- names(data())
      prev <- input$data_time_variable
      selected <- if (!is.null(prev) && prev %in% vars) prev else NULL
      shiny::updateSelectInput(session, "data_time_variable",
                               choices = vars, selected = selected)
    }
    msg
  })

  shiny::observe({
    res <- model()
    status <- NULL

    if (is.null(res)) {
      msg <- "Please compile a model"
    } else {
      if (!res$is_current) {
        status <- shiny::div(
          class = "panel-group",
          shiny::div(
            class = "panel panel-warning",
            shiny::div(class = "panel-heading",
                       shiny::icon("exclamation fa-lg"),
                       "Warning: model is out of date"),
            shiny::div(class = "panel-body",
                       "Consider recompiling the model")))
      }

      pars <- coef(res$result$model)
      msg <- sprintf("Model with %d parameters", ncol(pars))
    }
    output$model_status <- shiny::renderUI(status)
    output$model_summary <- shiny::renderText(msg)
  })
}
