mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Configure"),
    shiny::h3("Data"),
    shiny::textOutput(ns("data_status")),
    shiny::selectInput(ns("data_time_variable"),
                       "Select time variable",
                       character(0)))
}


mod_configure_server <- function(input, output, session, data, model) {
  output$data_status <- shiny::renderText({
    if (is.null(data())) {
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
    if (!is.null(res)) {
      pars <- coef(res$generator)
      msg <- sprintf("Model with %d parameters", ncol(pars))
      output$model_status <- shiny::renderText(msg)
    }
  })
}
