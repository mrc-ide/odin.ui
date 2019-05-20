mod_csv_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Upload data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::p(paste(
          "Upload data for the epidemic; this file should include",
          "a column for time and one or more response variables.",
          "The data must have a header row, and all columns must contain",
          "only numbers. Missing data is allowed.")),
        shiny::fileInput(
          ns("filename"), NULL,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),
        shiny::textOutput(ns("summary")),
        shiny::actionButton(ns("clear"), "Clear")),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("data_plot")),
        shiny::dataTableOutput(ns("data_table")))))
}


mod_csv_server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL)

  shiny::observeEvent(
    input$clear, {
      shinyjs::reset("filename")
      rv$data <- NULL
    })

  shiny::observe({
    if (!is.null(input$filename)) {
      rv$data <- validate_csv(input$filename$datapath)
    }
  })

  output$summary <- shiny::renderText({
    if (is.null(rv$data)) {
      NULL
    } else {
      if (rv$data$success) {
        data <- rv$data$data
        sprintf("Uploaded %d x %d observations", nrow(data), ncol(data))
      } else {
        paste("Error:", rv$data$error)
      }
    }
  })

  output$data_plot <- plotly::renderPlotly({
    if (!is.null(rv$data$data)) {
      data <- rv$data$data
      cols <- odin_ui_palettes("odin")(ncol(data) - 1L)
      plot_data(data, cols)
    }
  })

  output$data_table <- shiny::renderDataTable({
    if (!is.null(rv$data$data)) {
      rv$data$data
    }
  }, options = list(paging = FALSE, dom = "t", searching = FALSE))

  shiny::reactive(rv$data$data)
}


## TODO: The validation rules here should get more flexible (minimum
## number of rows and columns for example)
validate_csv <- function(filename) {
  data <- tryCatch(read_csv(filename), error = identity)
  if (inherits(data, "error")) {
    list(success = FALSE, data = NULL, error = data$message)
  } else if (any(duplicated(names(data)))) {
    list(success = FALSE, data = NULL,
         error = "Data contains duplicate names")
  } else if (ncol(data) < 2) {
    list(success = FALSE, data = NULL,
         error = "Expected at least two columns")
  } else if (nrow(data) < 10) {
    list(success = FALSE, data = NULL,
         error = "Expected at least 10 rows")
  } else if (!all(vapply(data, is.numeric, logical(1)))) {
    list(success = FALSE, data = NULL,
         error = "All columns must be numeric")
  } else {
    list(success = TRUE, data = data, error = NULL)
  }
}
