devtools::load_all()

read_csv <- function(filename) {
  read.csv(filename, stringsAsFactors = FALSE, check.names = FALSE)
}


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


csv_ui <- function() {
  shiny::shinyUI(
    shiny::fluidPage(
      shiny::titlePanel("Upload data"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::p(paste(
            "Upload data for the epidemic; this file should include",
            "a column for time and one or more response variables.",
            "The data must have a header row, and all columns must contain",
            "only numbers. Missing data is allowed.")),
          shiny::fileInput(
            "filename", NULL,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
          shiny::textOutput("summary")),
        shiny::mainPanel(
          plotly::plotlyOutput("data_plot"),
          shiny::dataTableOutput("data_table")))))
}


csv_server <- function() {
  function(input, output, session) {
    user_data <- shiny::reactive({
      if (!is.null(input$filename)) {
        validate_csv(input$filename$datapath)
      }
    })

    output$summary <- shiny::renderText({
      if (is.null(user_data())) {
        NULL
      } else {
        if (user_data()$success) {
          data <- user_data()$data
          sprintf("Uploaded %d x %d observations", nrow(data), ncol(data))
        } else {
          paste("Error:", user_data()$error)
        }
      }
    })

    output$data_plot <- plotly::renderPlotly({
      if (!is.null(user_data()$data)) {
        data <- user_data()$data
        cols <- odin_ui_palettes("odin")(ncol(data) - 1L)
        plot_data(data, cols)
      }
    })

    output$data_table <- shiny::renderDataTable({
      if (!is.null(user_data()$data)) {
        data <- user_data()$data
      }
    }, options = list(paging = FALSE, dom = "t", searching = FALSE))
  }
}

shiny::shinyApp(csv_ui(), csv_server())
