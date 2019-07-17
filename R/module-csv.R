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
        shiny::fileInput(ns("filename"), NULL, accept = accept_csv()),
        shiny::selectInput(ns("name_time"),
                           "Select time variable",
                           character(0)),
        shiny::uiOutput(ns("summary")),
        shiny::hr(),
        shiny::div(
          class = "text-right",
          shiny::actionButton(ns("clear"), "Clear", shiny::icon("times"),
                              class = "btn-danger"))),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("data_plot")),
        shiny::dataTableOutput(ns("data_table")))))
}


mod_csv_server <- function(input, output, session, csv_status_body) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$result <- csv_result(rv$imported$value, input$name_time)
  })

  shiny::observe({
    rv$status <- csv_status(rv$result, csv_status_body)
  })

  output$summary <- shiny::renderUI({
    csv_summary(rv$imported, rv$result)
  })

  output$data_plot <- plotly::renderPlotly({
    csv_plot(rv$result)
  })

  output$data_table <- shiny::renderDataTable(
    rv$imported$value$data,
    options = list(paging = FALSE, dom = "t", searching = FALSE))

  shiny::observe({
    if (!is.null(input$filename)) {
      ## NOTE: the isolate here breaks a cyclic dependency and allows
      ## the "clear" to work
      shiny::isolate({
        rv$imported <- csv_import(input$filename$datapath, input$filename$name,
                                  min_cols = 2, min_rows = 10)
        if (rv$imported$success) {
          update_select_input(session, "name_time", rv$imported$value$info)
        }
      })
    }
  })

  shiny::observeEvent(
    input$clear, {
      shinyjs::reset("filename")
      rv$result <- NULL
      rv$imported <- NULL
      update_select_input(session, "name_time", NULL)
    })

  get_state <- function() {
    list(imported = rv$imported, result = rv$result,
         name_time = list(choices = names(rv$result$data),
                          selected = rv$result$name_time))
  }

  set_state <- function(state) {
    ## TODO: can't yet set the filename in the upload widget
    rv$imported <- state$imported
    rv$result <- state$result
    update_select_input(session, "name_time", state$name_time)
  }

  list(result = shiny::reactive(add_status(rv$result, rv$status)),
       get_state = get_state,
       set_state = set_state)
}


csv_import <- function(path, filename, min_cols = 2, min_rows = 10) {
  result <- with_success(read_csv(path))
  if (!result$success) {
    return(result)
  }
  csv_validate(result$value, filename, min_cols, min_rows)
}


csv_validate <- function(data, filename, min_cols, min_rows) {
  error <- NULL
  if (any(duplicated(names(data)))) {
    dup <- unique(names(data)[duplicated(names(data))])
    error <- c(error, sprintf("Data contains duplicate names (%s)",
                              paste(squote(dup), collapse = ", ")))
  }
  is_numeric <- vapply(data, is.numeric, logical(1))
  if (!all(is_numeric)) {
    err <- paste(squote(names(data)[!is_numeric]), collapse = ", ")
    error <- c(error, sprintf("All columns must be numeric (%s)", err))
  }
  if (ncol(data) < min_cols) {
    error <- c(error, sprintf("Expected at least %d columns", min_cols))
  }
  if (nrow(data) < min_rows) {
    error <- c(error, sprintf("Expected at least %d rows", min_rows))
  }

  success <- length(error) == 0L
  if (!success) {
    csv_import_error(error)
  } else {
    csv_import_result(data, filename)
  }
}


## The summary depends on both the import and the configured; we'll
## lift errors out of one and results out of the other.
csv_summary <- function(imported, result) {
  if (is.null(imported$success)) {
    class <- "info"
    head <- "Upload a data set to begin"
    body <- NULL
  } else if (!imported$success) {
    class <- "danger"
    head <- "Error loading csv"
    body <- unordered_list(imported$error)
  } else {
    head <- sprintf("Uploaded %d rows and %d columns",
                    nrow(imported$value$data), ncol(imported$value$data))
    if (isTRUE(result$configured)) {
      body <- sprintf("Response variables: %s",
                      paste(result$name_vars, collapse = ", "))
      class <- "success"
    } else {
      body <- "Select a time variable to view plot"
      class <- "info"
    }
  }
  simple_panel(class, head, body)
}


csv_status <- function(result, body = NULL) {
  if (isTRUE(result$configured)) {
    ok <- TRUE
    class <- "success"
    title <- sprintf("%d rows of data have been uploaded", nrow(result$data))
    body <- NULL
  } else {
    ok <- FALSE
    class <- "danger"
    if (is.null(result$data)) {
      title <- "Data not present"
    } else {
      title <- "Please select time variable for your data"
    }
  }
  module_status(class, title, body)
}


csv_plot_series <- function(result) {
  if (!isTRUE(result$configured)) {
    return(NULL)
  }
  plot_plotly_series_bulk(
    result$data[[result$name_time]],
    result$data[result$name_vars],
    col = result$cols, points = TRUE, y2 = FALSE)
}


csv_plot <- function(result) {
  plot_plotly(csv_plot_series(result))
}


csv_import_error <- function(message) {
  list(success = FALSE, value = NULL, error = message)
}


csv_import_result <- function(data, filename) {
  value <- list(data = data,
                filename = filename,
                info = csv_guess_time(data))
  list(success = TRUE, value = value, error = NULL)
}


csv_guess_time <- function(data) {
  vars <- names(data)
  name_times <- c("t", "time", "day", "date", "week", "year")
  i <- which(tolower(vars) %in% name_times)
  if (length(i) == 1L) {
    guess <- vars[[i]]
  } else {
    guess <- NA
  }
  list(choices = vars, selected = guess)
}


csv_result <- function(value, name_time) {
  odin_data_source(value$data, value$filename, name_time)
}
