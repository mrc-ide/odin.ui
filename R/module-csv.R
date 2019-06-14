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
        shiny::selectInput(ns("name_time"),
                           "Select time variable",
                           character(0)),
        shiny::uiOutput(ns("summary")),
        shiny::hr(),
        shiny::actionButton(ns("clear"), "Clear")),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("data_plot")),
        shiny::dataTableOutput(ns("data_table")))))
}


mod_csv_server <- function(input, output, session, csv_status_body) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$configured <- csv_configure(rv$imported, input$name_time)
  })

  shiny::observe({
    rv$status <- csv_status(rv$configured, csv_status_body)
  })

  output$summary <- shiny::renderUI({
    csv_summary(rv$imported, rv$configured)
  })

  output$data_plot <- plotly::renderPlotly({
    csv_plot(rv$configured)
  })

  output$data_table <- shiny::renderDataTable(
    rv$imported$data,
    options = list(paging = FALSE, dom = "t", searching = FALSE))

  shiny::observe({
    if (!is.null(input$filename)) {
      ## NOTE: the isolate here breaks a cyclic dependency and allows
      ## the "clear" to work
      shiny::isolate({
        rv$imported <- csv_process(input$filename$datapath, input$filename$name)
        if (rv$imported$success) {
          shiny::updateSelectInput(session, "name_time",
                                   choices = rv$imported$vars,
                                   selected = rv$imported$guess)
        }
      })
    }
  })

  shiny::observeEvent(
    input$clear, {
      shinyjs::reset("filename")
      rv$configured <- NULL
      rv$imported <- NULL
      shiny::updateSelectInput(session, "name_time", choices = character(0))
    })

  get_state <- function() {
    list(imported = rv$imported, configured = rv$configured)
  }

  set_state <- function(state) {
    ## TODO: can't yet set the filename in the upload widget
    rv$imported <- state$imported
    rv$configured <- state$configured
    shiny::updateSelectInput(session, "name_time",
                             choices = state$imported$vars,
                             selected = state$configured$name_time)
  }

  list(result = shiny::reactive(c(rv$configured, list(status = rv$status))),
       get_state = get_state,
       set_state = set_state)
}


csv_process <- function(path, filename, min_cols = 2, min_rows = 10) {
  result <- csv_validate(path, min_cols, min_rows)
  result$filename <- filename

  if (result$success) {
    vars <- names(result$data)

    name_times <- c("t", "time", "day", "date", "week", "year")
    i <- which(tolower(vars) %in% name_times)
    if (length(i) == 1L) {
      guess <- vars[[i]]
    } else {
      guess <- NA
    }

    result$vars <- vars
    result$guess <- guess
  }

  result
}


csv_validate <- function(filename, min_cols, min_rows) {
  data <- tryCatch(read_csv(filename), error = identity)
  if (inherits(data, "error")) {
    return(list(success = FALSE, data = NULL, error = data$message))
  }

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
    data <- NULL
  }

  list(success = success, data = data, error = error)
}


csv_configure <- function(data, name_time) {
  if (identical(name_time, "") || !any(data$vars == name_time)) {
    name_time <- NULL
  }
  result <- list(data = data$data,
                 name_time = name_time,
                 configured = !is.null(data$data) && !is.null(name_time))
  if (result$configured) {
    result$name_vars <- setdiff(data$vars, name_time)
    result$cols <- odin_colours_data(result$name_vars)
  }
  result
}


csv_summary <- function(imported, configured) {
  if (!is.null(imported$error)) {
    class <- "danger"
    head <- "Error loading csv"
    body <- unordered_list(imported$error)
  } else if (is.null(imported)) {
    class <- "info"
    head <- "Upload a data set to begin"
    body <- NULL
  } else {
    head <- sprintf("Uploaded %d rows and %d columns",
                    nrow(configured$data), ncol(configured$data))
    if (isTRUE(configured$configured)) {
      body <- sprintf("Response variables: %s",
                      paste(configured$name_vars, collapse = ", "))
      class <- "success"
    } else {
      body <- "Select a time variable to view plot"
      class <- "info"
    }
  }
  simple_panel(class, head, body)
}


csv_status <- function(data, body = NULL) {
  if (isTRUE(data$configured)) {
    ok <- TRUE
    class <- "success"
    title <- sprintf("%d rows of data have been uploaded", nrow(data$data))
    body <- NULL
  } else {
    ok <- FALSE
    class <- "danger"
    if (is.null(data$data)) {
      title <- "Data not present"
    } else {
      title <- "Please select time variable for your data"
    }
  }
  module_status(class, title, body)
}


csv_plot <- function(result) {
  if (!isTRUE(result$configured)) {
    return(NULL)
  }
  series <- plot_plotly_series_bulk(
    result$data[[result$name_time]],
    result$data[result$name_vars],
    col = result$cols, points = TRUE, y2 = FALSE)
  plot_plotly(series)
}
