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
  rv <- shiny::reactiveValues(data = NULL)

  shiny::observeEvent(
    input$clear, {
      shinyjs::reset("filename")
      shiny::updateSelectInput(session, "name_time", choices = character(0))
      rv$data <- NULL
    })

  shiny::observe({
    if (!is.null(input$filename)) {
      shiny::isolate({
        rv$data <- csv_process(input$filename$datapath)
        if (rv$data$success) {
          shiny::updateSelectInput(session, "name_time",
                                   choices = rv$data$vars,
                                   selected = rv$data$guess)
        }
      })
    }
  })

  shiny::observe({
    rv$data <- csv_configure(rv$data, input$name_time)
  })

  shiny::observe({
    rv$status <- csv_status(rv$data, csv_status_body)
  })

  output$summary <- shiny::renderUI({
    csv_summary(rv$data)
  })

  output$data_plot <- plotly::renderPlotly({
    if (isTRUE(rv$data$configured)) {
      cols <- odin_colours_data(rv$data$name_vars)
      plot_data(rv$data$data, rv$data$name_time, rv$data$name_vars, cols)
    }
  })

  output$data_table <- shiny::renderDataTable({
    if (!is.null(rv$data$data)) {
      rv$data$data
    }
  }, options = list(paging = FALSE, dom = "t", searching = FALSE))

  get_state <- function() {
    list(filename = input$filename$name,
         data = rv$data)
  }

  set_state <- function(state) {
    ## TODO: can't yet set the filename in the upload widget
    rv$data <- state$data
    shiny::updateSelectInput(session, "name_time",
                             choices = state$data$vars,
                             selected = state$data$name_time)
  }

  list(result = shiny::reactive(c(rv$data, list(status = rv$status))),
       status = shiny::reactive(csv_status(rv$data, csv_status_body)),
       get_state = get_state,
       set_state = set_state)
}


csv_process <- function(filename, min_cols = 2, min_rows = 10) {
  result <- csv_validate(filename, min_cols, min_rows)

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
  if (is.null(name_time) || !nzchar(name_time)) {
    name_time <- NULL
  }
  data$name_time <- name_time
  data$name_vars <- setdiff(data$vars, name_time)
  data$configured <- !is.null(data$data) && !is.null(name_time)
  data
}


csv_summary <- function(data) {
  if (is.null(data$success)) {
    NULL
  } else if (data$success) {
    head <- sprintf("Uploaded %d rows and %d columns",
                    nrow(data$data), ncol(data$data))
    if (isTRUE(data$configured)) {
      body <- sprintf("Response variables: %s",
                      paste(data$name_vars, collapse = ", "))
      class <- "success"
    } else {
      body <- "Select a time variable to view plot"
      class <- "info"
    }
    simple_panel(class, head, body)
  } else {
    body <- shiny::tags$ul(lapply(data$error, shiny::tags$li))
    simple_panel("danger", "Error loading csv", body)
  }
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
