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
        shiny::textOutput(ns("summary")),
        shiny::hr(),
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
      shiny::updateSelectInput(session, "name_time", choices = character(0))
      rv$data <- NULL
    })

  shiny::observe({
    if (!is.null(input$filename)) {
      shiny::isolate({
        rv$data <- validate_csv(input$filename$datapath)
        if (rv$data$success) {
          vars <- names(rv$data$data)
          name_times <- c("t", "time", "day", "date", "week", "year")
          i <- which(tolower(vars) %in% name_times)
          if (length(i) == 1L) {
            selected <- vars[[i]]
          } else {
            selected <- NA
          }
          shiny::updateSelectInput(session, "name_time",
                                   choices = vars, selected = selected)
        }
      })
    }
  })

  ## NOTE: this triggers far too often, but I think it's a tolerable
  ## load.
  shiny::observe({
    name_time <- input$name_time
    if (is.null(name_time) || !nzchar(name_time)) {
      name_time <- NULL
    }
    rv$data$name_time <- name_time
    rv$data$name_vars <- setdiff(names(rv$data$data), name_time)
    rv$data$configured <- !is.null(rv$data$data) && !is.null(name_time)
  })

  output$summary <- shiny::renderText({
    if (is.null(rv$data$success)) {
      NULL
    } else {
      if (rv$data$success) {
        data <- rv$data$data
        txt1 <- sprintf("Uploaded %d rows and %d columns.",
                        nrow(data), ncol(data))
        if (isTRUE(rv$data$configured)) {
          txt2 <- sprintf("Response variables: %s",
                          paste(rv$data$name_vars, collapse = ", "))
        } else {
          txt2 <- "Select a time variable to view plot"
        }
        paste(txt1, txt2, sep = "\n\n")
      } else {
        paste("Error:", rv$data$error)
      }
    }
  })

  output$data_plot <- plotly::renderPlotly({
    if (isTRUE(rv$data$configured)) {
      cols <- odin_ui_palettes("odin")(length(rv$data$name_vars))
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
                             choices = names(state$data$data),
                             selected = state$data$name_time)
  }

  ## TODO: Can drop success and error here, but they don't hurt
  list(result = shiny::reactive(rv$data),
       get_state = get_state,
       set_state = set_state)
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
