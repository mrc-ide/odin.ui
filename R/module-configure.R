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
    shiny::textOutput(ns("model_summary")),
    shiny::h3("Link"),
    shiny::uiOutput(ns("link")),
    shiny::textOutput(ns("link_status")))
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
      shiny::updateSelectInput(session, "data_time_variable",
                               choices = vars,
                               selected = selected(prev, vars))
    }
    msg
  })

  rv <- shiny::reactiveValues(link = NULL)

  ## Doing parameter tuning here will be really hard to do while
  ## preserving changes across model versions.  Not sure what do to
  ## really.  Perhaps we should just enforce this all at the level of
  ## the model compiler?
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

  output$link <- shiny::renderUI({
    d <- data()
    m <- model()
    time <- input$data_time_variable
    if (is.null(d) || is.null(m) || !nzchar(time)) {
      rv$map <- NULL
    } else {
      vars_data <- setdiff(names(d), time)
      ## TODO: push this into the editor module
      metadata <- model_metadata(m$result$model)
      vars_model <- c(
        names(metadata$data$variable$contents),
        names(metadata$data$output$contents))

      ns <- session$ns
      fmt <- "link_data_%s"
      rv$map <- setNames(sprintf(fmt, vars_data), vars_data)
      opts <- list(
        placeholder = "Select variable",
        onInitialize = I('function() { this.setValue(""); }'))
      lapply(vars_data, function(x)
        shiny::selectizeInput(
          ns(sprintf(fmt, x)), x, choices = vars_model, options = opts))
    }
  })

  shiny::observe({
    if (is.null(rv$map)) {
      rv$link <- NULL
    } else {
      link <- lapply(rv$map, function(x) input[[x]])
      rv$link <- link[vlapply(link, function(x) !is.null(x) && nzchar(x))]
    }
  })

  output$link_status <- shiny::renderText({
    if (length(rv$link) == 0L) {
      "No linked variables"
    } else {
      paste(sprintf("%s ~ %s", names(rv$link), vcapply(rv$link, identity)),
            collapse = " & ")
    }
  })

  return(shiny::reactive(rv$link))
}


selected <- function(prev, choices) {
  if (!is.null(prev) && prev %in% choices) prev else NA
}
