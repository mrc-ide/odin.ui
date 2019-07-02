mod_control_run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


## This will need some work for a *pair* of models?
mod_control_run_server <- function(input, output, session, model, options) {
  rv <- shiny::reactiveValues()
  options <- control_run_options_validate(options)

  shiny::observe({
    rv$configuration <- control_run_configuration(model(), options)
  })

  output$ui <- shiny::renderUI({
    control_run_ui(rv$configuration, session$ns)
  })

  shiny::observe({
    rv$values <- control_run_result(
      options, get_inputs(input, rv$configuration$inputs))
  })

  get_state <- function() {
    get_inputs(input, rv$configuration$inputs)
  }

  set_state <- function(state) {
    set_inputs(session, names(state), state)
  }

  list(
    result = shiny::reactive(rv$values),
    get_state = get_state,
    set_state = set_state)
}


control_run_configuration <- function(model, options) {
  if (!isTRUE(model$success)) {
    return(NULL)
  }
  i <- list_to_logical(options)
  if (!any(i)) {
    return(NULL)
  }

  inputs <- c(control_end_time = "end", use_replicates = "replicates")[i]

  list(options = options, inputs = inputs)
}


control_run_ui <- function(configuration, ns) {
  if (is.null(configuration)) {
    return(NULL)
  }

  end <- replicates <- NULL
  if (configuration$options$control_end_time) {
    end <- simple_numeric_input("End time", ns("end"), NA)
  }
  if (configuration$options$replicates) {
    replicates <- simple_numeric_input("Replicates", ns("replicates"), NA)
  }
  tags <- drop_null(list(end, replicates))
  mod_model_control_section("Run options", tags, ns = ns)
}


control_run_result <- function(options, values) {
  list(options = options, values = values)
}


control_run_options <- function(control_end_time = FALSE, replicates = FALSE) {
  list(control_end_time = control_end_time, replicates = replicates)
}


control_run_options_validate <- function(options) {
  control_run_options(
    control_end_time = options$control_end_time %||% FALSE,
    replicates = options$replicates %||% FALSE)
}
