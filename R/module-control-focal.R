MOD_CONTROL_FOCAL_DEFAULT_PCT <- 10
MOD_CONTROL_FOCAL_DEFAULT_N <- 10
MOD_CONTROL_FOCAL_MAX_N <- 20

mod_control_focal_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_control_focal_server <- function(input, output, session, pars, user) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$configuration <- control_focal_configuration(pars())
  })

  output$ui <- shiny::renderUI({
    control_focal_ui(rv$configuration, session$ns)
  })

  shiny::observe({
    rv$result <- with_success(control_focal_result(
      input$name, input$scale, input$type,
      input$pct, input$from, input$to, input$n, user()))
  })

  output$status <- shiny::renderUI({
    control_focal_status(rv$result)
  })

  shiny::outputOptions(output, "ui", suspendWhenHidden = FALSE)

  get_state <- function() {
    input_ids <- c("name", "scale", "type", "pct", "from", "to", "n")
    list(configuration = rv$configuration,
         state = get_inputs(input, input_ids))
  }

  set_state <- function(state) {
    rv$configuration <- state$configuration
    shiny::updateSelectInput(session, "name", selected = state$state$name)
    shiny::updateSelectInput(session, "scale", selected = state$state$scale)
    shiny::updateSelectInput(session, "type", selected = state$state$type)
    restore_inputs(session, state$state[c("pct", "from", "to", "n")])
  }

  reset <- function() {
    output$ui <- shiny::renderUI(
      control_focal_ui(rv$configuration, session$ns))
  }

  list(result = shiny::reactive(rv$result$value),
       recompute = function(user) control_focal_recompute(get_state(), user),
       reset = reset,
       get_state = get_state,
       set_state = set_state)
}


control_focal_configuration <- function(pars) {
  if (is.null(pars)) {
    return(NULL)
  }
  list(pars = pars$name)
}


control_focal_ui <- function(configuration, ns) {
  pars <- configuration$pars

  if (length(pars) == 0) {
    return(NULL)
  }

  n <- MOD_CONTROL_FOCAL_DEFAULT_N
  name <- pars[[1]]
  type <- NULL
  scale <- NULL
  pct <- MOD_CONTROL_FOCAL_DEFAULT_PCT
  from <- NA
  to <- NA

  odin_control_section(
    "Vary parameter",
    simple_select_input(
      "Parameter to vary", ns("name"), pars, selected = name),
    simple_select_input(
      "Scale type", ns("scale"), c("Arithmetic", "Logarithmic"), scale),
    simple_select_input(
      "Variation type", ns("type"), c("Percentage", "Range"), type),
    simple_numeric_input("Variation (%)", ns("pct"), pct),
    simple_numeric_input("From", ns("from"), from),
    simple_numeric_input("To", ns("to"), to),
    simple_numeric_input("Number of runs", ns("n"), n),

    shiny::uiOutput(ns("status")),
    ns = ns)
}


control_focal_ui_focal <- function(type, pct, value, ns) {
  if (is_missing(type)) {
    return(NULL)
  }
  if (type == "Percentage") {
    pct <- MOD_CONTROL_FOCAL_DEFAULT_PCT
    simple_numeric_input("Variation (%)", ns("pct"), pct)
  } else {
    r <- control_focal_pct_to_range(value, pct)
    from <- r$from
    to <- r$to
    shiny::tagList(
      simple_numeric_input("From", ns("from"), from),
      simple_numeric_input("To", ns("to"), to))
  }
}


control_focal_result <- function(name, scale, type, pct, from, to, n, user) {
  if (is_missing(name)) {
    stop("Please select a valid parameter")
  }
  value <- user[[name]]
  if (is_missing(value)) {
    stop(sprintf("Enter a valid value for the parameter %s", name))
  }

  if (is_missing(n)) {
    stop("Number of runs must be given")
  } else if (n < 2) {
    stop("At least 2 runs are needed")
  } else if (n > MOD_CONTROL_FOCAL_MAX_N) {
    stop(sprintf("At most %d runs are possible", MOD_CONTROL_FOCAL_MAX_N))
  }

  if (is_missing(scale)) {
    stop("Please select a valid value for the scale type")
  }
  logarithmic <- scale == "Logarithmic"

  if (is_missing(type)) {
    stop("Please select a valid value for the variation type")
  }
  if (type == "Percentage") {
    if (is_missing(pct)) {
      stop("'Variation %' is missing")
    }
    r <- control_focal_pct_to_range(value, pct)
    from <- r$from
    to <- r$to
  } else {
    if (is_missing(from)) {
      stop("'From' is missing")
    }
    if (is_missing(to)) {
      stop("'To' is missing")
    }
    pct <- control_focal_range_to_pct(value, from, to)
  }

  if (isTRUE(logarithmic)) {
    values <- seq_log(from, to, length.out = n)
  } else {
    values <- seq(from, to, length.out = n)
  }

  list(base = user, name = name, value = value, n = n, pct = pct,
       from = from, to = to, logarithmic = scale == "Logarithmic",
       values = values)
}


control_focal_status <- function(result) {
  if (!is.null(result)) {
    if (isTRUE(result$success)) {
      values <- format(result$value$values, digits = 4)
      if (length(values) > 4) {
        values <- c(values[1:3], "...", values[[length(values)]])
      }
      simple_panel("success", paste(values, collapse = ", "), NULL)
    } else {
      simple_panel("danger", result$error, NULL)
    }
  }
}


control_focal_recompute <- function(focal, user) {
  control_focal_result(focal$name, focal$pct, focal$n, user)
}


control_focal_pct_to_range <- function(value, pct) {
  if (is_missing(value) || is_missing(pct)) {
    list(from = NA, to = NA)
  } else {
    dy <- abs(pct / 100 * value)
    list(from = value - dy, to = value + dy)
  }
}


control_focal_range_to_pct <- function(value, from, to) {
  if (is_missing(value) || is_missing(from) || is_missing(to)) {
    MOD_CONTROL_FOCAL_DEFAULT_PCT
  } else {
    round((to - from) / value / 2 * 100)
  }
}
