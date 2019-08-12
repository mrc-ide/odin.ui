MOD_CONTROL_FOCAL_DEFAULT_PCT <- 10

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
    rv$result <- control_focal_result(
      input$name, input$scale, input$type,
      input$pct, input$from, input$to, input$n, user())
  })

  output$status <- shiny::renderText({
    control_focal_status(rv$focal)
  })

  output$focal <- shiny::renderUI({
    result <- shiny::isolate(rv$result)
    control_focal_ui_focal(input$type, result, session$ns)
  })

  get_state <- function() {
    list(name = input$name,
         type = input$type,
         scale = input$scale,
         pct = input$pct,
         from = input$from,
         to = input$to,
         n = input$n)
  }

  set_state <- function(state) {
    output$ui <- shiny::renderUI(
      control_focal_ui(rv$configuration, session$ns, state))
  }

  reset <- function() {
    output$ui <- shiny::renderUI(
      control_focal_ui(rv$configuration, session$ns))
  }

  list(result = shiny::reactive(rv$result),
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


control_focal_ui <- function(configuration, ns, restore = NULL) {
  pars <- configuration$pars

  if (length(pars) == 0) {
    return(NULL)
  }

  n <- restore$n %||% MOD_CONTROL_FOCAL_DEFAULT_PCT
  name <- restore$name %||% pars[[1]]

  mod_model_control_section(
    "Vary parameter",
    simple_select_input(
      "Parameter to vary", ns("name"), pars, selected = name),
    simple_select_input(
      "Scale type", ns("scale"), c("Arithmetic", "Logarithmic")),
    simple_select_input(
      "Variation type", ns("type"), c("Percentage", "Range")),
    shiny::uiOutput(ns("focal")),
    simple_numeric_input("Number of runs", ns("n"), n),

    shiny::textOutput(ns("status")),
    ns = ns)
}


control_focal_ui_focal <- function(type, result, ns, restore = NULL) {
  if (is_missing(type)) {
    return(NULL)
  }
  if (type == "Percentage") {
    if (is.null(restore)) {
      pct <- control_focal_range_to_pct(result$value, result$from, result$to)
    } else {
      pct <- restore$pct %||% MOD_CONTROL_FOCAL_DEFAULT_PCT
    }
    simple_numeric_input("Variation (%)", ns("pct"), pct)
  } else {
    if (is.null(restore)) {
      r <- control_focal_pct_to_range(result$value, result$pct)
      from <- r$from
      to <- r$to
    } else {
      from <- restore$from %||% NA
      to <- restore$to %||% NA
    }
    shiny::tagList(
      simple_numeric_input("From", ns("from"), from),
      simple_numeric_input("To", ns("to"), to))
  }
}


control_focal_result <- function(name, scale, type, pct, from, to, n, user) {
  if (is_missing(pct) || is_missing(name) || is_missing(n)) {
    return(NULL)
  }
  value <- user[[name]]
  if (is_missing(value)) {
    return(NULL)
  }
  if (type == "Percentage") {
    r <- control_focal_pct_to_range(value, pct)
    from <- r$from
    to <- r$to
  } else {
    pct <- control_focal_range_to_pct(value, from, to)
  }
  list(base = user, name = name, value = value, n = n, pct = pct,
       from = from, to = to, logarithmic = scale == "Logarithmic")
}


control_focal_status <- function(focal) {
  if (!is.null(focal)) {
    sprintf("%s - %s - %s",
            focal$from, focal$value, focal$to)
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
