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
      input$name, input$pct, input$n, user())
  })

  output$status <- shiny::renderText({
    control_focal_status(rv$focal)
  })

  get_state <- function() {
    list(name = input$name,
         pct = input$pct,
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

  pct <- restore$pct %||% 10
  n <- restore$n %||% 10
  name <- restore$name %||% pars[[1]]

  mod_model_control_section(
    "Vary parameter",
    horizontal_form_group(
      "Parameter to vary",
      raw_select_input(
        ns("name"), pars, selected = name)),
    simple_numeric_input("Variation (%)", ns("pct"), pct),
    simple_numeric_input("Number of runs", ns("n"), n),
    shiny::textOutput(ns("status")),
    ns = ns)
}


control_focal_result <- function(name, pct, n, user) {
  if (is_missing(pct) || is_missing(name) || is_missing(n)) {
    return(NULL)
  }
  value <- user[[name]]
  if (is_missing(value)) {
    return(NULL)
  }
  dy <- abs(pct / 100 * value)
  from <- value - dy
  to <- value + dy
  list(base = user, name = name, value = value, n = n, from = from, to = to)
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
