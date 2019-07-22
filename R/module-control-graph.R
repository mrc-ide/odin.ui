mod_control_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


## I think that all we need here is the vars and their colours
mod_control_graph_server <- function(input, output, session, cfg) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$configuration <- control_graph_configuration(cfg())
  })

  output$ui <- shiny::renderUI({
    control_graph_ui(rv$configuration, session$ns)
  })

  shiny::observe({
    rv$values <- list(
      logscale = input$logscale,
      y2 = get_inputs(input, rv$configuration$id, rv$configuration$name))
  })

  get_state <- function() {
    c(list(logscale = input$logscale),
      get_inputs(input, rv$configuration$id))
  }

  set_state <- function(state) {
    set_inputs(session, names(state), state, shiny::updateCheckboxInput)
  }

  reset <- function() {
    output$ui <- shiny::renderUI(
      control_graph_ui(rv$configuration, session$ns))
  }

  list(
    result = shiny::reactive(rv$values),
    reset = reset,
    get_state = get_state,
    set_state = set_state)
}


## The configuration here is one put together by a previous module...
control_graph_configuration <- function(configuration) {
  if (is.null(configuration)) {
    return(NULL)
  }
  vars <- configuration$vars[configuration$vars$include, , drop = FALSE]
  if (nrow(vars) == 0L) {
    return(NULL)
  }

  list(name = vars$name,
       id = sprintf("id_%s", vars$name),
       col = configuration$cols$model[vars$name])
}


control_graph_ui <- function(configuration, ns, restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }

  labels <- Map(function(lab, col)
    shiny::span(lab, style = paste0("color:", col)),
    configuration$name, configuration$col)

  if (!is.null(restore)) {
    value_y2 <- restore$y2
    value_logscale <- restore$logscale_y
  } else {
    value_y2 <- FALSE
    value_logscale <- FALSE
  }

  tags <- shiny::div(
    class = "form-group",
    raw_checkbox_input(ns("logscale"), "Log scale y axis", value_logscale),
    shiny::tags$label("Plot on second y axis"),
    Map(raw_checkbox_input, ns(configuration$id), labels, value_y2))

  id <- ns("hide")
  title <- "Graph settings"

  ## TODO: style here should move into css
  head <- shiny::a(style = "text-align: right; display: block;",
                   "data-toggle" = "collapse", class = "text-muted",
                   href = paste0("#", id),
                   title, shiny::icon("gear", lib = "font-awesome"))
  body <- shiny::div(
    id = id, class = "collapse box", style = "width: 300px;",
    list(tags))

  shiny::div(class = "pull-right mt-3", head, body)
}
