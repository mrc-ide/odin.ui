mod_parameters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_parameters_server <- function(input, output, session, pars,
                                  with_option = FALSE, title = NULL) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$configuration <- parameters_configuration(pars(), with_option, title)
  })

  output$ui <- shiny::renderUI({
    parameters_ui(rv$configuration, session$ns)
  })

  shiny::observe({
    pars <- rv$configuration$pars
    rv$values <- get_inputs(input, pars$id_value, pars$name)
  })

  list(
    result = shiny::reactive(rv$values),
    set = function(values) browser())
}


parameters_configuration <- function(pars, with_option, title) {
  if (is.null(pars)) {
    return(NULL)
  }
  pars$id_value <- sprintf("value_%s", pars$name)
  pars$id_option <- sprintf("option_%s", pars$name)
  title <- title %||% "Model parameters"
  list(pars = pars, with_option = with_option, title = title)
}


parameters_ui <- function(configuration, ns, restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }
  pars <- configuration$pars

  ## TODO: the id_value bit comes out to the configuration within the
  ## module
  value <- restore$value %||% pars$value
  if (configuration$with_option) {
    f <- function(name, id_value, value, id_option, option) {
      shiny::fluidRow(
        shiny::column(
          10,
          simple_numeric_input(name, id_value, value)),
        shiny::column(
          2, shiny::checkboxInput(id_option, "", option)))
    }
    option <- restore$option %||% pars$option
    controls <- Map(f, pars$name, ns(pars$id_value),
                    value, ns(pars$id_option), option)
  } else {
    controls <- unname(Map(simple_numeric_input,
                           pars$name, ns(pars$id_value), value))
  }

  if ("group" %in% names(pars)) {
    controls <- split(controls, pars$group, drop = TRUE)
    f <- function(label, value) {
      shiny::div(shiny::tags$b(label), value)
    }
    controls <- unname(Map(f, names(controls), controls))
  }

  mod_model_control_section(configuration$title, controls, ns = ns)
}
