mod_parameter_input <- function(id, title) {
  ns <- shiny::NS(id)

  shiny::tagList(
    if (!is.null(title)) {
      shiny::titlePanel(title)
    } else {
      ## TODO: This is not really super tidy but I need a little
      ## spare vertical space here before the panel layout but I
      ## don't see the cleanest way of adding it.
      shiny::p(class = "spacer")
    },
    shiny::sidebarLayout(
      shiny::sidebarPanel(shiny::uiOutput(ns("parameter_control"))),
      shiny::mainPanel(dygraphs::dygraphOutput(ns("result_plot")))))
}


mod_parameter_server <- function(input, output, session,
                                 model, default_time, parameters,
                                 extra = NULL) {
  ns <- session$ns
  model_output <- shiny::reactiveValues(data = NULL)

  graph_data <- attr(model, "graph_data")()
  extra <- validate_extra(extra, graph_data)
  user <- coef(model)

  parameters <- validate_model_parameters(model, parameters)
  control <- mod_parameter_control(graph_data, default_time, parameters,
                                   extra, ns)

  output$parameter_control <- shiny::renderUI(control$tags)

  shiny::observeEvent(
    input$go_button, {
      pars <- mod_model_getpars(input, control$parameter_name_map)
      time <- mod_model_gettime(input, control$has_start_time, control$discrete)
      ## TODO: this needs sanitisation - non missing, range of greater
      ## than zero, len of at least 3
      focal_name <- input$focal_name
      focal_values <- seq(input$focal_min, input$focal_max,
                          length.out = input$focal_len)
      if (user$integer[user$name == focal_name]) {
        focal_values <- round(focal_values)
      }

      collect <- parameter_collector(input$report_type, input$report_name)
      replicates <- input$replicates

      model_output$data <- shiny::withProgress(
        run_model_parameters(model, focal_name, focal_values, pars, time,
                             replicates, extra, collect,
                             callback_shiny_progress),
        max = length(focal_values), value = 0, message = "Running simulations")
    })

  shiny::observe({
    if (!is.null(model_output$data)) {
      output$result_plot <-
        dygraphs::renderDygraph(parameter_plot(model_output$data))
    }
  })
}


mod_parameter_control_focal <- function(parameters, ns) {
  p <- parameters[[1]]
  tags <- mod_model_control_section(
    "Focal parameter",
    shiny::selectInput(
      ns("focal_name"),
      "Parameter to vary",
      names(parameters),
      selected = NA_character_),
    shiny::numericInput(ns("focal_min"), "From", p$range_min %||% p$default),
    shiny::numericInput(ns("focal_max"), "To", p$range_max %||% p$default),
    shiny::numericInput(ns("focal_len"), "Number of points", 20),
    ns = ns)
  list(tags = tags)
}


## TODO: punting here quite a lot - this will need major work
mod_parameter_control_report <- function(extra, ns) {
  tags <- mod_model_control_section(
    "Report",
    shiny::selectInput(ns("report_type"), "Summarise",
                       set_names("last", "Last value")),
    shiny::selectInput(ns("report_name"), "Variable", names(extra)),
    ns = ns)
  list(tags = tags)
}


mod_parameter_control <- function(graph_data, default_time, parameters, extra,
                                  ns = identity) {
  pars <- mod_model_control_parameters(parameters, ns)
  run_options <- mod_model_control_run_options(default_time, graph_data, 100L, extra, ns)
  report <- mod_parameter_control_report(extra, ns)
  focal <- mod_parameter_control_focal(parameters, ns)

  tags <- shiny::tagList(
    shiny::actionButton(ns("go_button"), "Run model",
                        shiny::icon("play"),
                        class = "btn-primary"),
    shiny::hr(),
    shiny::div(
      class = "panel-group",
      pars$tags,
      run_options$tags,
      report$tags,
      focal$tags))

  list(tags = tags,
       parameter_name_map = pars$name_map,
       has_start_time = run_options$has_start_time,
       discrete = graph_data$discrete,
       stochastic = graph_data$stochastic,
       replicates = run_options$replicates)
}


parameter_collector <- function(verb, target, name = target) {
  stopifnot(verb == "last")
  force(name)
  force(target)
  function(d) {
    len <- length(d$t)
    ret <- lapply(target, function(x) d[[x]][[len]])
    names(ret) <- name
    ret
  }
}
