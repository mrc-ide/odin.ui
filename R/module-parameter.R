mod_parameter_input <- function(id, title) {
  ns <- shiny::NS(id)

  shiny::tagList(
    if (!is.null(title)) {
      shiny::titlePanel(title)
    },
    shiny::p(class = "mt-5"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(class = "form-horizontal mb-5",
                         shiny::uiOutput(ns("parameter_control")))),
      shiny::mainPanel(
        shiny::div(class = "graph-wrapper",
                   dygraphs::dygraphOutput(ns("result_plot"))))))
}

mod_parameter_server <- function(input, output, session,
                                 model, default_time, parameters,
                                 extra = NULL, output_control = NULL,
                                 time_scale = NULL) {
  ns <- session$ns
  model_output <- shiny::reactiveValues(data = NULL)

  metadata <- model_metadata(model)
  extra <- validate_extra(extra, metadata)
  user <- coef(model)

  parameters <- validate_model_parameters(model, parameters)
  control <- mod_parameter_control(metadata, default_time, parameters,
                                   extra, output_control, ns)

  path_css <- odin_ui_file("css/styles.css")

  output$parameter_control <- shiny::renderUI({
    shiny::div(shiny::includeCSS(path_css),control$tags)
  })

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
      replicates <- min(MAX_REPLICATES_PARAMETER_PLOT, input$replicates)

      model_output$data <- shiny::withProgress(
        run_model_parameters(model, focal_name, focal_values, pars, time,
                             replicates, extra, time_scale, collect,
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
    horizontal_form_group("Parameter to vary",
      raw_select_input(
      ns("focal_name"),
      names(parameters),
      selected = NA_character_)),
    horizontal_form_group(
      "From",
      raw_numeric_input(ns("focal_min"), p$range_min %||% p$default)),
    horizontal_form_group(
      "To",
      raw_numeric_input(ns("focal_max"), p$range_max %||% p$default)),
    horizontal_form_group(
      "Number of points",
      raw_numeric_input(ns("focal_len"), 20)),
    ns = ns)
  list(tags = tags)
}


## TODO: punting here quite a lot - this will need major work
mod_parameter_control_report <- function(extra, ns) {
  tags <- mod_model_control_section(
    "Report",
    horizontal_form_group(
      "Summarise",
      raw_select_input(ns("report_type"), set_names("last", "Last value"))),
    horizontal_form_group(
      "Variable",
      raw_select_input(ns("report_name"), names(extra))),
    ns = ns,
    collapsed = TRUE)
  list(tags = tags)
}


mod_parameter_control <- function(metadata, default_time, parameters,
                                  extra, output_control, ns = identity) {
  pars <- mod_model_control_parameters(parameters, ns)
  run_options <- mod_model_control_run_options(default_time, metadata, 100L,
                                               extra, output_control, ns,
                                               collapsed = TRUE)
  report <- mod_parameter_control_report(extra, ns)
  focal <- mod_parameter_control_focal(parameters, ns)

  tags <- shiny::tagList(
    shiny::div(
      class = "list-group odin-options",
      pars$tags,
      focal$tags,
      run_options$tags,
      report$tags),
    shiny::actionButton(ns("go_button"), "Run model",
            shiny::icon("play"),
            class = "btn-blue pull-right")
  )

  list(tags = tags,
       parameter_name_map = pars$name_map,
       has_start_time = run_options$has_start_time,
       discrete = metadata$features$discrete,
       stochastic = metadata$features$has_stochastic,
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
