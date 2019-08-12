mod_vis_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Visualise"),
    shiny::sidebarLayout(
      odin_sidebar(
        run = ns("run"),
        reset = ns("reset"),
        import = NULL,
        auto_run = NULL,
        control = shiny::tagList(
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run"))),
        status = shiny::tagList(
          shiny::uiOutput(ns("status_model1")),
          shiny::uiOutput(ns("status_model2")))),
      shiny::mainPanel(
        shiny::div(
          class = "plotly-graph-wrapper",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("odin_output")))),
        shiny::div(
          class = "pull-right",
          mod_download_ui(ns("download")),
          mod_control_graph_ui(ns("control_graph"))),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_vis")))),
        mod_table_summary_ui(ns("table")))))
}


mod_vis_compare_server <- function(input, output, session, model1, model2,
                                   run_options = NULL) {
  rv <- shiny::reactiveValues()
  run_options <- control_run_options(control_end_time = TRUE)

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars))
  control_graph <- shiny::callModule(
    mod_control_graph_server, "control_graph",
    shiny::reactive(rv$configuration))
  control_run <- shiny::callModule(
    mod_control_run_server, "control_run", model2, run_options)
  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "compare")
  table <- shiny::callModule(
    mod_table_summary_server, "table", shiny::reactive(rv$result))

  modules <- submodules(parameters = parameters,
                        control_graph = control_graph,
                        control_run = control_run,
                        download = download,
                        table = table)

  shiny::observe({
    rv$configuration <- compare_configuration(
      model1(), model2(), control_run$result()$options)
  })

  shiny::observeEvent(
    input$run, {
      user <- parameters$result()
      control <- control_run$result()
      rv$result <- with_success(
        compare_vis_run(rv$configuration, user, control))
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      modules$reset()
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      compare_vis_plot(rv$result$value, control_graph$result())
    }
  })

  output$status_vis <- shiny::renderUI({
    vis_status(rv$result)
  })

  output$status_model1 <- shiny::renderUI({
    show_module_status_if_not_ok(model1()$status)
  })

  output$status_model2 <- shiny::renderUI({
    show_module_status_if_not_ok(model2()$status)
  })

  get_state <- function() {
    if (is.null(rv$configuration) || is.null(rv$result)) {
      return(NULL)
    }
    list(user = rv$result$value$inputs$user,
         options = rv$result$value$inputs$options,
         modules = modules$get_state())
  }

  set_state <- function(state) {
    if (is.null(state)) {
      return()
    }
    rv$configuration <- compare_configuration(
      model1(), model2(), control_run$result()$options)
    modules$set_state(state$modules)
    ## TODO: not sure why this is not working, but we're not getting
    ## the previous model here correctly.
    if (!is.null(rv$configuration)) {
      rv$result <- with_success(compare_vis_run(
        rv$configuration, state$user, state$options))
    }
  }

  list(get_state = get_state,
       set_state = set_state)
}


compare_vis_run <- function(configuration, user, options) {
  if (is.null(configuration) || is.null(user)) {
    return(NULL)
  }
  res <- lapply(configuration$configuration, vis_run, user, options)
  configuration$download_names <-
    compare_download_names(res, configuration$names)
  list(configuration = configuration,
       inputs = list(user = user, options = options),
       simulation = lapply(res, "[[", "simulation"))
}


compare_vis_plot <- function(result, control) {
  y2 <- control$y2
  logscale <- control$logscale
  plot_plotly(compare_vis_plot_series(result, y2), logscale)
}


compare_vis_plot_series <- function(result, y2_model) {
  f <- function(cfg, simulation) {
    x <- list(configuration = cfg, simulation = simulation)
    vis_plot_series(x, NULL, y2_model)
  }
  series <- Map(f, result$configuration$configuration, result$simulation)
  plotly_combine_series(series, result$configuration$names)
}
