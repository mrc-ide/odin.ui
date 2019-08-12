mod_batch_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Batch"),
    shiny::sidebarLayout(
      odin_sidebar(
        run = ns("run"),
        reset = ns("reset"),
        import = ns("import_button"),
        auto_run = NULL,
        controls = shiny::tagList(
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
          mod_control_focal_ui(ns("control_focal")),
          mod_control_batch_plot(ns("control_batch_plot"))),
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
          shiny::column(4, shiny::uiOutput(ns("status_batch")))))))
}


mod_batch_compare_server <- function(input, output, session, model1, model2,
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
  control_focal <- shiny::callModule(
    mod_control_focal_server, "control_focal",
    shiny::reactive(rv$configuration$pars),
    parameters$result)
  control_plot <- shiny::callModule(
    mod_control_batch_plot_server, "control_batch_plot",
    shiny::reactive(!is.null(rv$configuration)))
  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "batch")

  modules <- submodules(
    parameters = parameters, control_graph = control_graph,
    control_run = control_run, control_focal = control_focal,
    control_plot = control_plot, download = download)

  shiny::observe({
    rv$configuration <- compare_configuration(
      model1(), model2(), control_run$result()$options)
  })

  shiny::observeEvent(
    input$run, {
      rv$result <- with_success(batch_compare_run(
        rv$configuration, control_focal$result(), control_run$result()))
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      modules$reset()
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      batch_compare_plot(rv$result$value, control_graph$result(),
                         control_plot$result())
    }
  })

  output$status_batch <- shiny::renderUI({
    batch_status(rv$result)
  })

  output$status_model1 <- shiny::renderUI({
    show_module_status_if_not_ok(model1()$status)
  })

  output$status_model2 <- shiny::renderUI({
    show_module_status_if_not_ok(model2()$status)
  })

  NULL
}


batch_compare_run <- function(configuration, focal, run_options) {
  if (is.null(configuration) || is.null(focal)) {
    return(NULL)
  }

  res <- lapply(configuration$configuration, batch_run, focal, run_options)
  configuration$configuration <- lapply(res, "[[", "configuration")
  simulation <- lapply(res, "[[", "simulation")
  configuration$download_names <-
    compare_download_names(res, configuration$names)
  list(configuration = configuration, simulation = simulation)
}


batch_compare_plot <- function(result, control, options) {
  y2_model <- control$y2
  logscale <- control$logscale
  xlab <- batch_xlab(options$type, result$focal)
  ylab <- batch_ylab(options$type, result$focal)
  plot_plotly(batch_compare_plot_series(result, y2_model, options),
              logscale, xlab, ylab)
}


batch_compare_plot_series <- function(result, y2_model, options) {
  f <- function(cfg, simulation) {
    x <- list(configuration = cfg,
              focal = result$focal,
              simulation = simulation)
    batch_plot_series(x, NULL, y2_model, options)
  }

  series <- Map(f, result$configuration$configuration, result$simulation)
  plotly_combine_series(series, result$configuration$names)
}
