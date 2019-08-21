mod_vis_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Visualise"),
    shiny::sidebarLayout(
      odin_sidebar(
        run = ns("run"),
        reset = ns("reset"),
        auto_run = NULL,
        controls = shiny::tagList(
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
                                   run_options = NULL,
                                   show_table_summary = FALSE) {
  rv <- shiny::reactiveValues()
  run_options <- control_run_options(control_end_time = TRUE)

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars))
  control_graph <- shiny::callModule(
    mod_control_graph_server, "control_graph",
    shiny::reactive(rv$configuration))
  control_run <- shiny::callModule(
    mod_control_run_server, "control_run",
    reactive_successful(model2), run_options)
  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "compare")
  table <- shiny::callModule(
    mod_table_summary_server, "table", shiny::reactive(rv$result),
    show_table_summary)

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
      rv$result <- compare_vis_result(
        rv$configuration, parameters$result(), control_run$result())
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      modules$reset()
    })

  shiny::observe({
    previous <- shiny::isolate(rv$previous_series)
    control <- control_graph$result()
    res <- plotly_with_redraw(
      compare_vis_plot_series(rv$result$value, control$y2),
      previous,
      logscale_y = control$logscale)
    if (res$action == "draw") {
      output$odin_output <- plotly::renderPlotly(res$data)
    } else if (res$action == "redraw") {
      plotly::plotlyProxyInvoke(
        plotly::plotlyProxy("odin_output", session), "restyle", res$data)
    }
    rv$previous_series <- res$series
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
    list(result = rv$result$deps,
         modules = modules$get_state())
  }

  set_state <- function(state) {
    if (!is.null(state)) {
      rv$result <- compare_vis_result_rerun(state$result)
      rv$configuration <- rv$result$value$configuration
      modules$set_state(state$modules)
    }
  }

  list(get_state = get_state,
       set_state = set_state)
}


compare_vis_result <- function(configuration, user, run_options) {
  result <- with_success(
    compare_vis_run(configuration, user, run_options))
  result$deps <- list(
    configuration = compare_configuration_save(configuration),
    user = user,
    run_options = run_options)
  result
}


compare_vis_result_rerun <- function(deps) {
  if (is.null(deps)) {
    return(NULL)
  }
  configuration <- compare_configuration_restore(deps$configuration)
  compare_vis_result(configuration, deps$user, deps$run_options)
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
  if (is.null(result) || length(y2_model) == 0) {
    return(NULL)
  }
  f <- function(cfg, simulation) {
    x <- list(configuration = cfg, simulation = simulation)
    vis_plot_series(x, NULL, y2_model)
  }
  series <- Map2(f, result$configuration$configuration, result$simulation)
  plotly_combine_series(series, result$configuration$names)
}
