mod_batch_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Batch"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          ## shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          ## TODO: status_configure but tone down warning to info
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
          shiny::uiOutput(ns("control_focal")),
          shiny::uiOutput(ns("control_plot")),
          ## mod_lock_ui(ns("lock")),
          shiny::hr(),
          ##
          shiny::actionButton(ns("reset"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("run"), "Run model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"))),
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
    mod_control_run_server, "control_run", model1, run_options)
  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "compare")

  shiny::observe({
    rv$configuration <- compare_configuration(
      model1(), model2(), control_run$result()$options)
  })

  output$control_graph <- shiny::renderUI({
    compare_control_graph(rv$configuration, session$ns)
  })

  output$control_focal <- shiny::renderUI({
    batch_control_focal(rv$configuration, session$ns)
  })

  output$control_plot <- shiny::renderUI({
    batch_control_plot(rv$configuration, session$ns)
  })

  output$control_plot_options <- shiny::renderUI({
    batch_control_plot_options(rv$configuration, input$plot_type, session$ns)
  })

  shiny::observeEvent(
    input$run, {
      rv$result <- with_success(batch_compare_run(
        rv$configuration, rv$focal, control_run$result()))
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      ## modules
      parameters$reset()
      control_run$reset()
      control_graph$reset()
    })

  shiny::observe({
    rv$focal <- batch_focal(
      input$focal_name, input$focal_pct, input$focal_n, parameters$result())
  })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      options <- list(type = input$plot_type,
                      slice_time = input$plot_slice_time,
                      extreme_type = input$plot_extreme_type)
      batch_compare_plot(rv$result$value, control_graph$result(), options)
    }
  })

  output$status_batch <- shiny::renderUI({
    batch_status(rv$result)
  })
}


batch_compare_run <- function(configuration, focal, run_options) {
  if (is.null(configuration) || is.null(focal)) {
    return(NULL)
  }

  f <- function(model) {
    configuration$model <- model
    batch_run(configuration, focal, run_options)
  }

  res1 <- f(configuration$model1)
  res2 <- f(configuration$model2)

  ## A bit of fairly ugly transformation on the download names:
  download_names <- download_names(
    display = c("Modelled", "Combined", "Parameters"),
    filename = c("modelled", "combined", "parameters"),
    data = c("smooth", "combined", "user"))
  i <- rep(1:2, each = length(download_names$display))
  model_names <- configuration$names

  configuration <- res1$configuration
  configuration$download_names <- download_names(
    display = sprintf("%s (%s)", download_names$display, model_names$long[i]),
    filename = sprintf("%s-%s", download_names$filename, model_names$short[i]),
    data = unname(Map(c, c("model1", "model2")[i], download_names$data)))

  list(configuration = configuration,
       focal = focal,
       simulation = list(model1 = res1$simulation, model2 = res2$simulation))
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
  f <- function(x) {
    result$simulation <- x
    batch_plot_series(result, NULL, y2_model, options)
  }

  series1 <- f(result$simulation$model1)
  series2 <- f(result$simulation$model2)

  plotly_combine_series(series1, series2, result$configuration$names)
}
