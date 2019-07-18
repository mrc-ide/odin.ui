mod_vis_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Visualise"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          ## TODO: status_configure but tone down warning to info
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
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
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
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
    mod_control_run_server, "control_run", model1, run_options)

  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "compare")

  table <- shiny::callModule(
    mod_table_summary_server, "table", shiny::reactive(rv$result))

  shiny::observe({
    rv$configuration <- compare_configuration(
      model1(), model2(), control_run$result()$options)
  })

  shiny::observeEvent(
    input$run, {
      user <- parameters$result()
      rv$result <- with_success(compare_vis_run(
        rv$configuration, user, control_run$result()))
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      ## modules
      parameters$reset()
      control_run$reset()
      control_graph$reset()
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      compare_vis_plot(rv$result$value, control_graph$result())
    }
  })

  output$status_vis <- shiny::renderUI({
    vis_status(rv$result)
  })
}


compare_union_metadata <- function(a, b, names) {
  ret <- rbind(a, b[!(b$name %in% a$name), , drop = FALSE])
  rownames(ret) <- NULL
  i <- 4 - (ret$name %in% b$name) - (ret$name %in% a$name) * 2
  lvls <- c("Shared", paste(names$long, "only"))
  ret$group <- factor(lvls[i], lvls)
  ret
}


compare_configuration <- function(model1, model2, run_options = NULL) {
  if (!isTRUE(model1$success) || !isTRUE(model2$success)) {
    return(NULL)
  }

  names <- list(long = c(model1$name, model2$name),
                short = c(model1$name_short, model2$name_short))

  pars <- compare_union_metadata(model1$info$pars, model2$info$pars, names)
  pars$value <- vnapply(pars$default_value, function(x) x %||% NA_real_)
  pars$id_value <- sprintf("par_value_%s", pars$name)

  vars <- compare_union_metadata(model1$info$vars, model2$info$vars, names)
  vars$id_graph_option <- sprintf("var_graph_option_%s", vars$name)

  cols <- odin_colours(vars$name, NULL, NULL)

  download_names <- download_names(
    display = c(names$long, "Parameters"),
    filename = c(names$short, "parameters"),
    data = c("model1", "model2", "user"))

  list(data = NULL, model1 = model1, model2 = model2, link = NULL,
       pars = pars, vars = vars, cols = cols, names = names,
       download_names = download_names)
}


compare_vis_run <- function(configuration, user, run_options) {
  if (is.null(configuration) || is.null(user)) {
    return(NULL)
  }

  err <- vlapply(user, is_missing)
  if (any(err)) {
    stop(sprintf("Missing parameter for %s",
                 paste(names(user)[err], collapse = ", ")))
  }

  mod1 <- configuration$model1$model(user = user, unused_user_action = "ignore")
  mod2 <- configuration$model2$model(user = user, unused_user_action = "ignore")

  t_end <- run_options$values$end
  if (is_missing(t_end)) {
    stop("Model run end time must be specified")
  }

  t_n <- 501 # TODO - should be configurable

  y1 <- model_run(mod1, t_end, t_n,
                  configuration$model1$info$features$discrete)
  y2 <- model_run(mod2, t_end, t_n,
                  configuration$model2$info$features$discrete)

  list(configuration = configuration,
       simulation = list(model1 = y1, model2 = y2, user = list_to_df(user)))
}


compare_vis_plot <- function(result, control) {
  y2 <- control$y2
  logscale <- control$logscale
  plot_plotly(compare_vis_plot_series(result, y2), logscale)
}


compare_vis_plot_series <- function(result, y2) {
  cfg <- result$configuration
  cols <- cfg$cols

  vars1 <- cfg$model1$info$vars$name
  vars2 <- cfg$model2$info$vars$name
  label1 <- sprintf("%s (%s)", vars1, cfg$names$long[[1]])
  label2 <- sprintf("%s (%s)", vars2, cfg$names$long[[2]])
  model1 <- result$simulation$model1
  model2 <- result$simulation$model2

  series1 <- plot_plotly_series_bulk(
    model1[, 1], model1[, vars1, drop = FALSE],
    col = cols$model, points = FALSE, y2 = y2,
    label = label1, legendgroup = vars1)

  series2 <- plot_plotly_series_bulk(
    model2[, 1], model2[, vars2, drop = FALSE],
    col = cols$model, points = FALSE, y2 = y2,
    label = label2, legendgroup = vars2, dash = "dash")

  c(series1, series2)
}
