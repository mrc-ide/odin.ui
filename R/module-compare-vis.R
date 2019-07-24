mod_vis_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Visualise"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("status_model1")),
          shiny::uiOutput(ns("status_model2")),
          ## TODO: status_configure but tone down warning to info
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
          ## mod_lock_ui(ns("lock")),
          shiny::hr(),
          ##
          shiny::actionButton(ns("reset"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-danger pull-right ml-2"),
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

  model <- list(model1, model2)
  cfg <- lapply(model, common_model_data_configuration,
                NULL, NULL, run_options)
  names <- list(long = vcapply(model, "[[", "name"),
                short = vcapply(model, "[[", "name_short"))

  pars <- compare_union_metadata(model1$info$pars, model2$info$pars, names)
  vars <- compare_union_metadata(model1$info$vars, model2$info$vars, names)
  cols <- odin_colours(vars$name, NULL, NULL)

  for (i in seq_along(cfg)) {
    cfg[[i]]$cols <- cols
  }

  download_names <- download_names(
    display = c(names$long, "Parameters"),
    filename = c(names$short, "parameters"),
    data = c("model1", "model2", "user"))

  list(data = NULL, model, configuration = cfg, link = NULL,
       pars = pars, vars = vars, cols = cols, names = names,
       download_names = download_names)
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


compare_download_names <- function(res, model_names) {
  display <- lapply(res, function(x) x$configuration$download_names$display)
  filename <- lapply(res, function(x) x$configuration$download_names$filename)
  data <- lapply(res, function(x)
    match(x$configuration$download_names$data, names(x$simulation)))

  n <- lengths(display)
  i <- rep(seq_along(n), n)
  download_names(
    display = sprintf("%s (%s)", unlist(display), model_names$long[i]),
    filename = sprintf("%s-%s", unlist(filename), model_names$short[i]),
    data = Map(c, i, unlist(data)))
}
