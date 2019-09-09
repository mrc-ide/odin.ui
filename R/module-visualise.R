## This is basically the module from module-model-plotly.R but we need
## a few tweaks and I don't want to bog that down any further at
## present (though hopefully we'll join back up eventually).  We want:
##
## * reactive model input
## * time to be fixed
## * include data (optionally perhaps?)
## * no replicates
## * no extra
##
## Then we'll use this as a base for the batch plot and the phase plot

mod_vis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_help_ui(ns("help"), class = "pull-right"),
    shiny::titlePanel("Visualise"),
    shiny::sidebarLayout(
      odin_sidebar(
        run = ns("run"),
        reset = ns("reset"),
        auto_run = ns("auto_run"),
        controls = shiny::tagList(
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
          mod_lock_ui(ns("lock"))),
        status = shiny::tagList(
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")))),

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
        mod_table_summary_ui(ns("table")),
        mod_model_code_ui(ns("code")))))
}


## Basic flow is
##   {data, model, link} => configuration
##   configuration => control interface
##   {configuration, control interface} => result
##   {result, control inteface} => plot
mod_vis_server <- function(input, output, session, data, model, link,
                           import = NULL, run_options = NULL,
                           show_table_summary = FALSE) {
  rv <- shiny::reactiveValues()

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars), import = import)
  control_graph <- shiny::callModule(
    mod_control_graph_server, "control_graph",
    shiny::reactive(rv$configuration))
  control_run <- shiny::callModule(
    mod_control_run_server, "control_run",
    reactive_successful(model), run_options)
  code <- shiny::callModule(
    mod_model_code_server, "code", model)

  table <- shiny::callModule(
    mod_table_summary_server, "table", shiny::reactive(rv$result),
    show_table_summary)
  help <- shiny::callModule(
    mod_help_server, "help", odin_ui_file("md/help/vis.md"))

  set_result <- function(result) {
    parameters$set(result$value$simulation$user)
    rv$result <- result
  }

  show_locked <- shiny::reactive(
    !is.null(rv$configuration) && !control_run$result()$options$replicates)
  locked <- shiny::callModule(
    mod_lock_server, "lock", show_locked, shiny::reactive(rv$result),
    set_result)

  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "visualise")

  modules <- submodules(parameters = parameters,
                        control_graph = control_graph,
                        control_run = control_run,
                        code = code, locked = locked, download = download)

  output$status_data <- shiny::renderUI({
    show_module_status_if_not_ok(data()$status)
  })

  output$status_model <- shiny::renderUI({
    show_module_status_if_not_ok(model()$status)
  })

  output$status_vis <- shiny::renderUI({
    vis_status(rv$result)
  })

  shiny::observe({
    rv$configuration <- common_model_data_configuration(
      model(), data(), link(), control_run$result()$options)
  })

  shiny::observeEvent(
    input$run, {
      rv$result <- vis_result(
        rv$configuration, parameters$result(), control_run$result())
   })

  shiny::observe({
    if (isTRUE(input$auto_run)) {
      rv$result <- vis_result(
        rv$configuration, parameters$result(), control_run$result())
    }
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
      vis_plot_series(rv$result$value, locked$result()$value, control$y2),
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

  get_state <- function() {
    list(result = rv$result$deps,
         modules = modules$get_state())
  }

  set_state <- function(state) {
    if (!is.null(state)) {
      rv$result <- vis_result_rerun(state$result)
      rv$configuration <- rv$result$value$configuration
      modules$set_state(state$modules)
    }
  }

  list(get_state = get_state,
       set_state = set_state)
}


vis_result <- function(configuration, user, run_options) {
  result <- with_success(vis_run(configuration, user, run_options))
  result$deps <- list(
    configuration = common_model_data_configuration_save(configuration),
    user = user,
    run_options = run_options)
  result
}


vis_result_rerun <- function(deps) {
  if (is.null(deps)) {
    return(NULL)
  }
  configuration <- common_model_data_configuration_restore(deps$configuration)
  vis_result(configuration, deps$user, deps$run_options)
}


## TODO: this probably breaks into two - one with data and one without?
vis_run <- function(configuration, user, run_options) {
  if (is.null(configuration) || is.null(user)) {
    return(NULL)
  }

  vars <- configuration$vars
  if (!any(vars$include)) {
    return(NULL)
  }

  err <- vlapply(user, is_missing)
  if (any(err)) {
    stop(sprintf("Missing parameter for %s",
                 paste(names(user)[err], collapse = ", ")))
  }

  ## TODO: also split below here base on data.
  if (run_options$options$replicates) {
    vis_run_replicate(configuration, user, run_options)
  } else {
    vis_run_single(configuration, user, run_options)
  }
}


vis_run_single <- function(configuration, user, run_options) {
  data <- configuration$data
  vars <- configuration$vars
  has_data <- !is.null(data)

  if (isTRUE(run_options$options$control_end_time)) {
    t_start <- 0
    t_end <- run_options$values$end
    if (is_missing(t_end)) {
      stop("Model run end time must be specified")
    }
  } else {
    t_data <- data$data[[data$name_time]]
    t_start <- min(t_data)
    t_end <- max(t_data)
  }

  model <- configuration$model
  if (model$info$features$has_user) {
    mod <- model$model(user = user, unused_user_action = "ignore")
  } else {
    mod <- model$model()
  }

  if (model$info$features$discrete) {
    stop("Discrete time models are not supported")
  }
  t_smooth <- seq(t_start, t_end, length.out = run_options$values$nout)
  result_smooth <- mod$run(c(if (t_smooth[[1]] > 0) 0, t_smooth))
  i <- setdiff(colnames(result_smooth), vars$name[!vars$include])
  result_smooth <- result_smooth[, i, drop = FALSE]

  if (has_data) {
    t_after_zero <- t_data[[1]] > 0
    result_data <- mod$run(c(if (t_after_zero) 0, t_data))
    if (t_after_zero) {
      result_data <- result_data[-1, , drop = FALSE]
    }
    result_data <- result_data[, i, drop = FALSE]
    result_combined <- cbind(result_data, data$data)
  } else {
    result_data <- result_combined <- NULL
  }

  list(configuration = configuration,
       type = "single",
       simulation = list(data = result_data,
                         combined = result_combined,
                         smooth = result_smooth,
                         user = list_to_df(user)))
}


vis_run_replicate <- function(configuration, user, run_options) {
  if (!configuration$model$info$features$has_stochastic) {
    stop("Replication interface only meaningful for stochastic models")
  }
  if (!is.null(configuration$data)) {
    stop("Data not supported")
  }
  if (!run_options$options$control_end_time) {
    stop("Need control over end time")
  }

  replicates <- run_options$values$replicates
  if (is_missing(replicates)) {
    stop("Replicates must be specified")
  }
  if (run_options$values$no_run) {
    stop("Too many replicates requested")
  }

  t_start <- 0
  t_end <- run_options$values$end
  if (is_missing(t_end)) {
    stop("Model run end time must be specified")
  }

  model <- configuration$model
  if (model$info$features$has_user) {
    mod <- model$model(user = user, unused_user_action = "ignore")
  } else {
    mod <- model$model()
  }

  dt <- if (run_options$options$scale_time) mod$contents()$dt else 1
  nsteps <- 500
  steps <- discrete_times(t_end, nsteps, dt)
  t <- steps * dt

  result <- mod$run(steps, replicate = replicates)
  result <- result[, configuration$vars$name, , drop = FALSE]
  result_mean <- cbind(t = t, rowMeans(result, dims = 2))

  ## Identify deterministic outputs:
  if (replicates == 1L) {
    fixed <- rep(TRUE, ncol(result))
  } else {
    f <- function(i, y) {
      all(diff(apply(y[, i, ], 1, range)) == 0)
    }
    fixed <- vlapply(seq_len(ncol(result)), f, result)
  }
  if (any(fixed)) {
    result <- result[, !fixed, , drop = FALSE]
  }

  list(configuration = configuration,
       type = "replicate",
       run_options = run_options,
       simulation = list(replicates = result,
                         smooth = result_mean,
                         fixed = fixed,
                         time = t * dt,
                         user = list_to_df(user)))
}


vis_plot_series <- function(result, locked, y2_model) {
  if (length(y2_model) == 0 || all(vlapply(y2_model, is.null))) {
    return(NULL)
  }
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, cfg$link$map)
  if (identical(result$type, "replicate")) {
    vis_plot_series_replicates(result, y2)
  } else {
    c(vis_plot_series_locked(result, locked, y2),
      vis_plot_series_focal(result, y2))
  }
}


vis_plot_series_focal <- function(result, y2) {
  cfg <- result$configuration
  cols <- cfg$cols
  vars <- cfg$vars[cfg$vars$include, ]

  model_vars <- intersect(vars$name, names(y2$model))
  model_data <- result$simulation$smooth
  show <- vars$show[match(model_vars, vars$name)]
  series_model <- plot_plotly_series_bulk(
    model_data[, 1], model_data[, model_vars, drop = FALSE],
    cols$model, FALSE, y2$model, legendgroup = TRUE, show = show)

  data_data <- cfg$data$data
  data_time <- cfg$data$name_time
  data_vars <- cfg$data$name_vars
  series_data <- plot_plotly_series_bulk(
    data_data[[data_time]], data_data[data_vars], cols$data, TRUE, y2$data)

  c(series_model, series_data)
}


vis_plot_series_replicates <- function(result, y2) {
  cfg <- result$configuration
  cols <- cfg$cols
  vars <- cfg$vars[cfg$vars$include, ]

  model_vars <- intersect(vars$name, names(y2$model))
  show <- set_names(vars$show[match(model_vars, vars$name)], model_vars)

  xy_replicates <- result$simulation$replicates
  xy_mean <- result$simulation$smooth
  time <- xy_mean[, 1]

  n <- dim(xy_replicates)[[3]]
  if (result$run_options$values$no_show) {
    series_replicates <- NULL
  } else {
    v <- intersect(model_vars, dimnames(xy_replicates)[[2]])
    series_replicates <- lapply(v, function(nm)
      plot_plotly_series_replicate(time, xy_replicates[, nm, ], nm,
                                   col = transp(cols$model[[nm]]),
                                   points = FALSE,
                                   y2 = y2$model[[nm]], legendgroup = nm,
                                   show = show[[nm]], width = 0.5))
    series_replicates <- unlist(series_replicates, FALSE, FALSE)
  }

  y <- xy_mean[, model_vars, drop = FALSE]
  label_mean <- sprintf("%s (mean)", colnames(y))
  label_mean[result$simulation$fixed] <-
    colnames(y)[result$simulation$fixed]
  series_mean <- plot_plotly_series_bulk(time, y, cols$model,
                                         points = FALSE, y2 = y2$model,
                                         show = show, label = label_mean)
  c(series_replicates, series_mean)
}


vis_plot_series_locked <- function(result, locked, y2) {
  if (is.null(locked)) {
    return(NULL)
  }
  if (identical(result, locked)) {
    return(NULL)
  }

  cfg <- result$configuration
  cols <- cfg$cols
  vars <- cfg$vars[cfg$vars$include, ]

  model_vars <- intersect(locked$configuration$vars$name,
                          intersect(vars$name, names(y2$model)))
  model_data <- locked$simulation$smooth
  show <- vars$show[match(model_vars, vars$name)]
  plot_plotly_series_bulk(
    model_data[, 1], model_data[, model_vars, drop = FALSE],
    cols$model, FALSE, y2$model, dash = "dot", width = 1,
    showlegend = FALSE, legendgroup = TRUE, show = show)
}



vis_plot <- function(result, locked, control) {
  y2_model <- control$y2
  logscale <- control$logscale
  plot_plotly(vis_plot_series(result, locked, y2_model), logscale)
}


vis_status <- function(result) {
  if (!is.null(result$error)) {
    simple_panel("danger", "Error running model", result$error)
  }
}
