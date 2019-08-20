mod_batch_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_help_ui(ns("help"), class = "pull-right"),
    shiny::titlePanel("Sensitivity"),
    shiny::sidebarLayout(
      odin_sidebar(
        run = ns("run"),
        reset = ns("reset"),
        auto_run = NULL,
        controls = shiny::tagList(
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
          mod_control_focal_ui(ns("control_focal")),
          mod_control_batch_plot_ui(ns("control_batch_plot")),
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
          shiny::column(4, shiny::uiOutput(ns("status_batch")))))))
}


mod_batch_server <- function(input, output, session, model, data, link,
                             import = NULL, run_options = NULL) {
  rv <- shiny::reactiveValues()

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars), import = import)
  control_graph <- shiny::callModule(
    mod_control_graph_server, "control_graph",
    shiny::reactive(rv$configuration))
  control_run <- shiny::callModule(
    mod_control_run_server, "control_run",
    reactive_successful(model), run_options, warn_show = FALSE)
  control_focal <- shiny::callModule(
    mod_control_focal_server, "control_focal",
    shiny::reactive(rv$configuration$pars),
    parameters$result)
  control_plot <- shiny::callModule(
    mod_control_batch_plot_server, "control_batch_plot",
    shiny::reactive(!is.null(rv$configuration)))
  help <- shiny::callModule(
    mod_help_server, "help", odin_ui_file("md/help/batch.md"))

  set_result <- function(result) {
    parameters$set(result$value$simulation$user)
    rv$result <- result
  }
  locked <- shiny::callModule(
    mod_lock_server, "lock",
    shiny::reactive(!is.null(rv$configuration)), shiny::reactive(rv$result),
    set_result)

  download <- shiny::callModule(
    mod_download_server, "download", shiny::reactive(rv$result$value),
    "batch")

  modules <- submodules(
    parameters = parameters, control_graph = control_graph,
    control_run = control_run, control_focal = control_focal,
    control_plot = control_plot, locked = locked,
    download = download)

  output$status_data <- shiny::renderUI({
    show_module_status_if_not_ok(data()$status)
  })

  output$status_model <- shiny::renderUI({
    show_module_status_if_not_ok(model()$status)
  })

  output$status_batch <- shiny::renderUI({
    batch_status(rv$result)
  })

  shiny::observe({
    rv$configuration <- common_model_data_configuration(
      model(), data(), link(), control_run$result()$options)
  })

  shiny::observeEvent(
    input$run, {
      rv$result <- batch_result(
        rv$configuration, control_focal$result(), control_run$result())
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      modules$reset()
    })

  shiny::observe({
    previous <- shiny::isolate(rv$previous_series)
    result <- rv$result$value
    control <- control_graph$result()
    options <- control_plot$result()

    series <- batch_plot_series(
      result, locked$result()$value, control$y2, options)
    res <- plotly_with_redraw(
      series, previous,
      logscale_x = batch_logscale_x(options$type, result$focal),
      logscale_y = control$logscale,
      xlab = batch_xlab(options$type, result$focal))

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
      rv$result <- batch_result_rerun(state$result)
      rv$configuration <- rv$result$value$configuration
      modules$set_state(state$modules)
    }
  }

  list(get_state = get_state,
       set_state = set_state)
}


batch_result <- function(configuration, focal, run_options) {
  result <- with_success(batch_run(configuration, focal, run_options))
  result$deps <- list(
    configuration = common_model_data_configuration_save(configuration),
    focal = focal,
    run_options = run_options)
  result
}


batch_result_rerun <- function(deps) {
  if (is.null(deps)) {
    return(NULL)
  }
  configuration <- common_model_data_configuration_restore(deps$configuration)
  batch_result(configuration, deps$focal, deps$run_options)
}


batch_run <- function(configuration, focal, run_options) {
  if (is.null(focal)) {
    return(NULL)
  }

  name <- focal$name
  n <- constrain(focal$n, 2, 20)
  if (isTRUE(focal$logarithmic)) {
    value <- seq_log(focal$from, focal$to, length.out = n)
  } else {
    value <- seq(focal$from, focal$to, length.out = n)
  }
  pars <- configuration$pars
  i <- match(name, pars$name)
  if (is.na(i)) {
    value <- focal$value
  } else {
    value <- value[value >= pars$min[[i]] & value <= pars$max[[i]]]
  }

  user <- focal$base
  f <- function(p) {
    user[[name]] <- p
    vis_run(configuration, user, run_options)
  }

  ## First, the central runs as our base set:
  central <- vis_run(configuration, user, run_options)

  ## Output types we'll work with (TODO: whitelist will be easier)
  types <- setdiff(names(drop_null(central$simulation)),
                   c("combined", "replicates", "fixed", "time"))
  ## Then the sensitivity around that
  batch <- lapply(value, f)
  g <- function(type) {
    combine_colwise(lapply(batch, function(x) x$simulation[[type]]))
  }

  ## Organise output that will download cleanly:
  simulation <- set_names(lapply(types, g), types)

  if (!is.null(central$simulation$data)) {
    simulation$combined <- cbind(simulation$data, configuration$data$data)
  }

  ## Update with central runs too:
  simulation$user <- cbind(
    central$simulation$user,
    simulation$user[!grepl("^name", names(simulation$user))],
    stringsAsFactors = FALSE)
  for (i in setdiff(types, "user")) {
    simulation[[i]] <- cbind(
      central$simulation[[i]],
      simulation[[i]][, -1, drop = FALSE])
  }

  ## And output for plotting
  simulation$batch <- batch
  simulation$central <- central
  configuration$focal <- list(name = name, value = value, base = user)

  list(configuration = configuration,
       focal = focal,
       simulation = simulation)
}


batch_plot_series <- function(result, locked, y2_model, options) {
  if (length(y2_model) == 0) {
    message("skipping plot")
    return(NULL)
  }
  fn <- switch(
    options$type %||% "trace",
    trace = batch_plot_series_trace,
    slice = batch_plot_series_slice,
    extreme = batch_plot_series_extreme,
    textreme = batch_plot_series_textreme)
  fn(result, locked, y2_model, options)
}


batch_plot_series_slice <- function(result, locked, y2_model, options) {
  cfg <- result$configuration
  cols <- cfg$cols$model
  vars <- cfg$vars[cfg$vars$include, ]
  model_vars <- vars$name

  xy <- result$simulation$central$simulation$smooth

  t <- options$slice_time
  if (is_missing(t)) {
    i <- nrow(xy)
  } else {
    i <- which.min(abs(t - xy[, 1]))
  }

  x <- cfg$focal$value
  tmp <- lapply(result$simulation$batch, function(x)
    x$simulation$smooth[i, -1, drop = FALSE])
  y <- do.call(rbind, tmp)

  ## TODO: also add the central on as a point here?
  ## TODO: also add the locked data on here
  plot_plotly_series_bulk(x, y, cols, points = FALSE, y2 = y2_model,
                          legendgroup = colnames(y))
}


batch_plot_series_extreme <- function(result, locked, y2_model, options) {
  if (is_missing(options$extreme_type)) {
    return(NULL)
  }

  cfg <- result$configuration
  cols <- cfg$cols$model
  vars <- cfg$vars[cfg$vars$include, ]
  model_vars <- vars$name

  ## There's some annotation work do to make this nicer.
  f <- if (options$extreme_type == "max") max else min
  x <- cfg$focal$value
  tmp <- lapply(result$simulation$batch, function(x)
    apply(x$simulation$smooth[, -1, drop = FALSE], 2, f))
  y <- do.call(rbind, tmp)

  ## TODO: also add the central on as a point here?
  ## TODO: also add the locked data on here
  plot_plotly_series_bulk(x, y, cols, points = FALSE, y2 = y2_model,
                          legendgroup = colnames(y))
}


batch_plot_series_textreme <- function(result, locked, y2_model, options) {
  if (is_missing(options$extreme_type)) {
    return(NULL)
  }

  cfg <- result$configuration
  cols <- cfg$cols$model
  vars <- cfg$vars[cfg$vars$include, ]
  model_vars <- vars$name

  ## There's some annotation work do to make this nicer.
  f <- if (options$extreme_type == "max") which.max else which.min
  x <- cfg$focal$value
  t <- result$simulation$central$simulation$smooth[, 1]
  tmp <- lapply(result$simulation$batch, function(x)
    apply(x$simulation$smooth[, -1, drop = FALSE], 2, f))
  y <- do.call(rbind, tmp)
  y[] <- t[c(y)]

  ## TODO: also add the central on as a point here?
  ## TODO: also add the locked data on here
  plot_plotly_series_bulk(x, y, cols, points = FALSE, y2 = y2_model,
                          legendgroup = colnames(y))
}


batch_plot_series_trace <- function(result, locked, y2_model, options) {
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, cfg$link$map)
  c(batch_plot_series_trace_locked(result, locked, y2),
    batch_plot_series_trace_focal(result, y2),
    batch_plot_series_trace_data(result, y2))
}


batch_plot_series_trace_focal <- function(result, y2) {
  batch_plot_series_trace_modelled(result, y2, FALSE)
}


batch_plot_series_trace_locked <- function(result, locked, y2) {
  if (is.null(locked)) {
    return(NULL)
  }
  if (identical(result, locked)) {
    return(NULL)
  }

  model_vars <- intersect(locked$configuration$vars$name, y2)
  batch_plot_series_trace_modelled(locked, y2, TRUE)
}


batch_plot_series_trace_modelled <- function(result, y2, locked = FALSE) {
  cfg <- result$configuration
  cols <- cfg$cols
  vars <- cfg$vars[cfg$vars$include, ]
  model_vars <- intersect(
    vars$name,
    colnames(result$simulation$central$simulation$smooth))

  if (locked) {
    width <- 1
    dash <- "dot"
  } else {
    width <- 2
    dash <- NULL
  }

  xy <- result$simulation$central$simulation$smooth
  series_central <- plot_plotly_series_bulk(
    xy[, 1], xy[, model_vars, drop = FALSE], cols$model,
    points = FALSE, y2 = y2$model, showlegend = !locked,
    legendgroup = model_vars, dash = dash, width = width,
    show = FALSE)

  f <- function(nm) {
    t <- result$simulation$smooth[, 1]
    y <- lapply(result$simulation$batch, function(x) x$simulation$smooth[, nm])
    m <- matrix(unlist(y), length(y[[1]]), length(y))
    colnames(m) <- sprintf("%s (%s = %s)", nm, cfg$focal$name, cfg$focal$value)
    col <- set_names(rep(cols$model[[nm]], ncol(m)), colnames(m))
    plot_plotly_series_bulk(t, m, col, points = FALSE, y2 = y2$model[[nm]],
                            legendgroup = nm, showlegend = FALSE,
                            width = width / 2, dash = dash,
                            show = FALSE)
  }

  series_batch <- unlist(lapply(model_vars, f), FALSE, FALSE)

  c(series_central, series_batch)
}


batch_plot_series_trace_data <- function(result, include) {
  cfg <- result$configuration
  cols <- cfg$cols
  data <- cfg$data$data
  data_time <- data[[cfg$data$name_time]]
  plot_plotly_series_bulk(
    data_time, data[names(cols$data)], cols$data, TRUE, FALSE)
}


batch_plot <- function(result, locked, control, options) {
  y2 <- control$y2
  logscale_y <- control$logscale
  xlab <- batch_xlab(options$type, result$focal)
  logscale_x <- batch_logscale_x(options$type, result$focal)
  plot_plotly(batch_plot_series(result, locked, y2, options),
              logscale_y, xlab, logscale_x = logscale_x)
}


batch_status <- function(result) {
  if (!is.null(result$error)) {
    simple_panel("danger", "Error running model", result$error)
  }
}


batch_xlab <- function(type, focal) {
  switch(
    type %||% "trace",
    trace = "Time",
    slice = focal$name,
    extreme = focal$name,
    textreme = focal$name)
}


batch_logscale_x <- function(type, focal) {
  isTRUE(focal$logarithmic) && type != "trace"
}


batch_ylab <- function(type, focal) {
  switch(
    type %||% "trace",
    trace = NULL,
    slice = NULL,
    extreme = "Maximum value",
    textreme = "Time")
}
