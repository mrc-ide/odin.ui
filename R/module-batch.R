mod_batch_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Sensitivity"),
    shiny::sidebarLayout(
      ## NOTE: almost the same as the visualiser
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          mod_parameters_ui(ns("parameters")),
          mod_control_run_ui(ns("control_run")),
          shiny::uiOutput(ns("control_focal")),
          shiny::uiOutput(ns("control_plot")),
          mod_lock_ui(ns("lock")),
          shiny::hr(),
          shiny::uiOutput(ns("import_button"), inline = TRUE),
          shiny::actionButton(ns("reset"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-red pull-right ml-2"),
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
          shiny::uiOutput(ns("control_graph"))),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_batch")))))))
}


mod_batch_server <- function(input, output, session, model, data, link,
                             import = NULL, run_options = NULL) {
  rv <- shiny::reactiveValues()

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars))
  control_run <- shiny::callModule(
    mod_control_run_server, "control_run", model, run_options)

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
    "visualise")

  output$status_data <- shiny::renderUI({
    show_module_status_if_not_ok(data()$status)
  })

  output$status_model <- shiny::renderUI({
    show_module_status_if_not_ok(model()$status)
  })

  output$status_focal <- shiny::renderText({
    batch_status_focal(rv$focal)
  })

  output$status_batch <- shiny::renderUI({
    batch_status(rv$result)
  })

  shiny::observe({
    rv$configuration <- common_model_data_configuration(
      model(), data(), link(), control_run$result()$options)
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

  output$control_graph <- shiny::renderUI({
    batch_control_graph(rv$configuration, session$ns)
  })

  shiny::observeEvent(
    input$run, {
      rv$result <- with_success(batch_run(
        rv$configuration, rv$focal, control_run$result()))
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      parameters$reset()
      locked$clear()
      control_run$reset()
      output$control_focal <- shiny::renderUI(
        batch_control_focal(rv$configuration, session$ns))
      output$control_plot <- shiny::renderUI(
        batch_control_plot(rv$configuration, session$ns))
    })

  shiny::observe({
    rv$focal <- batch_focal(
      input$focal_name, input$focal_pct, input$focal_n, parameters$result())
  })

  output$import_button <- shiny::renderUI({
    if (!is.null(import) && !is.null(import$user())) {
      shiny::actionButton(
        session$ns("import"), import$title, import$icon)
    }
  })

  shiny::observeEvent(
    input$import, {
      user <- import$user()
      if (parameters$set(user)) {
        rv$focal <- batch_focal(
          input$focal_name, input$focal_pct, input$focal_n, user)
        rv$result <- with_success(batch_run(
          rv$configuration, rv$focal, control_run$result()))
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      vars <- rv$configuration$vars
      y2_model <- get_inputs(input, vars$id_graph_option, vars$name)
      options <- list(type = input$plot_type,
                      slice_time = input$plot_slice_time,
                      extreme_type = input$plot_extreme_type)
      batch_plot(rv$result$value, locked$result()$value,
                 y2_model, input$logscale_y, options)
    }
  })

  get_state <- function() {
    if (is.null(rv$configuration)) {
      return(NULL)
    }
    vars <- rv$configuration$vars
    focal <- rv$result$value$focal
    control_graph <-
      list(option = get_inputs(input, vars$id_graph_option, vars$name),
           logscale_y = input$logscale_y)
    control_focal <- list(name = input$focal_n,
                          pct = input$focal_pct,
                          n = input$focal_n)
    list(user = user,
         focal = focal,
         control_focal = control_focal,
         control_graph = control_graph,
         parameters = parameters$get_state(),
         locked = locked$get_state())
  }

  set_state <- function(state) {
    if (is.null(state)) {
      return()
    }
    locked$set_state(state$locked)
    rv$configuration <- common_model_data_configuration(
      model(), data(), link())
    parameters$set_state(state$parameters)
    rv$result <- with_success(batch_run(rv$configuration, state$focal))
    output$control_graph <- shiny::renderUI(
      batch_control_graph(rv$configuration, session$ns, state$control_graph))
    output$control_focal <- shiny::renderUI(
      batch_control_focal(rv$configuration, session$ns, state$control_focal))
  }

  list(get_state = get_state,
       set_state = set_state)
}


batch_control_focal <- function(configuration, ns, restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }

  pct <- restore$pct %||% 10
  n <- restore$n %||% 10
  name <- restore$name %||% configuration$pars$name[[1]]

  mod_model_control_section(
    "Vary parameter",
    horizontal_form_group(
      "Parameter to vary",
      raw_select_input(
        ns("focal_name"), configuration$pars$name, selected = name)),
    simple_numeric_input("Variation (%)", ns("focal_pct"), pct),
    simple_numeric_input("Number of runs", ns("focal_n"), n),
    shiny::textOutput(ns("status_focal")),
    ns = ns)
}


batch_control_plot <- function(configuration, ns, restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }

  types <- c("Trace over time" = "trace",
             "Value at a single time" = "slice",
             "Value at its min/max" = "extreme")

  mod_model_control_section(
    "Plot options",
    simple_select_input("Type of plot", ns("plot_type"), types),
    shiny::uiOutput(ns("control_plot_options")),
    ns = ns)
}


batch_control_plot_options <- function(configuration, type, ns,
                                       restore = NULL) {
  switch(
    type,
    slice = simple_numeric_input(
      "Time to use (default is last)", ns("plot_slice_time"), NA),
    extreme = simple_select_input(
      "Extreme to use", ns("plot_extreme_type"), c("max", "min")),
    NULL)
}


batch_focal <- function(name, pct, n, user) {
  if (is_missing(pct) || is_missing(name) || is_missing(n)) {
    return(NULL)
  }
  value <- user[[name]]
  if (is_missing(value)) {
    return(NULL)
  }
  dy <- abs(pct / 100 * value)
  from <- value - dy
  to <- value + dy
  list(base = user, name = name, value = value, n = n, from = from, to = to)
}


batch_run <- function(configuration, focal, run_options) {
  if (is.null(focal)) {
    return(NULL)
  }
  name <- focal$name
  n <- constrain(focal$n, 2, 20)
  value <- seq(focal$from, focal$to, length.out = n)
  pars <- configuration$pars
  i <- match(name, pars$name)
  value <- value[value >= pars$min[[i]] & value <= pars$max[[i]]]

  user <- focal$base
  f <- function(p) {
    user[[name]] <- p
    vis_run(configuration, user, run_options)
  }

  ## First, the central runs as our base set:
  central <- vis_run(configuration, user, run_options)

  ## Output types we'll work with:
  types <- setdiff(names(drop_null(central$simulation)), "combined")

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
  switch(
    options$type,
    trace = batch_plot_series_trace,
    slice = batch_plot_series_slice,
    extreme = batch_plot_series_extreme)(result, locked, y2_model, options)
}


batch_plot_series_slice <- function(result, locked, y2_model, options) {
  cfg <- result$configuration
  cols <- cfg$cols
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
  plot_plotly_series_bulk(x, y, cols, points = FALSE, y2 = y2_model)
}


batch_plot_series_extreme <- function(result, locked, y2_model, options) {
  if (is_missing(options$extreme_type)) {
    return(NULL)
  }

  cfg <- result$configuration
  cols <- cfg$cols
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
  plot_plotly_series_bulk(x, y, cols, points = FALSE, y2 = y2_model)
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


batch_plot <- function(result, locked, y2_model, logscale_y, options) {
  xlab <- switch(
    options$type,
    trace = "Time",
    slice = result$focal$name,
    extreme = result$focal$name)
  plot_plotly(batch_plot_series(result, locked, y2_model, options),
              logscale_y, xlab)
}


batch_control_graph <- function(configuration, ns, restore = NULL) {
  title <- "Plot on second y axis"
  common_control_graph(configuration, ns, title, restore)
}


batch_status_focal <- function(focal) {
  if (!is.null(focal)) {
    sprintf("%s - %s - %s",
            focal$from, focal$value, focal$to)
  }
}


batch_status <- function(result) {
  if (!is.null(result$error)) {
    simple_panel("danger", "Error running model", result$error)
  }
}
