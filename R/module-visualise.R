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
    shiny::titlePanel("Visualise"),
    ## shiny::p(class = "mt-5"),
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
          mod_lock_ui(ns("lock")),
          shiny::hr(),
          ##
          shiny::uiOutput(ns("import_button"), inline = TRUE),
          shiny::actionButton(ns("reset"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-danger pull-right ml-2"),
          shiny::actionButton(ns("run"), "Run model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"),
          shiny::div(
            class = "form-group pull-right", style = "clear:both;",
            shiny::div(
              class = "col-sm-12",
              raw_checkbox_input(ns("auto_run"), "Auto run", value = FALSE))))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::div(
          class = "pull-right",
          mod_download_ui(ns("download")),
          mod_control_graph_ui(ns("control_graph"))),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_vis")))),
        mod_model_code_ui(ns("code")))))
}


## Basic flow is
##   {data, model, link} => configuration
##   configuration => control interface
##   {configuration, control interface} => result
##   {result, control inteface} => plot
mod_vis_server <- function(input, output, session, data, model, link,
                           import = NULL, run_options = NULL) {
  rv <- shiny::reactiveValues()

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars))
  control_graph <- shiny::callModule(
    mod_control_graph_server, "control_graph",
    shiny::reactive(rv$configuration))
  control_run <- shiny::callModule(
    mod_control_run_server, "control_run", model, run_options)
  code <- shiny::callModule(
    mod_model_code_server, "code", model)

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
      rv$result <- with_success(vis_run(
        rv$configuration, parameters$result(), control_run$result()))
    })

  shiny::observeEvent(
    input$reset, {
      rv$result <- NULL
      modules$reset()
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
        rv$result <- with_success(vis_run(
          rv$configuration, user, control_run$result()))
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      vis_plot(rv$result$value, locked$result()$value, control_graph$result())
    }
  })

  get_state <- function() {
    if (is.null(rv$configuration) || is.null(rv$result)) {
      return(NULL)
    }
    list(user_result = df_to_list(rv$result$value$simulation$user),
         modules = modules$get_state())
  }

  set_state <- function(state) {
    if (is.null(state)) {
      return()
    }
    rv$configuration <- common_model_data_configuration(
      model(), data(), link())
    modules$set_state(state$modules)
    rv$result <- with_success(vis_run(rv$configuration, state$user_result))
  }

  list(get_state = get_state,
       set_state = set_state)
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

  data <- configuration$data
  has_data <- !is.null(data)

  if (run_options$options$control_end_time) {
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
  mod <- model$model(user = user, unused_user_action = "ignore")

  t_smooth <- seq(t_start, t_end, length.out = 501)
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
       simulation = list(data = result_data,
                         combined = result_combined,
                         smooth = result_smooth,
                         user = list_to_df(user)))
}


vis_plot_series <- function(result, locked, y2_model) {
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, cfg$link$map)
  c(vis_plot_series_locked(result, locked, y2),
    vis_plot_series_focal(result, y2))
}


vis_plot_series_focal <- function(result, y2) {
  cfg <- result$configuration
  cols <- cfg$cols
  vars <- cfg$vars[cfg$vars$include, ]

  model_vars <- vars$name
  model_data <- result$simulation$smooth
  series_model <- plot_plotly_series_bulk(
    model_data[, 1], model_data[, model_vars, drop = FALSE],
    cols$model, FALSE, y2$model, legendgroup = TRUE, show = vars$show)

  data_data <- cfg$data$data
  data_time <- cfg$data$name_time
  data_vars <- cfg$data$name_vars
  series_data <- plot_plotly_series_bulk(
    data_data[[data_time]], data_data[data_vars], cols$data, TRUE, y2$data)

  c(series_model, series_data)
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

  model_vars <- intersect(locked$configuration$vars$name, vars$name)
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
