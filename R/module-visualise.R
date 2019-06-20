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
          shiny::uiOutput(ns("control_parameters")),
          ##
          shiny::uiOutput(ns("import_button"), inline = TRUE),
          shiny::actionButton(ns("reset_button"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("go_button"), "Run model",
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
        shiny::uiOutput(ns("control_graph")),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_vis")))))))
}


## Basic flow is
##   {data, model, configure} => configuration
##   configuration => control interface
##   {configuration, control interface} => result
##   {result, control inteface} => plot
mod_vis_server <- function(input, output, session, data, model, configure,
                           import = NULL) {
  rv <- shiny::reactiveValues()

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
      model(), data(), configure())
  })

  output$control_parameters <- shiny::renderUI({
    common_control_parameters(rv$configuration$pars, session$ns)
  })

  output$control_graph <- shiny::renderUI({
    vis_control_graph(rv$configuration, session$ns)
  })

  shiny::observeEvent(
    input$go_button, {
      pars <- rv$configuration$pars
      user <- get_inputs(input, pars$id_value, pars$name)
      rv$result <- with_success(vis_run(rv$configuration, user))
    })

  output$import_button <- shiny::renderUI({
    if (!is.null(import) && !is.null(import())) {
      shiny::actionButton(session$ns("import"), "Import",
                          shiny::icon("calculator"))
    }
  })

  shiny::observeEvent(
    input$import, {
      user <- import()
      pars <- rv$configuration$pars
      if (identical(names(user), pars$name)) {
        set_inputs(session, pars$id_value, user)
        rv$result <- with_success(vis_run(rv$configuration, user))
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      vars <- rv$configuration$vars
      y2_model <- get_inputs(input, vars$id_graph_option, vars$name)
      vis_plot(rv$result$value, y2_model, input$logscale_y)
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      common_download_filename(input$download_filename, input$download_type,
                               "visualise")
    },
    content = function(filename) {
      common_download_data(filename, rv$result$value$simulation,
                           input$download_type)
    })

  get_state <- function() {
    if (is.null(rv$configuration)) {
      return(NULL)
    }
    pars <- rv$configuration$pars
    vars <- rv$configuration$vars
    user_display <- get_inputs(input, pars$id_value, pars$name)
    user_result <- df_to_list(rv$result$value$simulation$user)
    control_graph <-
      list(option = get_inputs(input, vars$id_graph_option, vars$name),
           logscale_y = input$logscale_y)
    list(user_display = user_display,
         user_result = user_result,
         control_graph = control_graph)
  }

  set_state <- function(state) {
    if (is.null(state)) {
      return()
    }
    shiny::isolate({
      rv$configuration <- common_model_data_configuration(
        model(), data(), configure())
      rv$result <- with_success(vis_run(rv$configuration, state$user_result))
      output$control_parameters <- shiny::renderUI(
        common_control_parameters(rv$configuration$pars, session$ns,
                                  state$user_display))
      output$control_graph <- shiny::renderUI(
        vis_control_graph(rv$configuration, session$ns, state$control_graph))
    })
  }

  list(get_state = get_state,
       set_state = set_state)
}


vis_control_parameters <- function(configuration, ns) {
  if (is.null(configuration)) {
    return(NULL)
  }
  pars <- configuration$pars
  mod_model_control_section(
    "Model parameters",
    Map(simple_numeric_input, pars$name, ns(pars$id_value), pars$value),
    ns = ns)
}


vis_run <- function(configuration, user) {
  if (is.null(configuration)) {
    return(NULL)
  }

  data <- configuration$data
  model <- configuration$model

  name_time <- data$name_time
  mod <- model$model(user = user)

  ## 1. Result aligned with the data
  t_data <- data$data[[name_time]]
  t_after_zero <- t_data[[1]] > 0
  t_smooth <- seq(min(t_data), max(t_data), length.out = 501)

  result_data <- mod$run(c(if (t_after_zero) 0, t_data))
  result_smooth <- mod$run(c(if (t_after_zero) 0, t_smooth))

  if (t_after_zero) {
    result_data <- result_data[-1, , drop = FALSE]
    result_smooth <- result_smooth[-1, , drop = FALSE]
  }

  ## Relevant only for the fitting:
  if ("include" %in% names(configuration$vars)) {
    i <- c(1, which(configuration$vars$include) + 1)
    result_data <- result_data[, i, drop = FALSE]
    result_smooth <- result_smooth[, i, drop = FALSE]
  }

  ## 2. Result combined with the data
  result_combined <- cbind(result_data, data$data)

  list(configuration = configuration,
       simulation = list(data = result_data,
                         combined = result_combined,
                         smooth = result_smooth,
                         user = list_to_df(user)))
}


vis_plot_series <- function(result, y2_model) {
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, result$configuration$link$map)
  cols <- result$configuration$cols

  model_vars <- cfg$model$info$vars$name
  ## TODO: don't use names(cols) here and in the data section
  model_data <- result$simulation$smooth
  series_model <- plot_plotly_series_bulk(
    model_data[, 1], model_data[, model_vars, drop = FALSE],
    cols$model, FALSE, y2$model)

  data_data <- cfg$data$data
  data_time <- cfg$data$name_time
  data_vars <- cfg$data$name_vars
  series_data <- plot_plotly_series_bulk(
    data_data[[data_time]], data_data[data_vars], cols$data, TRUE, y2$data)

  c(series_model, series_data)
}


vis_plot <- function(result, y2_model, logscale_y) {
  plot_plotly(vis_plot_series(result, y2_model), logscale_y)
}


vis_control_graph <- function(configuration, ns, restore = NULL) {
  common_control_graph(configuration, ns, "Plot on second y axis", restore)
}


vis_status <- function(result) {
  if (!is.null(result$error)) {
    simple_panel("danger", "Error running model", result$error)
  }
}
