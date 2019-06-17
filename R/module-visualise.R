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
          ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
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
        shiny::uiOutput(ns("control_graph")))))
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

  shiny::observe({
    rv$configuration <- vis_configuration(model(), data(), configure()$link)
  })

  output$control_parameters <- shiny::renderUI({
    common_control_parameters(rv$configuration$pars, session$ns)
  })

  output$control_graph <- shiny::renderUI({
    common_control_graph(rv$configuration, session$ns,
                         "Plot on second y axis", "id_y2")
  })

  shiny::observeEvent(
    input$go_button, {
      pars <- rv$configuration$pars
      user <- get_inputs(input, pars$id_value, pars$name)
      rv$result <- vis_run(rv$configuration, user)
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
        rv$result <- vis_run(rv$configuration, user)
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result)) {
      y2_model <- get_inputs(input, rv$configuration$vars$id_y2,
                       rv$configuration$vars$name)
      vis_plot(rv$result, y2_model, input$logscale_y)
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      common_download_filename(input$download_filename, input$download_type,
                               "visualise")
    },
    content = function(filename) {
      common_download_data(filename, rv$result$simulation, input$download_type)
    })

  ## TODO: save/load state
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


vis_configuration <- function(model, data, link) {
  if (!isTRUE(model$result$success) || !isTRUE(data$configured)) {
    return(NULL)
  }
  ## Configure how we'll interact with paramters, to pull them from
  ## the ui
  pars <- model$result$info$pars
  pars$value <- vnapply(pars$default_value, function(x) x %||% NA_real_)
  pars$id_value <- sprintf("par_value_%s", pars$name)

  vars <- model$result$info$vars
  vars$id_y2 <- sprintf("var_y2_%s", vars$name)

  cols <- odin_colours(vars$name, data$name_vars, link)

  list(data = data, model = model, link = link,
       pars = pars, vars = vars, cols = cols)
}


vis_run <- function(configuration, user) {
  if (is.null(configuration)) {
    return(NULL)
  }

  data <- configuration$data
  model <- configuration$model

  name_time <- data$name_time
  mod <- model$result$model(user = user)

  ## 1. Result aligned with the data
  t <- data$data[[name_time]]
  t_after_zero <- t[[1]] > 0
  result_data <- mod$run(if (t_after_zero) c(0, t) else t)
  if (t_after_zero) {
    result_data <- result_data[-1, , drop = FALSE]
  }

  ## 2. Result combined with the data
  result_combined <- cbind(result_data, data$data)

  ## 3. Result smoothly computed
  result_smooth <- mod$run(seq(0, max(t), length.out = 501))

  list(configuration = configuration,
       simulation = list(data = result_data,
                         combined = result_combined,
                         smooth = result_smooth,
                         user = list_to_df(user)))
}


vis_plot_series <- function(result, y2_model) {
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, result$configuration$link)
  cols <- result$configuration$cols

  xy <- result$simulation$smooth
  series_model <- plot_plotly_series_bulk(
    xy[, 1], xy[, names(cols$model), drop = FALSE], cols$model, FALSE, y2$model)

  data <- cfg$data$data
  data_time <- data[[cfg$data$name_time]]
  series_data <- plot_plotly_series_bulk(
    data_time, data[names(cols$data)], cols$data, TRUE, y2$data)

  c(series_model, series_data)
}


vis_plot <- function(result, y2_model, logscale_y) {
  plot_plotly(vis_plot_series(result, y2_model), logscale_y)
}
