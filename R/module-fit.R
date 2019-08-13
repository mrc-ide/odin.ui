mod_fit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Fit a model"),
    shiny::sidebarLayout(
      odin_sidebar(
        run = shiny::actionButton(ns("fit"), "Fit model",
                                  shiny::icon("play"), class = "btn-blue"),
        reset = ns("reset"),
        auto_run = NULL,
        controls = shiny::tagList(
          shiny::uiOutput(ns("control_target")),
          mod_parameters_ui(ns("parameters")),
          mod_lock_ui(ns("lock"))),
        status = shiny::tagList(
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          shiny::uiOutput(ns("status_link")))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::div(
          class = "pull-right",
          mod_download_ui(ns("download")),
          mod_control_graph_ui(ns("control_graph"))),
        shiny::textOutput(ns("goodness_of_fit")),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_run")))),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_fit")))),
        mod_model_code_ui(ns("code")))))
}


mod_fit_server <- function(input, output, session, data, model, link) {
  rv <- shiny::reactiveValues()

  parameters <- shiny::callModule(
    mod_parameters_server, "parameters",
    shiny::reactive(rv$configuration$pars), with_option = TRUE)
  control_graph <- shiny::callModule(
    mod_control_graph_server, "control_graph",
    shiny::reactive(rv$configuration))
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
                        locked = locked, download = download)

  output$status_data <- shiny::renderUI({
    show_module_status_if_not_ok(data()$status)
  })

  output$status_model <- shiny::renderUI({
    show_module_status_if_not_ok(model()$status)
  })

  output$status_link <- shiny::renderUI({
    show_module_status_if_not_ok(link()$status)
  })

  output$status_fit <- shiny::renderUI({
    rv$status$ui
  })

  output$status_run <- shiny::renderUI({
    vis_status(rv$result)
  })

  shiny::observe({
    rv$configuration <- fit_configuration(model(), data(), link())
  })

  output$control_target <- shiny::renderUI({
    ## TODO: it would be nice to depend on input$target so that we can
    ## persist the previous choice but getting that to work without a
    ## circular dependency is hard and isolate over all this
    ## expression does not work well.
    fit_control_target(rv$configuration$link, session$ns)
  })

  shiny::observeEvent(
    input$fit, {
      user <- parameters$result()
      rv$fit <- shiny::withProgress(
        message = "model fit in progress",
        detail = "this may take a while",
        value = 0,
        fit_run(rv$configuration, input$target, user, attr(user, "option")))
      if (rv$fit$success) {
        parameters$set(rv$fit$value$user, notify = FALSE)
      }
    })

  shiny::observeEvent(
    input$reset, {
      rv$fit <- NULL
      modules$reset()
      output$control_target <- shiny::renderUI(
        fit_control_target(rv$configuration$link, session$ns))
    })

  shiny::observe({
    rv$status <- fit_status(rv$fit)
  })

  shiny::observe({
    rv$result <- with_success(vis_run(
      rv$configuration, parameters$result(), control_run_default()))
  })

  shiny::observe({
    rv$goodness_of_fit <- fit_goodness_of_fit(rv$result, input$target)
  })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value) && !is.null(input$target)) {
      fit_plot(rv$result$value, locked$result()$value,
               input$target, control_graph$result())
    }
  })

  output$goodness_of_fit <- shiny::renderText({
    if (!is.null(rv$goodness_of_fit)) {
      sprintf("Sum of squares: %s",
              format(rv$goodness_of_fit, big.mark = ","))
    }
  })

  get_state <- function() {
    if (is.null(rv$configuration) || is.null(rv$fit)) {
      return(NULL)
    }
    list(fit = rv$fit,
         control_target = input$target,
         modules = modules$get_state())
  }

  set_state <- function(state) {
    if (is.null(state)) {
      return()
    }
    rv$configuration <- fit_configuration(model(), data(), link())
    output$control_target <- shiny::renderUI(fit_control_target(
      rv$configuration$link, session$ns, state$control_target))
    rv$fit <- state$fit
    modules$set_state(state$modules)
  }

  list(result = shiny::reactive(add_status(rv$fit$value, rv$status)),
       user = shiny::reactive(rv$fit$value$user),
       get_state = get_state,
       set_state = set_state)
}


fit_configuration <- function(model, data, link) {
  configuration <- common_model_data_configuration(model, data, link)
  if (!is.null(configuration$pars)) {
    ## Additional UI elements to indicate if the parameters should vary:
    configuration$pars$id_vary <-
      sprintf("par_vary_%s", configuration$pars$name)
    ## Information about the default vary state (might be dropped later?)
    configuration$pars$vary <- FALSE
    configuration$vars$include <-
      configuration$vars$name %in% list_to_character(link$map)
  }
  configuration
}


## TODO: add to this:
## * compare function (if supported - Anne could use likelihood here)
## * tolerance
## * algorithm (subplex/etc)
fit_control_target <- function(link, ns, restore = NULL) {
  if (is.null(link)) {
    return(NULL)
  }
  choices <- set_names(names(link$map), link$label)
  selected <- restore %||% NA
  odin_control_section(
    "Optimisation",
    horizontal_form_group(
      "Target to fit",
      raw_select_input(ns("target"), choices, selected),
      label_width = 4),
    ns = ns)
}


fit_control_parameters <- function(pars, ns, restore = NULL) {
  if (is.null(pars)) {
    return(NULL)
  }
  f <- function(name, id_value, value, id_vary, vary) {
    shiny::fluidRow(
      shiny::column(
        10,
        simple_numeric_input(name, id_value, value)),
      shiny::column(
        2, shiny::checkboxInput(id_vary, "", vary)))
  }
  value <- restore$value %||% pars$value
  vary <- restore$vary %||% pars$vary
  odin_control_section(
    "Model parameters",
    Map(f, pars$name, ns(pars$id_value), value, ns(pars$id_vary), vary),
    ns = ns)
}


fit_run <- function(configuration, target, user, vary, method = "subplex") {
  data_t <- configuration$data$data[[configuration$data$name_time]]
  data_y <- configuration$data$data[[target]]
  model <- configuration$model$model

  name_model_y <- configuration$link$map[[target]]
  user <- list_to_numeric(user, TRUE)
  vary <- names(vary)[list_to_logical(vary)]

  i <- match(vary, configuration$pars$name)
  lower <- configuration$pars$min[i]
  upper <- configuration$pars$max[i]

  compare <- compare_sse
  tolerance <- 1e-6

  with_success(
    odin_fit_model(data_t, data_y, model, name_model_y, user, vary,
                   lower, upper, method = method, compare = compare,
                   tolerance = tolerance))
}


fit_status <- function(result) {
  if (is.null(result$success)) {
    return(NULL)
  }
  if (result$success) {
    class <- "success"
    title <- sprintf("Fit model in %2.2f s", result$value$elapsed)
    body <- NULL
  } else {
    class <- "danger"
    title <- "Error fitting model to data"
    body <- result$error
  }
  module_status(class, title, body)
}


fit_goodness_of_fit <- function(result, target, compare = compare_sse) {
  if (is.null(result$value) || is.null(target)) {
    return(NULL)
  }
  cfg <- result$value$configuration
  y_data <- cfg$data$data[[target]]
  name_y_model <- cfg$link$map[[target]]
  y_model <- result$value$simulation$data[, name_y_model]
  compare(y_data, y_model)
}


fit_plot_series <- function(result, locked, target, y2_model) {
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, cfg$link$map)
  target_model <- cfg$link$map[[target]]

  c(fit_plot_series_locked(result, locked, target_model, y2),
    fit_plot_series_focal(result, target_model, y2),
    fit_plot_series_data(result, target, y2))
}


fit_plot_series_data <- function(result, target, y2) {
  cfg <- result$configuration
  data <- cfg$data$data
  data_time <- data[[cfg$data$name_time]]
  data_vars <- cfg$data$name_vars
  symbol <- set_names(ifelse(data_vars == target, "circle", "circle-open"),
                      data_vars)
  plot_plotly_series_bulk(
    data_time, data[data_vars], cfg$cols$data, TRUE, y2$data, symbol = symbol)
}


fit_plot_series_focal <- function(result, target, y2) {
  cfg <- result$configuration

  model_vars <- cfg$vars$name[cfg$vars$include]

  xy <- result$simulation$smooth
  dash <- set_names(ifelse(model_vars == target, "solid", "dash"), model_vars)
  plot_plotly_series_bulk(
    xy[, 1], xy[, model_vars, drop = FALSE], cfg$cols$model, FALSE, y2$model,
    dash = dash)
}


fit_plot_series_locked <- function(result, locked, target, y2) {
  if (is.null(locked)) {
    return(NULL)
  }
  if (identical(result, locked)) {
    return(NULL)
  }

  cfg <- result$configuration
  model_vars <- intersect(locked$configuration$vars$name,
                          cfg$vars$name[cfg$vars$include])
  model_data <- locked$simulation$smooth
  plot_plotly_series_bulk(
    model_data[, 1], model_data[, model_vars, drop = FALSE],
    cfg$cols$model, FALSE, y2$model, dash = "dot", width = 1,
    showlegend = FALSE, legendgroup = TRUE)
}


fit_plot <- function(result, locked, target, control) {
  y2 <- control$y2
  logscale <- control$logscale
  plot_plotly(fit_plot_series(result, locked, target, y2), logscale)
}
