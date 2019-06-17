mod_fit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Fit a model"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          shiny::uiOutput(ns("status_link")),
          ##
          shiny::uiOutput(ns("control_target")),
          shiny::uiOutput(ns("control_parameters")),
          ##
          shiny::actionButton(ns("reset_button"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("fit"), "Fit model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::uiOutput(ns("control_graph")),
        shiny::textOutput(ns("goodness_of_fit")),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_fit")))))))
}


mod_fit_server <- function(input, output, session, data, model, configure) {
  rv <- shiny::reactiveValues()

  output$status_data <- shiny::renderUI({
    show_module_status_if_not_ok(data()$status)
  })

  output$status_model <- shiny::renderUI({
    show_module_status_if_not_ok(model()$status)
  })

  output$status_link <- shiny::renderUI({
    show_module_status_if_not_ok(configure()$status)
  })

  output$status_fit <- shiny::renderUI({
    rv$fit$status$ui
  })

  shiny::observe({
    rv$configuration <- fit_configuration(model(), data(), configure())
  })

  output$control_parameters <- shiny::renderUI({
    fit_control_parameters(rv$configuration$pars, session$ns)
  })

  output$control_target <- shiny::renderUI({
    ## TODO: it would be nice to depend on input$target so that we can
    ## persist the previous choice but getting that to work without a
    ## circular dependency is hard and isolate over all this
    ## expression does not work well.
    fit_control_target(rv$configuration, session$ns)
  })

  ## TODO: this should *only* include
  output$control_graph <- shiny::renderUI({
    common_control_graph(
      rv$configuration, session$ns, "Plot on second y axis")
  })

  shiny::observeEvent(
    input$fit, {
      pars <- rv$configuration$pars
      user <- get_inputs(input, pars$id_value, pars$name)
      vary <- get_inputs(input, pars$id_vary, pars$name)
      rv$fit <- shiny::withProgress(
        message = "model fit in progress",
        detail = "this may take a while",
        value = 0,
        fit_run(rv$configuration, input$target, user, vary))
      if (rv$fit$success) {
        set_inputs(session, pars$id_value, rv$fit$result$user)
        rv$result <- vis_run(rv$configuration, rv$fit$result$user)
      }
    })

  shiny::observe({
    target <- input$target
    pars <- rv$configuration$pars
    user <- get_inputs(input, pars$id_value, pars$name)
    if (!is.null(target) && !any(is.na(list_to_numeric(user)))) {
      compare <- fit_make_compare(rv$configuration, target)
      rv$result <- vis_run(rv$configuration, user)
      rv$goodness_of_fit <- compare(rv$result$simulation$data)
    } else {
      rv$result <- NULL
      rv$goodness_of_fit <- NULL
    }
  })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result)) {
      ## TODO: I wonder if we can strip this down earlier?
      vars <- rv$configuration$vars[rv$configuration$vars$include, ]
      y2_model <- get_inputs(input, vars$id_graph_option, vars$name)
      fit_plot(rv$result, input$target, y2_model, input$logscale_y)
    }
  })

  output$goodness_of_fit <- shiny::renderText({
    if (!is.null(rv$goodness_of_fit)) {
      sprintf("Sum of squares: %s",
              format(rv$goodness_of_fit, big.mark = ","))
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      common_download_filename(input$download_filename, input$download_type,
                               "fit")
    },
    content = function(filename) {
      common_download_data(filename, rv$result$simulation, input$download_type)
    })

  get_state <- function() {
    browser()
  }

  set_state <- function(state) {
    ## browser()
  }

  shiny::outputOptions(output, "control_parameters", suspendWhenHidden = FALSE)

  list(result = shiny::reactive(c(rv$fit$result, list(status = rv$fit$status))),
       user = shiny::reactive(rv$fit$result$user),
       get_state = get_state,
       set_state = set_state)
}


fit_configuration <- function(model, data, link) {
  configuration <- common_model_data_configuration(model, data, link$link)
  if (!is.null(configuration$pars)) {
    configuration$pars$id_vary <-
      sprintf("par_vary_%s",configuration$pars$name)
    configuration$pars$vary <- FALSE
    ## TODO: remove this cheat later
    configuration$pars$vary <-
      configuration$pars$name %in% c("I0", "cfr", "R0_before", "R0_after")

    configuration$vars$include <-
      configuration$vars$name %in% list_to_character(link$link)
    ## TODO: this would be improved by passing the whole link through
    ## above
    configuration$link_label <- link$label
  }
  configuration
}


fit_control_target <- function(configuration, ns) {
  if (is.null(configuration$link)) {
    return(NULL)
  }
  choices <- set_names(names(configuration$link), configuration$link_label)
  selected <- NA
  mod_model_control_section(
    "Optimisation",
    horizontal_form_group(
      "Target to fit",
      raw_select_input(ns("target"), choices, selected),
      label_width = 4),
    ns = ns)
}


fit_control_parameters <- function(pars, ns) {
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
  mod_model_control_section(
    "Model parameters",
    Map(f, pars$name, ns(pars$id_value), pars$value,
        ns(pars$id_vary), pars$vary),
    ns = ns)
}


fit_result <- function(success, result, message) {
  class <- if (success) "success" else "danger"
  list(success = success,
       result = result,
       message = message,
       status = module_status(class, message, NULL))
}


fit_run <- function(configuration, target, user, vary) {
  user <- list_to_numeric(user, TRUE)
  if (any(is.na(user))) {
    return(fit_result(
      success = FALSE,
      result = NULL,
      message = sprintf("Starting parameter value needed for %s",
                        names(user)[is.na(user)])))
  }

  vary <- names(vary)[list_to_logical(vary)]
  if (length(vary) == 0L) {
    return(fit_result(
      success = FALSE,
      result = NULL,
      message = "Select at least one parameter to vary"))
  }

  pars <- configuration$model$result$info$pars
  i <- match(vary, pars$name)
  lower <- pars$min[i]
  upper <- pars$max[i]

  objective <- fit_make_objective(configuration, target, user, vary)
  result <- do_fit(user[vary], objective, lower, upper,
                   tolerance = 1e-6, method = "nmkb")
  user[vary] <- result$par
  result$user <- as.list(user)
  result$target <- target

  fit_result(
    success = TRUE,
    result = result,
    message = sprintf("Ran optimisation in %2.2f s", result$elapsed))
}


fit_make_objective <- function(configuration, target, user, vary) {
  mod <- configuration$model$result$model(user = as.list(user))
  time <- configuration$data$data[[configuration$data$name_time]]
  compare <- fit_make_compare(configuration, target)
  force(vary)
  function(p) {
    mod$set_user(user = set_names(as.list(p), vary))
    y <- mod$run(c(0, time))[-1, , drop = FALSE]
    compare(y)
  }
}


fit_make_compare <- function(configuration, target, compare = compare_sse) {
  real <- configuration$data$data[[target]]
  target_model <- configuration$link[[target]]
  compare <- match.fun(compare)
  function(modelled) {
    compare(modelled[, target_model], real)
  }
}


fit_plot_series <- function(result, target, y2_model) {
  cfg <- result$configuration
  y2 <- odin_y2(y2_model, cfg$data$name_vars, result$configuration$link)
  cols <- result$configuration$cols
  target_model <- result$configuration$link[[target]]

  model_vars <- cfg$vars$name[cfg$vars$include]

  xy <- result$simulation$smooth
  dash <- set_names(ifelse(model_vars == target_model, "solid", "dash"),
                    model_vars)
  series_model <- plot_plotly_series_bulk(
    xy[, 1], xy[, model_vars, drop = FALSE], cols$model, FALSE, y2$model,
    dash = dash)

  data <- cfg$data$data
  data_time <- data[[cfg$data$name_time]]
  data_vars <- cfg$data$name_vars
  symbol <- set_names(ifelse(data_vars == target, "circle", "circle-open"),
                      data_vars)
  series_data <- plot_plotly_series_bulk(
    data_time, data[data_vars], cols$data, TRUE, y2$data, symbol = symbol)

  c(series_model, series_data)
}


fit_plot <- function(result, target, y2_model, logscale_y) {
  plot_plotly(fit_plot_series(result, target, y2_model), logscale_y)
}
