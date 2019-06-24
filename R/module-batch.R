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
          shiny::uiOutput(ns("control_parameters")),
          shiny::uiOutput(ns("control_focal")),
          mod_lock_ui(ns("lock")),
          shiny::hr(),
          shiny::uiOutput(ns("import_button"), inline = TRUE),
          shiny::actionButton(ns("reset_button"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("go_button"), "Run model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::uiOutput(ns("control_graph")),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_batch")))))))
}


mod_batch_server <- function(input, output, session, model, data, link,
                             import = NULL) {
  rv <- shiny::reactiveValues()

  set_result <- function(result) {
    pars <- rv$configuration$pars
    set_inputs(session, pars$id_value, result$value$simulation$user$value)
    rv$result <- result
  }
  locked <- shiny::callModule(
    mod_lock_server, "lock",
    shiny::reactive(!is.null(rv$configuration)), shiny::reactive(rv$result),
    set_result)

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
    vars <- rv$configuration$vars
    include <- get_inputs(input, vars$id_graph_option, vars$name)
    batch_status(rv$result, include)
  })

  shiny::observe({
    rv$configuration <- common_model_data_configuration(
      model(), data(), link())
  })

  output$control_parameters <- shiny::renderUI({
    common_control_parameters(rv$configuration$pars, session$ns)
  })

  output$control_focal <- shiny::renderUI({
    batch_control_focal(rv$configuration, session$ns)
  })

  output$control_graph <- shiny::renderUI({
    batch_control_graph(rv$configuration, session$ns)
  })

  shiny::observeEvent(
    input$go_button, {
      rv$result <- with_success(batch_run(rv$configuration, rv$focal))
    })

  shiny::observe({
    pars <- rv$configuration$pars
    user <- get_inputs(input, pars$id_value, pars$name)
    rv$focal <- batch_focal(
      input$focal_name, input$focal_pct, input$focal_n, user)
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
        rv$focal <- batch_focal(
          input$focal_name, input$focal_pct, input$focal_n, user)
        rv$result <- with_success(batch_run(rv$configuration, rv$focal))
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      vars <- rv$configuration$vars
      include <- get_inputs(input, vars$id_graph_option, vars$name)
      batch_plot(rv$result$value, locked$result()$value,
                 include, input$logscale_y)
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      common_download_filename(input$download_filename, input$download_type,
                               "batch")
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
    user <- get_inputs(input, pars$id_value, pars$name)
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
         locked = locked$get_state())
  }

  set_state <- function(state) {
    if (is.null(state)) {
      return()
    }
    locked$set_state(state$locked)
    rv$configuration <- common_model_data_configuration(
      model(), data(), link())
    rv$result <- with_success(batch_run(rv$configuration, state$focal))
    output$control_parameters <- shiny::renderUI(
      common_control_parameters(rv$configuration$pars, session$ns, state$user))
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


batch_run <- function(configuration, focal) {
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
    vis_run(configuration, user)
  }

  ## First, the central runs as our base set:
  central <- vis_run(configuration, user)

  ## Output types we'll work with:
  types <- names(central$simulation)

  ## Then the sensitivity around that
  batch <- lapply(value, f)
  g <- function(type) {
    combine_colwise(lapply(batch, function(x) x$simulation[[type]]))
  }

  ## Organise output that will download cleanly:
  simulation <- set_names(lapply(types, g), types)

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


batch_plot_series <- function(result, locked, include) {
  cfg <- result$configuration
  cols <- cfg$cols
  include <- names(include)[vlapply(include, isTRUE)]
  if (length(include) == 0L) {
    return(NULL)
  }

  c(batch_plot_series_locked(result, locked, include),
    batch_plot_series_focal(result, include),
    batch_plot_series_data(result, inclue))
}


batch_plot_series_focal <- function(result, include) {
  batch_plot_series_modelled(result, include, FALSE)
}


batch_plot_series_locked <- function(result, locked, include) {
  if (is.null(locked)) {
    return(NULL)
  }
  if (identical(result, locked)) {
    return(NULL)
  }

  model_vars <- intersect(locked$configuration$vars$name, include)
  batch_plot_series_modelled(locked, include, TRUE)
}


batch_plot_series_modelled <- function(result, include, locked = FALSE) {
  cfg <- result$configuration
  cols <- cfg$cols

  if (locked) {
    width <- 1
    dash <- "dot"
  } else {
    width <- 2
    dash <- NULL
  }

  xy <- result$simulation$central$simulation$smooth
  series_central <- plot_plotly_series_bulk(
    xy[, 1], xy[, include, drop = FALSE], cols$model,
    points = FALSE, y2 = FALSE, showlegend = !locked,
    legendgroup = set_names(include, include), dash = dash, width = width)

  f <- function(nm) {
    t <- result$simulation$smooth[, 1]
    y <- lapply(result$simulation$batch, function(x) x$simulation$smooth[, nm])
    m <- matrix(unlist(y), length(y[[1]]), length(y))
    colnames(m) <- sprintf("%s (%s = %s)", nm, cfg$focal$name, cfg$focal$value)
    col <- set_names(rep(cols$model[[nm]], ncol(m)), colnames(m))
    plot_plotly_series_bulk(t, m, col, FALSE, FALSE,
                            legendgroup = nm, showlegend = FALSE,
                            width = width / 2, dash = dash)
  }

  series_batch <- unlist(lapply(include, f), FALSE, FALSE)

  c(series_central, series_batch)
}


batch_plot_series_data <- function(result, include) {
  cfg <- result$configuration
  cols <- cfg$cols
  data <- cfg$data$data
  data_time <- data[[cfg$data$name_time]]
  plot_plotly_series_bulk(
    data_time, data[names(cols$data)], cols$data, TRUE, FALSE)
}


batch_plot <- function(result, locked, include, logscale_y) {
  plot_plotly(batch_plot_series(result, locked, include), logscale_y)
}


batch_control_graph <- function(configuration, ns, restore = NULL) {
  common_control_graph(configuration, ns, "Display series in plot", restore)
}


batch_status_focal <- function(focal) {
  if (!is.null(focal)) {
    sprintf("%s - %s - %s",
            focal$from, focal$value, focal$to)
  }
}


batch_status <- function(result, include) {
  if (!is.null(result$error)) {
    simple_panel("danger", "Error running model", result$error)
  } else if (isTRUE(result$success) && !any(vlapply(include, isTRUE))) {
    simple_panel("info", "Select a series to show plot",
                 paste("Drawing these plots can be slow, so start by",
                       "selecting a series by opening the 'Graph settings'",
                       "by clicking the cog icon"))
  }
}
