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
          ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
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
        shiny::uiOutput(ns("control_graph")))))
}


mod_batch_server <- function(input, output, session, model, data, configure,
                             import = NULL) {
  rv <- shiny::reactiveValues()

  output$status_data <- shiny::renderUI({
    show_module_status_if_not_ok(data()$status)
  })

  output$status_model <- shiny::renderUI({
    show_module_status_if_not_ok(model()$status)
  })

  output$status_focal <- shiny::renderText({
    batch_status_focal(rv$focal)
  })

  shiny::observe({
    rv$configuration <- common_model_data_configuration(
      model(), data(), configure()$link)
  })

  output$control_parameters <- shiny::renderUI({
    common_control_parameters(rv$configuration$pars, session$ns)
  })

  output$control_focal <- shiny::renderUI({
    batch_control_focal(rv$configuration, session$ns)
  })

  output$control_graph <- shiny::renderUI({
    common_control_graph(
      rv$configuration, session$ns, "Display series in plot")
  })

  shiny::observeEvent(
    input$go_button, {
      rv$result <- batch_run(rv$configuration, rv$focal)
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
        rv$result <- batch_run(rv$configuration, rv$focal)
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result)) {
      vars <- rv$configuration$vars
      include <- get_inputs(input, vars$id_graph_option, vars$name)
      batch_plot(rv$result, include, input$logscale_y)
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      common_download_filename(input$download_filename, input$download_type,
                               "batch")
    },
    content = function(filename) {
      common_download_data(filename, rv$result$simulation, input$download_type)
    })

  ## TODO: save/load state
}


batch_control_focal <- function(configuration, ns) {
  if (is.null(configuration)) {
    return(NULL)
  }
  mod_model_control_section(
    "Vary parameter",
    horizontal_form_group(
      "Parameter to vary",
      raw_select_input(
        ns("focal_name"), configuration$pars$name, selected = NA)),
    simple_numeric_input("Variation (%)", ns("focal_pct"), 10),
    simple_numeric_input("Number of runs", ns("focal_n"), 10),
    shiny::textOutput(ns("status_focal")),
    ns = ns)
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
       simulation = simulation)
}


batch_plot_series <- function(result, include) {
  cfg <- result$configuration
  cols <- cfg$cols
  include <- names(include)[list_to_logical(include)]

  ## TODO: remove cheat:
  if (length(include) == 0L) {
    include <- intersect(c("weekly_onset", "weekly_death_h"),
                         result$configuration$vars$name)
  }

  if (length(include) == 0L) {
    return(NULL)
  }

  xy <- result$simulation$central$simulation$smooth
  series_central <- plot_plotly_series_bulk(
    xy[, 1], xy[, include, drop = FALSE], cols$model, FALSE, FALSE,
    legendgroup = set_names(include, include))

  plot_plotly(series_central)

  f <- function(nm) {
    t <- result$simulation$smooth[, 1]
    y <- lapply(result$simulation$batch, function(x) x$simulation$smooth[, nm])
    m <- matrix(unlist(y), length(y[[1]]), length(y))
    colnames(m) <- sprintf("%s (%s = %s)", nm, cfg$focal$name, cfg$focal$value)
    col <- set_names(rep(cols$model[[nm]], ncol(m)), colnames(m))
    plot_plotly_series_bulk(t, m, col, FALSE, FALSE,
                            legendgroup = nm, showlegend = FALSE, width = 1)
  }
  series_batch <- unlist(lapply(include, f), FALSE, FALSE)

  data <- cfg$data$data
  data_time <- data[[cfg$data$name_time]]
  series_data <- plot_plotly_series_bulk(
    data_time, data[names(cols$data)], cols$data, TRUE, FALSE)

  c(series_batch, series_central, series_data)
}


batch_plot <- function(result, include, logscale_y) {
  plot_plotly(batch_plot_series(result, include), logscale_y)
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


batch_status_focal <- function(focal) {
  if (!is.null(focal)) {
    sprintf("%s - %s - %s",
            focal$from, focal$value, focal$to)
  }
}
