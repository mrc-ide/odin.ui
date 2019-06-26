mod_vis_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Visualise"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          ## TODO: status_configure but tone down warning to info
          shiny::uiOutput(ns("control_parameters")),
          ## mod_lock_ui(ns("lock")),
          shiny::hr(),
          ##
          shiny::actionButton(ns("reset_button"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("run"), "Run model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::uiOutput(ns("control_graph")),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("status_vis")))))))
}


mod_vis_compare_server <- function(input, output, session, model1, model2) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$configuration <- compare_configuration(
      model1(), model2())
  })

  output$control_parameters <- shiny::renderUI({
    compare_control_parameters(
      rv$configuration$pars, rv$configuration$names, session$ns)
  })

  output$control_graph <- shiny::renderUI({
    compare_control_graph(rv$configuration, session$ns)
  })

  shiny::observeEvent(
    input$run, {
      pars <- rv$configuration$pars
      user <- get_inputs(input, pars$id_value, pars$name)
      rv$result <- with_success(compare_vis_run(rv$configuration, user))
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result$value)) {
      vars <- rv$configuration$vars
      y2_model <- get_inputs(input, vars$id_graph_option, vars$name)
      compare_vis_plot(rv$result$value, y2_model, input$logscale_y)
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      compare_download_filename(input$download_filename, input$download_type,
                                rv$configuration$names)
    },
    content = function(filename) {
      compare_download_data(filename, rv$result$value$simulation,
                           input$download_type, rv$configuration$names)
    })
}


compare_union_metadata <- function(a, b, present = "present") {
  ret <- rbind(a, b[!(b$name %in% a$name), , drop = FALSE])
  rownames(ret) <- NULL
  i <- (ret$name %in% a$name) + (ret$name %in% b$name) * 2 + 1
  ret[[present]] <- c(NA, "1_only", "2_only", "both")[i]
  ret
}


compare_configuration <- function(model1, model2) {
  if (!isTRUE(model1$success) || !isTRUE(model2$success)) {
    return(NULL)
  }

  pars <- compare_union_metadata(model1$info$pars, model2$info$pars)
  pars$value <- vnapply(pars$default_value, function(x) x %||% NA_real_)
  pars$id_value <- sprintf("par_value_%s", pars$name)

  vars <- compare_union_metadata(model1$info$vars, model2$info$vars)
  vars$id_graph_option <- sprintf("var_graph_option_%s", vars$name)

  cols <- odin_colours(vars$name, NULL, NULL)

  names <- list(long = c(model1$name, model2$name),
                short = c(model1$name_short, model2$name_short))

  list(data = NULL, model1 = model1, model2 = model2, link = NULL,
       pars = pars, vars = vars, cols = cols, names = names)
}


compare_control_parameters <- function(pars, names, ns, restore = NULL) {
  if (is.null(pars)) {
    return(NULL)
  }
  value <- restore %||% pars$value

  controls <- unname(Map(simple_numeric_input,
                         pars$name, ns(pars$id_value), value))

  f <- function(label, key) {
    i <- pars$present == key
    if (any(i)) {
      c(list(shiny::div(shiny::tags$b(label))), controls[i])
    }
  }

  mod_model_control_section(
    "Model parameters",
    f("Shared", "both"),
    f(paste(names$long[[1]], "only"), "1_only"),
    f(paste(names$long[[2]], "only"), "2_only"),
    ns = ns)
}


compare_vis_run <- function(configuration, user) {
  if (is.null(configuration)) {
    return(NULL)
  }

  err <- vlapply(user, is_missing)
  if (any(err)) {
    stop(sprintf("Missing parameter for %s",
                 paste(names(user)[err], collapse = ", ")))
  }

  mod1 <- configuration$model1$model(user = user, unused_user_action = "ignore")
  mod2 <- configuration$model2$model(user = user, unused_user_action = "ignore")

  t_end <- 30 # TODO - must be configurable, but where?
  t_n <- 501 # TODO - should be configurable

  y1 <- model_run(mod1, t_end, t_n,
                  configuration$model1$info$features$discrete)
  y2 <- model_run(mod2, t_end, t_n,
                  configuration$model2$info$features$discrete)

  list(configuration = configuration,
       simulation = list(model1 = y1, model2 = y2))
}


compare_vis_plot <- function(result, y2, logscale_y) {
  plot_plotly(compare_vis_plot_series(result, y2), logscale_y)
}


compare_vis_plot_series <- function(result, y2) {
  cfg <- result$configuration
  cols <- cfg$cols

  vars1 <- cfg$model1$info$vars$name
  vars2 <- cfg$model2$info$vars$name
  label1 <- sprintf("%s (%s)", vars1, cfg$names$long[[1]])
  label2 <- sprintf("%s (%s)", vars2, cfg$names$long[[2]])
  model1 <- result$simulation$model1
  model2 <- result$simulation$model2

  series1 <- plot_plotly_series_bulk(
    model1[, 1], model1[, vars1, drop = FALSE],
    col = cols$model, points = FALSE, y2 = y2,
    label = label1, legendgroup = vars1)

  series2 <- plot_plotly_series_bulk(
    model2[, 1], model2[, vars2, drop = FALSE],
    col = cols$model, points = FALSE, y2 = y2,
    label = label2, legendgroup = vars2, dash = "dash")

  c(series1, series2)
}


compare_control_graph <- function(configuration, ns, restore = NULL) {
  types <- configuration$names$long
  title <- "Plot on second y axis"
  common_control_graph(configuration, ns, title, types, restore)
}


compare_download_filename <- function(filename, type, names) {
  name <- names$short[[match(type, names$long)]]
  common_download_filename(filename, name, "compare")
}


compare_download_data <- function(filename, simulation, type, names) {
  data <- simulation[[match(type, names$long)]]
  write_csv(data, filename)
}
