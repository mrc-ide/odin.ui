##' @importFrom dde difeq_replicate
mod_model_input <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("odin_control")),
    ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
    shiny::actionButton(ns("reset_button"), "Reset", shiny::icon("refresh"),
                        class = "btn-grey pull-right ml-2"),
    shiny::actionButton(ns("go_button"), "Run model", shiny::icon("play"),
                        class = "btn-blue pull-right"),
    shiny::div(
      class = "form-group pull-right", style = "clear:both;",
      shiny::div(
        class = "col-sm-12",
        raw_checkbox_input(ns("auto_run"), "Auto run", value = FALSE))))
}


## all-in-one module that includes a sidebar interface
mod_model_ui <- function(id, title) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (!is.null(title)) {
      shiny::titlePanel(title)
    },
    shiny::p(class = "mt-5"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(class = "form-horizontal", mod_model_input(id))),
      shiny::mainPanel(
        shiny::div(class = "graph-wrapper",
                    shiny::uiOutput(ns("odin_output"))),
        shiny::uiOutput(ns("graph_settings")))))
}


mod_model_control_graph <- function(graph_data, extra, ns) {
  graph_options <- mod_model_control_graph_options(graph_data, extra, ns)
  shiny::tagList(
  shiny::div(class = "pull-right",
    shiny::div(class = "form-inline mt-5",
      shiny::div(class = "form-group",
        raw_text_input(ns("download_filename"), placeholder = "filename", value = "")),
      shiny::span("."),
    shiny::div(class = "form-group",
      raw_select_input(ns("download_format"), choices = list("csv","rds","json"))),
      shiny::downloadButton(ns("download_button"), "Download", class = "btn-blue")),
  graph_options$tags))
}

mod_model_server <- function(input, output, session,
                             model, default_time, parameters,
                             extra = NULL) {
  ns <- session$ns

  graph_data <- attr(model, "graph_data")()
  extra <- validate_extra(extra, graph_data)

  parameters <- validate_model_parameters(model, parameters)
  model_output <- shiny::reactiveValues(data = NULL)
  control <- mod_model_control(graph_data, default_time, parameters, extra, ns)

  output$odin_control <- shiny::renderUI({
    path_css <- odin_ui_file("css/styles.css")
    times <- input$reset_button
    model_output$data <- NULL
    shiny::div(id = ns(paste0("odin_control_", times)),
                shiny::includeCSS(path_css),
                control$tags)
  })

  output$graph_settings <- shiny::renderUI({
    mod_model_control_graph(graph_data, extra, ns)
  })

  output$odin_output <- shiny::renderUI({
    if (is.null(model_output$data)) {
      shiny::tagList()
    } else if (inherits(model_output$data, "error")) {
      shiny::div(
        class = "panel-group",
        shiny::div(
          class = sprintf("panel panel-%s", "danger"),
          shiny::div(
            class = "panel-heading",
            shiny::icon(paste("times-circle", "fa-lg")),
            sprintf("%s: %s", "Error", "running model")),
          shiny::div(
            class = "panel-body",
            model_output$data$message)))
    } else {
      dygraphs::dygraphOutput(ns("result_plot"))
    }
  })

  shiny::observe({
    ## NOTE: the isTRUE here seems necessary when this is composed
    ## into the editor app at least; otherwise it fails to start up
    ## somehow!
    if (isTRUE(input$auto_run)) {
      update_model(model, input, model_output, control, extra)
    }
  })

  shiny::observeEvent(
    input$go_button, {
      update_model(model, input, model_output, control, extra)
    })

  output$result_plot <- dygraphs::renderDygraph({
    graph_options <- mod_model_getgraph_options(input, control$output_name_map)
    if (is.null(model_output$data$replicates)) {
      plot_model_output_single(model_output$data$output_expanded,
                               graph_options)
    } else {
      plot_model_output_replicates(model_output$data$output_expanded,
                                   graph_options)
    }
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      ## This should be the model title but for some reason that's
      ## hard to get here.  So it needs passing into the module on
      ## creation instead and that's a bit more work than I'd like to
      ## do right now.
      mod_model_compute_filename("model",
                                 input$download_filename,
                                 input$download_format)
    },
    content = function(filename) {
      write_model_data(model_output$data, filename, input$download_format)
    })
}


mod_model_getpars <- function(x, map) {
  ret <- lapply(map, function(el) x[[el]])
  ret[lengths(ret) != 0L]
}


mod_model_gettime <- function(x, has_start_time, discrete) {
  time_start <- if (has_start_time) x$time_start else 0.0
  time_end <- x$time_end
  if (is.null(time_start) || is.null(time_end)) {
    msg <- if (has_start_time) "Start and end" else "End"
    stop(sprintf("%s must be given", msg))
  }
  if (discrete) {
    seq(time_start, time_end, by = round(x$time_detail))
  } else {
    seq(time_start, time_end, length.out = round(x$time_detail))
  }
}


mod_model_getoutput <- function(x, map) {
  vlapply(map, function(el) x[[el]])
}


mod_model_getgraph_options <- function(input, name_map) {
  include <- mod_model_getoutput(input, name_map)
  palette <- odin_ui_palettes("odin")
  cols <- set_names(palette(length(include)), names(include))
  list(include = include,
       cols = cols,
       line_width = 1,
       fill = FALSE,
       alpha = 1,
       stack = FALSE)
}


mod_model_compute_filename <- function(title, filename, format) {

  if (!nzchar(filename)) {
    filename <- title
  }

  sprintf("%s.%s", filename, format)
}


update_model <- function(model, input, output, control, extra) {
  output$data <- tryCatch({
    pars <- mod_model_getpars(input, control$parameter_name_map)
    time <- mod_model_gettime(input, control$has_start_time, control$discrete)
    replicates <- input$replicates
    if (identical(pars, output$pars) && identical(time, output$time)) {
      ## I think that this should *not* be possible for stochastic models!
      message("Skipping")
      return()
    }
    run_model(model, pars, time, replicates, extra)
  }, error = function (x) str(x))
}
