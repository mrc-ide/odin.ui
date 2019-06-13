mod_plotly_ui <- function(id, title) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (!is.null(title)) {
      shiny::titlePanel(title)
    },
    ## shiny::p(class = "mt-5"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(class = "form-horizontal", mod_plotly_input(id))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   shiny::uiOutput(ns("odin_output"))),
        shiny::uiOutput(ns("graph_settings")))))
}


##' @importFrom dde difeq_replicate
mod_plotly_input <- function(id) {
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


mod_plotly_control_graph <- function(metadata, extra, output_control, ns) {
  graph_options <- mod_plotly_control_graph_options(metadata, extra,
                                                    output_control, ns)
  shiny::tagList(
    shiny::div(
      class = "pull-right",
      shiny::div(
        class = "form-inline mt-5",
        shiny::div(
          class = "form-group",
          raw_text_input(ns("download_filename"), placeholder = "filename",
                         value = "")),
        shiny::span("."),
        shiny::div(
          class = "form-group",
          raw_select_input(ns("download_format"),
                           choices = list("csv","rds","json"))),
        shiny::downloadButton(ns("download_button"), "Download",
                              class = "btn-blue")),
      graph_options$tags))
}

mod_plotly_server <- function(input, output, session, model,
                              default_time, parameters, extra = NULL,
                              output_control = NULL,
                              default_replicates = 1L, time_scale =
                              NULL) { ns <- session$ns

  metadata <- model_metadata(model)
  extra <- validate_extra(extra, metadata)
  time_scale <- validate_time_scale(time_scale, metadata)

  parameters <- validate_model_parameters(model, parameters)
  model_output <- shiny::reactiveValues(data = NULL)
  control <- mod_plotly_control(metadata, default_time, default_replicates,
                                parameters, extra, output_control, ns)

  output$odin_control <- shiny::renderUI({
    path_css <- odin_ui_file("css/styles.css")
    times <- input$reset_button
    model_output$data <- NULL
    shiny::div(id = ns(paste0("odin_control_", times)),
               shiny::includeCSS(path_css),
               control$tags)
  })

  output$graph_settings <- shiny::renderUI({
    mod_plotly_control_graph(metadata, extra, output_control, ns)
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
      plotly::plotlyOutput(ns("result_plot"))
    }
  })

  shiny::observe({
    ## NOTE: the isTRUE here seems necessary when this is composed
    ## into the editor app at least; otherwise it fails to start up
    ## somehow!
    if (isTRUE(input$auto_run)) {
      mod_plotly_update_model(
        model, input, model_output, control, extra, time_scale)
    }
  })

  shiny::observeEvent(
    input$go_button, {
      mod_plotly_update_model(
        model, input, model_output, control, extra, time_scale)
    })

  output$result_plot <- plotly::renderPlotly({
    graph_options <- mod_plotly_getgraph_options(input, control$output_name_map,
                                                 output_control)
    if (is.null(model_output$data$replicates)) {
      plot_model_output_single_plotly(model_output$data$output_expanded,
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
      mod_plotly_compute_filename("model",
                                  input$download_filename,
                                  input$download_format)
    },
    content = function(filename) {
      write_model_data(model_output$data, filename, input$download_format)
    })
}


mod_plotly_getpars <- function(x, map) {
  ret <- lapply(map, function(el) x[[el]])
  ret[lengths(ret) != 0L]
}


mod_plotly_gettime <- function(x, has_start_time, discrete) {
  time_start <- if (has_start_time) x$time_start else 0.0
  time_end <- x$time_end
  if (is.null(time_start) || is.null(time_end)) {
    msg <- if (has_start_time) "Start and end" else "End"
    stop(sprintf("%s must be given", msg))
  }
  list(start = time_start, end = time_end, detail = x$time_detail,
       discrete = discrete)
}


mod_plotly_getoutput <- function(x, map) {
  vlapply(map, function(el) x[[el]])
}


mod_plotly_getgraph_options <- function(input, name_map, output_control) {
  include <- set_names(rep(TRUE, length(name_map)), names(name_map))
  second_y <- mod_plotly_getoutput(input, name_map)
  palette <- odin_ui_palettes("odin")
  cols <- set_names(palette(length(name_map)), names(name_map))
  list(include = include,
       cols = cols,
       line_width = 1,
       fill = FALSE,
       alpha = 1,
       stack = FALSE,
       second_y = second_y,
       logscale_y = input$logscale_y)
}


mod_plotly_compute_filename <- function(title, filename, format) {

  if (!nzchar(filename)) {
    filename <- title
  }

  sprintf("%s.%s", filename, format)
}


mod_plotly_update_model <- function(model, input, output, control, extra, time_scale) {
  output$data <- tryCatch({
    pars <- mod_plotly_getpars(input, control$parameter_name_map)
    time <- mod_plotly_gettime(input, control$has_start_time, control$discrete)
    if (is.null(input$replicates)) {
      replicates <- NULL
    } else {
      replicates <- min(MAX_REPLICATES_MODEL, input$replicates)
    }
    if (identical(pars, output$pars) && identical(time, output$time)) {
      ## I think that this should *not* be possible for stochastic models!
      message("Skipping")
      return()
    }
    run_model(model, pars, time, replicates, extra, time_scale)
  }, error = function (x) utils::str(x))
}


mod_plotly_control <- function(...) {
  mod_model_control(...)
}


mod_plotly_control_graph_options <- function(metadata, extra, output_control,
                                             ns) {
  title <- "Graph settings"

  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  outputs <- mod_model_control_outputs(metadata, extra, output_control, ns)

  ## TODO: this is duplicated from the graphing code and needs pulling
  ## in somewhere better once we decide on the right course of action
  ## for graph options.
  palette <- odin_ui_palettes("odin")
  cols <- palette(length(outputs$name_map))
  labels <- Map(function(lab, col)
    shiny::span(lab, style = paste0("color:", col)),
    outputs$vars$name,
    palette(length(outputs$name_map)))

  tags <- shiny::div(class = "form-group",
                     raw_checkbox_input(ns("logscale_y"), "Log scale y axis"),
                     shiny::tags$label("Plot on second y axis"),
                     Map(raw_checkbox_input, ns(outputs$name_map),
                         labels, value = FALSE))

  head <- shiny::a(style = "text-align: right; display: block;",
                   "data-toggle" = "collapse",
                   class = "text-muted",
                   href = paste0("#", id),
                   title, shiny::icon("gear", lib = "font-awesome"))

  body <- shiny::div(id = id,
                    class = "collapse box",
                    style = "width: 300px;",
                    list(tags))

  list(tags = shiny::div(class = "pull-right mt-3", head, body))
}
