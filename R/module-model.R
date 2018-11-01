##' @importFrom dde difeq_replicate
mod_model_input <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("odin_control")),
    ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
    shiny::actionButton(ns("reset_button"), "Reset", shiny::icon("refresh"), class = "btn-warning pull-right ml-2"),
    shiny::actionButton(ns("go_button"), "Run model", shiny::icon("play"), class = "btn-purple pull-right"),
    shiny::div(class="form-group pull-right", style="clear:both;",
      shiny::div(class="col-sm-12", raw_checkbox_input(ns("auto_run"), "Auto run", value = FALSE))
    )
  )
}


## all-in-one module that includes a sidebar interface
mod_model_ui <- function(id, title) {
  ns <- shiny::NS(id)
  path_css <- odin_ui_file("css/styles.css")
  shiny::tagList(
    shiny::includeCSS(path_css),
    if (!is.null(title)) {
      shiny::titlePanel(title)
    } else {
      ## TODO: This is not really super tidy but I need a little
      ## spare vertical space here before the panel layout but I
      ## don't see the cleanest way of adding it.
      shiny::p(class = "spacer")
    },

    shiny::p(class = "mt-5"),
    shiny::sidebarLayout(
      shiny::div(class = "col-sm-4 col-lg-3", tags$form(class = "form-horizontal", mod_model_input(id))),
      shiny::mainPanel(shiny::uiOutput(ns("odin_output")))
    ))
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
    times <- input$reset_button
    model_output$data <- NULL
    shiny::div(id = ns(paste0("odin_control_", times)),
                control$tags)
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
      graph_options <- mod_model_control_graph_options(ns)
      shiny::tagList(
        dygraphs::dygraphOutput(ns("result_plot")),
        shiny::div(class="pull-right",
                    shiny::div(class="form-inline mt-5",
                                  shiny::div(class="form-group", shiny::tags$label("Format:"), raw_select_input(ns("download_format"), choices = list("auto","csv","rds","json"))),
                                  shiny::div(class="form-group", shiny::tags$label("Filename:"), raw_text_input(ns("download_filename"), value = "")),
                                  shiny::downloadButton(ns("download_button"), "Download", class="btn-purple")
                  ),
                  graph_options$tags
        )
      )
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
      plot_model_output_single(model_output$data$output_expanded, graph_options)
    } else {
      plot_model_output_replicates(model_output$data, graph_options)
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
  palette <- odin_ui_palettes(input$choice_palette)
  cols <- set_names(palette(length(include)), names(include))
  list(include = include,
       cols = cols,
       line_width = input$line_width,
       fill = input$graph_fill,
       alpha = input$graph_alpha,
       stack = input$graph_stack)
}


mod_model_compute_filename <- function(title, filename, format) {
  if (nzchar(filename)) {
    filename
  } else {
    ext <- if (format == "auto") "csv" else format
    sprintf("%s.%s", title, ext)
  }
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
  }, error = identity)
}
