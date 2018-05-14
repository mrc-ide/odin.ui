mod_model_input <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("odin_control")),
    shiny::hr(),
    ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
    shiny::actionButton(ns("go_button"), "Run model",
                        shiny::icon("play"),
                        class = "btn-primary"),
    shiny::actionButton(ns("reset_button"), "Reset",
                        shiny::icon("refresh"),
                        class = "btn-danger"))
}


mod_model_output <- function(id) {
  ns <- shiny::NS(id)
  dygraphs::dygraphOutput(ns("result_plot"))
}


## all-in-one module that includes a sidebar interface
mod_model_ui <- function(id, title) {
  path_css <- odin_ui_file("styles.css")
  shiny::tagList(
    shiny::includeCSS(path_css),
    if (!is.null(title)) shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(mod_model_input(id)),
      shiny::mainPanel(mod_model_output(id))))
}


mod_model <- function(input, output, session,
                          model, default_time) {
  ns <- session$ns
  model_output <- shiny::reactiveValues(data = NULL)
  control <- mod_model_control(model, default_time, ns)

  output$odin_control <- shiny::renderUI({
    times <- input$reset_button
    model_output$data <- NULL
    shiny::div(id = ns(paste0("odin_control_", times)),
               control$tags)
  })

  shiny::observeEvent(
    input$go_button, {
      p <- mod_model_getpars(input, control$parameter_name_map)
      t <- mod_model_gettime(input, control$has_start_time)
      model_output$data <- run_model(model, p, t)
    })

  output$result_plot <- dygraphs::renderDygraph({
    if (is.null(model_output$data)) {
      return()
    }
    
    graph_options <- mod_model_getgraph_options(input, control$output_name_map)
    
    plot_model_output(model_output$data, graph_options)
  })
}


mod_model_getpars <- function(x, map) {
  ret <- lapply(map, function(el) x[[el]])
  ret[lengths(ret) != 0L]
}


mod_model_gettime <- function(x, has_start_time) {
  time_start <- if (has_start_time) x$time_start else 0.0
  seq(time_start, x$time_end, length.out = round(x$time_detail))
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
