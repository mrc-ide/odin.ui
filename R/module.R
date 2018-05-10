odin_ui_model_input <- function(id) {
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


odin_ui_model_output <- function(id) {
  ns <- shiny::NS(id)
  dygraphs::dygraphOutput(ns("result_plot"))
}


## all-in-one module that includes a sidebar interface
odin_ui_model_ui <- function(id, title) {
  path_css <- system.file("styles.css", package = "odin.ui", mustWork = TRUE)
  shiny::tagList(
    shiny::includeCSS(path_css),
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(odin_ui_model_input("odin_ui")),
      shiny::mainPanel(odin_ui_model_output("odin_ui"))))
}


odin_ui_model <- function(input, output, session,
                          model, default_time) {
  ns <- session$ns
  model_output <- shiny::reactiveValues(data = NULL)
  control <- odin_ui_control(model, default_time, ns)

  output$odin_control <- shiny::renderUI({
    times <- input$reset_button
    model_output$data <- NULL
    shiny::div(id = ns(paste0("odin_control_", times)),
               control$tags)
  })

  shiny::observeEvent(
    input$go_button, {
      p <- odin_ui_get_pars(input, control$parameter_name_map)
      t <- odin_ui_get_time(input, control$has_start_time)
      model_output$data <- run_model(model, p, t)
    })

  output$result_plot <- dygraphs::renderDygraph({
    if (is.null(model_output$data)) {
      return()
    }
    include <- odin_ui_get_output(input, control$output_name_map)
    plot_model_output(model_output$data, include, control$output_cols)
  })
}


odin_ui_get_pars <- function(x, map) {
  ret <- lapply(map, function(el) x[[el]])
  ret[lengths(ret) != 0L]
}


odin_ui_get_time <- function(x, has_start_time) {
  time_start <- if (has_start_time) x$time_start else 0.0
  seq(time_start, x$time_end, length.out = round(x$time_detail))
}


odin_ui_get_output <- function(x, map) {
  vlapply(map, function(el) x[[el]])
}
