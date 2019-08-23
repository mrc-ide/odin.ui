mod_control_batch_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_control_batch_plot_server <- function(input, output, session, render) {
  rv <- shiny::reactiveValues()

  output$ui <- shiny::renderUI({
    control_batch_plot_ui(render(), session$ns)
  })

  output$options <- shiny::renderUI({
    control_batch_plot_options(input$type, session$ns)
  })

  shiny::observe({
    rv$result <- list(type = input$type,
                      slice_time = input$slice_time,
                      extreme_type = input$extreme_type)
  })

  reset <- function() {
    output$ui <- shiny::renderUI(
      control_batch_plot_ui(render(), session$ns))
  }

  list(result = shiny::reactive(rv$result),
       reset = reset)
}


control_batch_plot_ui <- function(render, ns) {
  if (!isTRUE(render)) {
    return(NULL)
  }

  types <- c("Trace over time" = "trace",
             "Value at a single time" = "slice",
             "Value at its min/max" = "extreme",
             "Time at value's min/max" = "textreme")

  odin_control_section(
    "Plot options",
    simple_select_input("Type of plot", ns("type"), types),
    shiny::uiOutput(ns("options")),
    ns = ns)
}


control_batch_plot_options <- function(type, ns) {
  switch(
    type %||% "slice",
    slice = simple_numeric_input(
      "Time to use (default is last)", ns("slice_time"), NA),
    extreme = simple_select_input(
      "Extreme to use", ns("extreme_type"), c("max", "min")),
    textreme = simple_select_input(
      "Extreme to use", ns("extreme_type"), c("max", "min")),
    NULL)
}
