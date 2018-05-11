odin_ui_control <- function(model, default_time, ns = identity) {
  pars <- odin_ui_control_parameters(model, ns)
  time <- odin_ui_control_time(default_time, ns)
  output <- odin_ui_control_output(model, ns)
  graph_options <- odin_ui_control_graph_options(ns)
  els <- c(pars$tags, time$tags, output$tags,
           graph_options$tags)
  list(tags = els,
       parameter_name_map = pars$name_map,
       has_start_time = time$has_start_time,
       output_name_map = output$name_map
       )
}


odin_ui_control_section <- function(title, ..., ns) {
  ## TODO: this needs some css styling
  ## https://stackoverflow.com/questions/29030260/inline-checkbox-next-to-a-h3-header
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))
  list(
    shiny::div(class = "odin_control_section_head",
               shiny::h2(title),
               shiny::checkboxInput(id, "hide")),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != true", id),
      ...))
}


odin_ui_control_parameters <- function(model, ns) {
  x <- stats::coef(model)

  if (!all(x$rank == 0L)) {
    stop("Only scalar parameters are currently supported")
  }
  if (!all(x$has_default)) {
    stop("All parameters must have defaults")
  }

  ## TODO: can we have a real list structure here?
  if (nrow(x) > 0L) {
    name_map <- set_names(paste0("pars_", x$name), x$name)
    tags <- odin_ui_control_section(
      "Parameters",
      unname(Map(shiny::numericInput, ns(name_map), x$name, x$default_value)),
      ns = ns)
  } else {
    name_map <- character()
    tags <- list()
  }

  list(tags = tags, name_map = name_map)
}


## TODO:
## * critical time
## * disable time selector entirely
## * solution tolerance
odin_ui_control_time <- function(default_time, ns) {
  if (length(default_time) == 1L) {
    default_time <- c(0, default_time)
    has_start_time <- FALSE
  } else if (length(default_time) == 2L) {
    has_start_time <- TRUE
  } else {
    stop("'default_time' must be length 1 or 2")
  }

  time_detail <- shiny::numericInput(
    ns("time_detail"), "number of output points", 1000)
  time_end <- shiny::numericInput(
    ns("time_end"), "end", default_time[[2L]])

  if (has_start_time) {
    time_start <- shiny::numericInput(
      ns("time_start"), "start", default_time[[1L]])
  } else {
    time_start <- NULL
  }

  tags <- odin_ui_control_section(
    "Time",
    drop_null(list(time_start, time_end, time_detail)),
    ns = ns)

  list(tags = tags, has_start_time = has_start_time)
}


odin_ui_control_output <- function(model, ns) {
  x <- attr(model, "graph_data")()
  vars <- x$nodes[x$nodes$type %in% c("variable", "output"), ]
  if (!all(vars$rank == 0L)) {
    stop("Currently all output must be scalar")
  }

  name_map <- set_names(paste0("plot_", vars$name_target), vars$name_target)
  
  tags <- odin_ui_control_section(
    "Output",
    Map(shiny::checkboxInput, ns(name_map), vars$name_target, value = TRUE),
    ns = ns)

  list(tags = tags, name_map = name_map)
}


odin_ui_control_graph_options <- function(ns) {
  ## note: choices must be valid palettes, as the palette will be obtained by
  ## 'get' in odin_ui_get_colors
  choices <- names(odin_ui_palettes())
  
  
  choice_palette <- shiny::selectInput(ns("choice_palette"),
                                       "Choose a palette",
                                       choices,
                                       selected = "odin")

  width_slider <- shiny::sliderInput(ns("line_width"),
                               "Indicate line width",
                               min = 0, max = 10,
                               value = 1, step = 0.1)

  fill_checkbox <- shiny::checkboxInput(ns("graph_fill"),
                                        "Fill the graph?",
                                        value = FALSE
                                        )

  stack_checkbox <- shiny::checkboxInput(ns("graph_stack"),
                                        "Stack the graph?",
                                        value = FALSE
                                        )

  alpha_slider <- shiny::sliderInput(ns("graph_alpha"),
                                     "Opacity",
                                     min = 0, max = 1,
                                     value = 1, step = 0.01)
  
  tags <- odin_ui_control_section(
    "Graph options",
    list(choice_palette, width_slider, fill_checkbox,
         stack_checkbox, alpha_slider),
    ns = ns)

  list(tags = tags)
}
