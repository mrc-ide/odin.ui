mod_model_control <- function(graph_data, default_time, parameters, extra,
                              ns = identity) {
  pars <- mod_model_control_parameters(parameters, ns)
  time <- mod_model_control_time(default_time, graph_data, ns)
  reps <- mod_model_control_reps(graph_data, 1L, ns)
  output <- mod_model_control_output(graph_data, extra, ns)
  graph_options <- mod_model_control_graph_options(ns)

  tags <- shiny::div(
    class = "panel-group",
    pars$tags,
    time$tags,
    reps$tags,
    output$tags,
    graph_options$tags)

  list(tags = tags,
       parameter_name_map = pars$name_map,
       has_start_time = time$has_start_time,
       output_name_map = output$name_map,
       discrete = graph_data$discrete,
       stochastic = graph_data$stochastic,
       replicates = reps$replicates)
}


mod_model_control_section <- function(title, ..., ns) {
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  head <- shiny::div(
    class = "panel-heading",
    shiny::h4(
      class = "panel-title",
      shiny::a(
        "data-toggle" = "collapse",
        "href" = paste0("#", id),
        title)))

  body <- shiny::div(
    id = id,
    class = "panel-collapse collapse in",
    shiny::div(
      class = "panel-body",
      ...))

  shiny::div(
    class = "panel panel-info",
    head,
    body)
}


mod_model_control_parameters <- function(parameters, ns) {
  ## TODO: can we have a real list structure here?
  if (length(parameters) > 0L) {
    name_map <- set_names(paste0("pars_", names(parameters)), names(parameters))
    input <- function(x) {
      if (is.null(x$description)) {
        title <- x$name
      } else {
        title <- sprintf("%s: %s", x$name, x$description)
      }

      if (x$has_range) {
        shiny::sliderInput(ns(name_map[[x$name]]), title, min = x$range_min,
                           max = x$range_max, value = x$default)
      } else {
        shiny::numericInput(ns(name_map[[x$name]]), title,
                            value = x$default)
      }
    }
    tags <- mod_model_control_section(
      "Parameters",
      unname(lapply(parameters, input)),
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
mod_model_control_time <- function(default_time, graph_data, ns) {
  if (length(default_time) == 1L) {
    default_time <- c(0, default_time)
    has_start_time <- FALSE
  } else if (length(default_time) == 2L) {
    has_start_time <- TRUE
  } else {
    stop("'default_time' must be length 1 or 2")
  }

  if (graph_data$discrete) {
    time_detail <- shiny::numericInput(
      ns("time_detail"), "reporting interval", 1L)
  } else {
    time_detail <- shiny::numericInput(
      ns("time_detail"), "number of output points", 1000L)
  }
  time_end <- shiny::numericInput(
    ns("time_end"), "end", default_time[[2L]])

  if (has_start_time) {
    time_start <- shiny::numericInput(
      ns("time_start"), "start", default_time[[1L]])
  } else {
    time_start <- NULL
  }

  tags <- mod_model_control_section(
    "Time",
    drop_null(list(time_start, time_end, time_detail)),
    ns = ns)

  list(tags = tags, has_start_time = has_start_time)
}


mod_model_control_reps <- function(graph_data, default, ns) {
  if (!graph_data$stochastic) {
    return(list(tags = NULL, replicates = FALSE))
  }
  tags <- mod_model_control_section(
    "Replicates",
    shiny::numericInput(
      ns("replicates"), "replicates", default),
    ns = ns)
  list(tags = tags, replicates = TRUE)
}


mod_model_control_output <- function(graph_data, extra, ns) {
  vars <- graph_data$nodes[graph_data$nodes$type %in% c("variable", "output"), ]

  if (!is.null(extra)) {
    extra <- data_frame(id = names(extra), label = names(extra),
                        name_target = names(extra), rank = NA_integer_,
                        type = "extra", stage = "time")
    vars <- rbind(vars, extra)
  }

  name_map <- set_names(paste0("plot_", vars$name_target), vars$name_target)

  tags <- mod_model_control_section(
    "Output",
    Map(shiny::checkboxInput, ns(name_map), vars$name_target, value = TRUE),
    ns = ns)

  list(tags = tags, name_map = name_map)
}


mod_model_control_graph_options <- function(ns) {
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
                                        value = FALSE)

  stack_checkbox <- shiny::checkboxInput(ns("graph_stack"),
                                        "Stack the graph?",
                                        value = FALSE)

  alpha_slider <- shiny::sliderInput(ns("graph_alpha"),
                                     "Opacity",
                                     min = 0, max = 1,
                                     value = 1, step = 0.01)

  tags <- mod_model_control_section(
    "Graph options",
    list(choice_palette, width_slider, fill_checkbox,
         stack_checkbox, alpha_slider),
    ns = ns)

  list(tags = tags)
}
