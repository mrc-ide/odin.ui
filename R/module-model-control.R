mod_model_control <- function(graph_data, default_time, parameters, extra,
                              ns = identity) {
  pars <- mod_model_control_parameters(parameters, ns)
  run_options <- mod_model_control_run_options(default_time, graph_data, 1L, extra, ns)

  tags <- shiny::div(
    class = "list-group options",
    pars$tags,
    run_options$tags)

  list(tags = tags,
       parameter_name_map = pars$name_map,
       has_start_time = run_options$has_start_time,
       output_name_map = run_options$output_name_map,
       discrete = graph_data$discrete,
       stochastic = graph_data$stochastic,
       replicates = run_options$replicates)
}


mod_model_control_section <- function(title, ..., ns) {
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  head <- shiny::a(class="list-group-item",
          "data-toggle" = "collapse",
          "href" = paste0("#", id),
          title, shiny::icon("sort", lib="font-awesome", class="pull-right"))

  body <- shiny::div(
    id = id,
    class = "collapse in",
    ...)

  list(head, body)
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
        horizontal_form_group(title, shiny::sliderInput(ns(name_map[[x$name]]), label = NULL, min = x$range_min,
                           max = x$range_max, value = x$default), label_width = 2, label_class="pt-5")
      } else {
        horizontal_form_group(title, raw_numeric_input(ns(name_map[[x$name]]),
                                                        value = x$default))
      }
    }
    tags <- mod_model_control_section(
      "Model parameters",
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
mod_model_control_run_options <- function(default_time, graph_data, default_reps, extra, ns) {
  if (length(default_time) == 1L) {
    default_time <- c(0, default_time)
    has_start_time <- FALSE
  } else if (length(default_time) == 2L) {
    has_start_time <- TRUE
  } else {
    stop("'default_time' must be length 1 or 2")
  }

  if (graph_data$discrete) {
    time_detail <- horizontal_form_group("reporting interval", raw_numeric_input(
      ns("time_detail"), 1L))
  } else {
    time_detail <- horizontal_form_group("number of output points", raw_numeric_input(
      ns("time_detail"), 1000L))
  }
  time_end <- horizontal_form_group("end", raw_numeric_input(
    ns("time_end"), default_time[[2L]]))

  if (has_start_time) {
    time_start <- horizontal_form_group("start", raw_numeric_input(
      ns("time_start"), default_time[[1L]]))
  } else {
    time_start <- NULL
  }

  has_replicates <- graph_data$stochastic

  if (has_replicates) {
    reps <- horizontal_form_group("replicates", raw_numeric_input(ns("replicates"), default_reps))
  }

  output <- mod_model_control_output(graph_data, extra, ns)

  tags <- mod_model_control_section(
    "Run options",
    drop_null(list(time_start, time_end, time_detail, reps, output)),
    ns = ns)

  list(tags = tags, has_start_time = has_start_time, replicates = has_replicates, output_name_map = output$name_map)
}

mod_model_control_output <- function(graph_data, extra, ns) {
  vars <- graph_data$nodes[graph_data$nodes$type %in% c("variable", "output"), ]

  if (!is.null(extra)) {
    extra <- data.frame(id = names(extra), label = names(extra),
                        name_target = names(extra), rank = NA_integer_,
                        type = "extra", stage = "time",
                        stringsAsFactors = FALSE, check.names = FALSE)
    vars <- rbind(vars, extra)
  }

  name_map <- set_names(paste0("plot_", vars$name_target), vars$name_target)

  tags <- horizontal_form_group("outputs", Map(raw_checkbox_input, ns(name_map), vars$name_target, value = TRUE))

  list(tags = tags, name_map = name_map)
}

raw_numeric_input <- function(inputId, value) {
  value <- restoreInput(id = inputId, default = value)
  tags$input(id = inputId, type = "number", class = "form-control",
    value = value, style = "width: 100px")
}

raw_checkbox_input <- function (inputId, label, value = FALSE)
{
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- shiny::tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"

 shiny::div(class = "checkbox",
    shiny::tags$label(inputTag, shiny::tags$span(label)))
}

horizontal_form_group <- function(label_name, input, label_width = 6, label_class = "") {
  shiny::div(class = "form-group",
    shiny::tags$label(label_name, class=paste0(label_class, paste0(" control-label col-sm-", label_width))),
    shiny::div(class=paste0("col-sm-", 12 - label_width), input)
  )
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

  title <- "Graph settings"

  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  head <- shiny::a(style="text-align: right; display: block;",
                  "data-toggle" = "collapse",
                  class = "text-muted",
                  "href" = paste0("#", id),
                  title, shiny::icon("gear", lib="font-awesome"))

  body <- shiny::div(id = id,
                    class = "collapse",
                    list(choice_palette, width_slider, fill_checkbox,
                    stack_checkbox, alpha_slider))

  list(tags = shiny::div(class="pull-right mt-3", head, body))
}
