mod_model_control <- function(graph_data, default_time, parameters, extra,
                              ns = identity) {
  pars <- mod_model_control_parameters(parameters, ns)
  run_options <- mod_model_control_run_options(default_time, graph_data, 1L, extra, ns)

  tags <- shiny::div(
    class = "list-group odin-options",
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


mod_model_control_section <- function(title, ..., ns, collapsed = FALSE) {
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  head <- shiny::a(class="list-group-item",
          "data-toggle" = "collapse",
          "href" = paste0("#", id),
          title, shiny::icon("sort", lib="font-awesome", class="pull-right"))

    if (collapsed){
        cssClass <- ""
    }
    else {
        cssClass <- "in"
    }

  body <- shiny::div(
    id = id,
    class = paste0("collapse ", cssClass),
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
mod_model_control_run_options <- function(default_time, graph_data, default_reps, extra, ns, collapsed = FALSE) {
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
    else {
      reps <- NULL
  }

  tags <- mod_model_control_section(
    "Run options",
    drop_null(list(time_start, time_end, time_detail, reps)),
    ns = ns,
    collapsed = collapsed)

    outputs <- mod_model_control_outputs(graph_data, extra, ns)

  list(tags = tags, has_start_time = has_start_time, replicates = has_replicates, output_name_map = outputs$name_map)
}

mod_model_control_outputs <- function(graph_data, extra, ns) {
  vars <- graph_data$nodes[graph_data$nodes$type %in% c("variable", "output"), ]

  if (!is.null(extra)) {
    extra <- data.frame(id = names(extra), label = names(extra),
                        name_target = names(extra), rank = NA_integer_,
                        type = "extra", stage = "time",
                        stringsAsFactors = FALSE, check.names = FALSE)
    vars <- rbind(vars, extra)
  }

  name_map <- set_names(paste0("plot_", vars$name_target), vars$name_target)

  list(name_map = name_map, vars = vars)
}

mod_model_control_graph_options <- function(graph_data, extra, ns) {

    title <- "Graph settings"

  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  outputs <- mod_model_control_outputs(graph_data, extra, ns)
  tags <- shiny::div(class="form-group",
                shiny::tags$label("outputs"),
                Map(raw_checkbox_input, ns(outputs$name_map), outputs$vars$name_target, value = TRUE))

    head <- shiny::a(style="text-align: right; display: block;",
                  "data-toggle" = "collapse",
                  class = "text-muted",
                  "href" = paste0("#", id),
                  title, shiny::icon("gear", lib="font-awesome"))

  body <- shiny::div(id = id,
                    class = "collapse box",
                    style="width: 300px;",
                    list(tags))

  list(tags = shiny::div(class="pull-right mt-3", head, body))
}
