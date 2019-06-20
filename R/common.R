module_status <- function(class, title, body) {
  list(ui = simple_panel(class, title, body),
       class = class,
       ok = class == "success")
}


show_module_status_if_not_ok <- function(x) {
  if (!isTRUE(x$ok)) {
    x$ui
  }
}


show_module_status_if_ok <- function(x) {
  if (isTRUE(x$ok)) {
    x$ui
  }
}


text_module_status <- function(x) {
  paste0("text-", x$class %||% "danger")
}


get_inputs <- function(input, ids, names) {
  set_names(lapply(ids, function(x) input[[x]]), names)
}


set_inputs <- function(session, ids, values, fn = shiny::updateNumericInput) {
  for (i in seq_along(ids)) {
    shiny::updateNumericInput(session, ids[[i]], value = values[[i]])
  }
}


odin_colours <- function(model, data, link) {
  col_model <- odin_colours_model(model)
  col_data <- odin_colours_data(data)

  if (length(link) > 0L) {
    link <- list_to_character(link, TRUE)
    col_model[link] <- col_data[names(link)]
  }

  list(model = col_model, data = col_data)
}


odin_colours_data <- function(data) {
  set_names(odin_ui_palettes("brewer_set1")(length(data)), data)
}


## It'll be hard to get the same colours here without creating
## dependencies on the link that are unfortunate.
odin_colours_model <- function(model) {
  set_names(odin_ui_palettes("odin")(length(model)), model)
}


odin_y2 <- function(y2_model, name_data, link) {
  y2_model <- vlapply(y2_model, isTRUE)
  y2_data <- set_names(rep(FALSE, length(name_data)), name_data)
  y2_data[names(link)] <- y2_model[list_to_character(link)]
  list(model = y2_model, data = y2_data)
}


##' @importFrom plotly plot_ly
plot_plotly <- function(series, logscale_y = FALSE) {
  if (length(series) == 0L) {
    return(NULL)
  }
  p <- plotly::plot_ly()
  p <- plotly::config(p, collaborate = FALSE, displaylogo = FALSE)

  ## Don't truncate labels:
  hoverlabel <- list(namelength = -1)

  for (s in series) {
    if (!is.null(s$marker)) {
      p <- plotly::add_markers(p, x = s$x, y = s$y, name = s$name,
                               marker = s$marker, yaxis = s$yaxis,
                               hoverlabel = hoverlabel,
                               showlegend = s$showlegend,
                               legendgroup = s$legendgroup)
    } else {
      p <- plotly::add_lines(p, x = s$x, y = s$y, name = s$name,
                             line = s$line, yaxis = s$yaxis,
                             hoverlabel = hoverlabel,
                             showlegend = s$showlegend,
                             legendgroup = s$legendgroup)
    }
  }

  if (isTRUE(logscale_y)) {
    p <- plotly::layout(p, yaxis = list(type = "log"))
  }

  if (any(vcapply(series, "[[", "yaxis") == "y2")) {
    opts <- list(overlaying = "y",
                 side = "right",
                 showgrid = FALSE,
                 type = if (logscale_y) "log" else "linear")
    p <- plotly::layout(p, yaxis2 = opts)
  }
  p
}


plot_plotly_series <- function(x, y, name, col, points = FALSE, y2 = FALSE,
                               showlegend = TRUE, legendgroup = NULL,
                               width = NULL, dash = "solid",
                               symbol = "circle") {
  i <- is.na(x) | is.na(y)
  if (any(i)) {
    x <- x[!i]
    y <- y[!i]
  }
  yaxis <- if (y2) "y2" else "y1"
  ret <- list(x = x, y = y, name = name, yaxis = yaxis,
              legendgroup = legendgroup, showlegend = showlegend)
  if (points) {
    ret$marker <- list(color = col, symbol = symbol)
  } else {
    ret$line <- list(color = col, width = width, dash = dash)
  }
  ret
}


plot_plotly_series_bulk <- function(x, y, col, points, y2,
                                    showlegend = TRUE, legendgroup = NULL,
                                    width = NULL, dash = "solid",
                                    symbol = "circle") {
  nms <- colnames(y)
  y2 <- expand_and_name(y2, nms)
  legendgroup <- expand_and_name(legendgroup, nms)
  width <- expand_and_name(width, nms)
  dash <- expand_and_name(dash, nms)
  symbol <- expand_and_name(symbol, nms)
  lapply(nms, function(i)
    plot_plotly_series(x, y[, i], i, col[[i]], points, y2[[i]],
                       showlegend = showlegend,
                       legendgroup = legendgroup[[i]],
                       width = width[[i]], dash = dash[[i]],
                       symbol = symbol[[i]]))
}


common_control_parameters <- function(pars, ns, restore = NULL) {
  if (is.null(pars)) {
    return(NULL)
  }
  value <- restore %||% pars$value
  mod_model_control_section(
    "Model parameters",
    Map(simple_numeric_input, pars$name, ns(pars$id_value), value),
    ns = ns)
}


common_control_graph_downloads <- function(ns) {
  shiny::div(
    class = "form-inline mt-5",
    shiny::div(
      class = "form-group",
      raw_text_input(
        ns("download_filename"), placeholder = "filename", value = "")),
    shiny::div(
      class = "form-group",
      raw_select_input(
        ns("download_type"),
        choices = list("modelled", "combined", "parameters"))),
    shiny::downloadButton(
      ns("download_button"), "Download", class = "btn-blue"))
}


common_control_graph <- function(configuration, ns, check_title,
                                 restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }

  shiny::div(
    class = "pull-right",
    common_control_graph_downloads(ns),
    common_control_graph_settings(configuration, ns, check_title, restore))
}


common_control_graph_settings <- function(configuration, ns, check_title,
                                          restore) {
  title <- "Graph settings"
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  vars <- configuration$vars
  if (!is.null(vars$include)) {
    vars <- vars[vars$include, , drop = FALSE]
  }
  labels <- Map(function(lab, col)
    shiny::span(lab, style = paste0("color:", col)),
    vars$name, configuration$cols$model[vars$name])

  if (!is.null(restore)) {
    value_option <- restore$option
    value_logscale_y <- restore$logscale_y
  } else {
    value_option <- FALSE
    value_logscale_y <- FALSE
  }

  tags <- shiny::div(class = "form-group",
                     raw_checkbox_input(ns("logscale_y"), "Log scale y axis",
                                        value_logscale_y),
                     shiny::tags$label(check_title),
                     Map(raw_checkbox_input, ns(vars$id_graph_option),
                         labels, value_option))

  head <- shiny::a(style = "text-align: right; display: block;",
                   "data-toggle" = "collapse",
                   class = "text-muted",
                   href = paste0("#", id),
                   title, shiny::icon("gear", lib = "font-awesome"))

  body <- shiny::div(id = id,
                     class = "collapse box",
                     style = "width: 300px;",
                     list(tags))

  shiny::div(class = "pull-right mt-3", head, body)
}


common_download_filename <- function(filename, type, prefix) {
  if (is_missing(filename)) {
    filename <- sprintf("odin-%s-%s-%s.csv", prefix, type, date_string())
  } else {
    filename <- ensure_extension(filename, "csv")
  }
  filename
}


common_download_data <- function(filename, simulation, type) {
  data <- switch(type,
                 modelled = simulation$smooth,
                 combined = simulation$combined,
                 parameters = simulation$user)
  write_csv(data, filename)
}


common_model_data_configuration <- function(model, data, link) {
  if (!isTRUE(model$success) || !isTRUE(data$configured)) {
    return(NULL)
  }

  ## Augment parameters with standard ids for working with shiny ui
  ## elements
  pars <- model$info$pars
  pars$value <- vnapply(pars$default_value, function(x) x %||% NA_real_)
  pars$id_value <- sprintf("par_value_%s", pars$name)

  ## ...and same for the variables, but we'll do this as a "graph
  ## option" because the place that this will turn up is the graphing
  ## options seection.
  vars <- model$info$vars
  vars$id_graph_option <- sprintf("var_graph_option_%s", vars$name)

  cols <- odin_colours(vars$name, data$name_vars, link$map)

  list(data = data, model = model, link = link,
       pars = pars, vars = vars, cols = cols)
}


add_status <- function(x, status) {
  c(x, list(status = status))
}


with_success <- function(expr) {
  res <- tryCatch(
    force(expr),
    error = identity)
  if (inherits(res, "error")) {
    list(success = FALSE,
         value = NULL,
         error = res$message)
  } else {
    list(success = TRUE,
         value = res,
         error = NULL)
  }
}


## Standard data structure for a data set that knows what is going on
## with time.
odin_data_source <- function(data, filename, name_time) {
  vars <- names(data)
  if (is_missing(name_time) || !(name_time %in% vars)) {
    name_time <- NULL
  }
  configured <- !is.null(data) && !is.null(name_time)
  result <- list(data = data, filename = filename, configured = configured)

  if (configured) {
    result$name_time <- name_time
    result$name_vars <- setdiff(vars, name_time)
    result$cols <- odin_colours_data(result$name_vars)
  }

  result
}


## result here must be the output of odin::odin_build, which includes:
##
## success, elapsed, output, model, ir & error
odin_model <- function(result, code) {
  if (is.null(result)) {
    result <- list(success = FALSE)
  }
  if (result$success) {
    result$info <- model_info(result$model)
  }
  if (!is.null(result$elapsed)) {
    result$elapsed <- result$elapsed[["elapsed"]]
  }
  result$code <- code
  result$is_current <- TRUE
  result
}


common_odin_validate <- function(code) {
  res <- odin::odin_validate(code, "text")
  if (!is.null(res$error)) {
    res$error <- res$error$message
  }
  res$code <- code
  res$messages <- vcapply(res$messages, function(x) x$message)
  res
}


common_odin_compile <- function(validation) {
  if (validation$success) {
    result <- odin::odin_build(validation$result)
  } else {
    result <- NULL
  }
  odin_model(result, validation$code)
}


common_odin_compile_from_code <- function(code) {
  common_odin_compile(common_odin_validate(code))
}
