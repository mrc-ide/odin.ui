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


text_module_status <- function(x) {
  paste0("text-", x$class %||% "danger")
}


get_inputs <- function(input, ids, names) {
  set_names(lapply(ids, function(x) input[[x]]), names)
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
  y2_data <- set_names(rep(FALSE, length(name_data)), name_data)
  y2_data[names(link)] <- y2_model[list_to_character(link)]
  list(model = list_to_logical(y2_model, TRUE), data = y2_data)
}


##' @importFrom plotly plot_ly
plot_plotly <- function(series, logscale_y = FALSE) {
  p <- plotly::plot_ly()
  p <- plotly::config(p, collaborate = FALSE, displaylogo = FALSE)

  for (s in series) {
    if (!is.null(s$marker)) {
      p <- plotly::add_markers(p, x = s$x, y = s$y, name = s$name,
                               marker = s$marker, yaxis = s$yaxis)
    } else {
      p <- plotly::add_lines(p, x = s$x, y = s$y, name = s$name,
                             line = s$line, yaxis = s$yaxis)
    }
  }

  if (logscale_y) {
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


plot_plotly_series <- function(x, y, name, col, points = FALSE, y2 = FALSE) {
  i <- is.na(x) | is.na(y)
  if (any(i)) {
    x <- x[!i]
    y <- y[!i]
  }
  yaxis <- if (y2) "y2" else "y1"
  ret <- list(x = x, y = y, name = name, yaxis = yaxis)
  if (points) {
    ret$marker <- list(color = col)
  } else {
    ret$line <- list(color = col)
  }
  ret
}


plot_plotly_series_bulk <- function(x, y, col, points, y2) {
  lapply(colnames(y), function(i)
    plot_plotly_series(x, y[, i], i, col[[i]], points, y2[[i]]))
}
