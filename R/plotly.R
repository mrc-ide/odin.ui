## This will probably get generalised out later but should work with
## most of the combination plots for now I think.
plotly_combine_series <- function(series, names) {
  n <- lengths(series)
  stopifnot(length(n) == 2)
  ret <- unlist(series, FALSE, FALSE)

  dash <- rep(c("solid", "dash"), n)
  name <- rep(names$long[1:2], n)

  for (i in seq_along(ret)) {
    x <- ret[[i]]

    x$name <- sprintf("%s (%s)", x$name, name[[i]])
    if ("line" %in% names(x)) {
      x$line$dash <- dash[[i]]
    }

    ret[[i]] <- x
  }

  ret
}


##' @importFrom plotly plot_ly
plot_plotly <- function(series, logscale_y = FALSE, xlab = "Time",
                        ylab = NULL, logscale_x = FALSE) {
  if (length(series) == 0L) {
    return(NULL)
  }
  p <- plotly::plot_ly()
  p <- plotly::config(p, displaylogo = FALSE,
                      modeBarButtonsToRemove = I("autoScale2d"))

  ## Don't truncate labels:
  hoverlabel <- list(namelength = -1)

  for (s in series) {
    if (!is.null(s$marker)) {
      p <- plotly::add_markers(p, x = s$x, y = s$y, name = s$name,
                               marker = s$marker, yaxis = s$yaxis,
                               hoverlabel = hoverlabel,
                               showlegend = s$showlegend,
                               legendgroup = s$legendgroup,
                               visible = s$visible)
    } else {
      p <- plotly::add_lines(p, x = s$x, y = s$y, name = s$name,
                             line = s$line, yaxis = s$yaxis,
                             hoverlabel = hoverlabel,
                             showlegend = s$showlegend,
                             legendgroup = s$legendgroup,
                             visible = s$visible)
    }
  }

  if (!is.null(xlab)) {
    p <- plotly::layout(p, xaxis = list(title = xlab))
  }
  if (!is.null(ylab)) {
    p <- plotly::layout(p, yaxis = list(title = ylab))
  }
  ## Force showing legend when only one series is included
  p <- plotly::layout(p, showlegend = TRUE)

  if (isTRUE(logscale_y)) {
    p <- plotly::layout(p, yaxis = list(type = "log"))
  }
  if (isTRUE(logscale_x)) {
    p <- plotly::layout(p, xaxis = list(type = "log"))
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
                               symbol = "circle", show = TRUE) {
  i <- is.na(x) | is.na(y)
  if (any(i)) {
    x <- x[!i]
    y <- y[!i]
  }
  yaxis <- if (y2) "y2" else "y1"
  ret <- list(x = x, y = y, name = name, yaxis = yaxis,
              legendgroup = legendgroup, showlegend = showlegend,
              visible = if (show) TRUE else "legendonly")
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
                                    symbol = "circle", label = NULL,
                                    show = TRUE) {
  nms <- colnames(y)
  label <- expand_and_name(label %||% colnames(y), nms)
  y2 <- expand_and_name(y2, nms)
  if (isTRUE(legendgroup)) {
    legendgroup <- set_names(colnames(y), colnames(y))
  } else {
    legendgroup <- expand_and_name(legendgroup, nms)
  }
  width <- expand_and_name(width, nms)
  dash <- expand_and_name(dash, nms)
  symbol <- expand_and_name(symbol, nms)
  show <- expand_and_name(show, nms)
  col <- expand_and_name(col, nms)
  lapply(nms, function(i)
    plot_plotly_series(x, y[, i], label[[i]], col[[i]], points, y2[[i]],
                       showlegend = showlegend,
                       legendgroup = legendgroup[[i]],
                       width = width[[i]], dash = dash[[i]],
                       symbol = symbol[[i]], show = show[[i]]))
}


plot_plotly_series_replicate <- function(x, y, ..., showlegend = TRUE) {
  if (!is.matrix(y) && length(y) == length(x)) {
    y <- matrix(y, ncol = 1)
  }
  lapply(seq_len(ncol(y)), function(i)
    plot_plotly_series(x, y[, i], showlegend = showlegend && i == 1, ...))
}


plotly_series_compatible <- function(a, b) {
  length(a) == length(b) &&
    identical(vcapply(a, "[[", "name"), vcapply(b, "[[", "name"))
}


plotly_with_redraw <- function(series, previous, ...) {
  if (length(series) == 0) {
    action <- "draw"
    data <- NULL
  } else if (identical(series, previous)) {
    action <- "pass"
    data <- NULL
  } else if (plotly_series_compatible(series, previous)) {
    action <- "redraw"
    data <- list(x = unname(lapply(series, "[[", "x")),
                 y = unname(lapply(series, "[[", "y")))
  } else {
    action <- "draw"
    data <- plot_plotly(series, ...)
  }
  list(series = series,
       action = action,
       data = data)
}
