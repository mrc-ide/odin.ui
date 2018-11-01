## 'options' is a list containing the following names elements:

## - 'xy' is a matrix whose first column is always time, but with varying labels
## ('t' or 'step')

## - 'include' is a named logical vector indicating which variables should be
## - included in the plot, matched against the columns of xy[, -1]

## - 'col' is a named vector of colours to be used for plotting the variables,
## matched against the columns of xy[, -1]

## - 'line_width' is a fixed line width

## - 'fill' is a logical (TRUE = region under lines is filled)

## - 'alpha' is the transparency used in the plot

## Note: we purposedly avoid the use of a piping operator here.

plot_model_output_single <- function(xy, options) {
  if (is.null(xy)) {
    return(NULL)
  }
  ## Generation of the basic plot
  include <- options$include
  var_to_keep <- names(include)[include]
  time <- round_time(xy$t)
  df <- as.data.frame(c(list(time = time), xy[var_to_keep]))
  out <- dygraphs::dygraph(df)

  ## colours need to be unnamed; we also add transparency
  cols <- options$cols[var_to_keep]
  cols <- unname(transp(cols, options$alpha))
  
  ## Customisation of the plot
  out <- dygraphs::dyOptions(out,
                             colors = cols,
                             strokeWidth = options$line_width,
                             fillGraph = options$fill,
                             fillAlpha = max(0, options$alpha - 0.1),
                             labelsKMB = TRUE,
                             stackedGraph = options$stack,
                             digitsAfterDecimal = 0,
                             animatedZooms = TRUE)
  out <- dygraphs::dyAxis(out, "x", label = "Time", drawGrid = FALSE)
  out <- dygraphs::dyAxis(out, "y", label = NULL, drawGrid = FALSE)
  out <- dygraphs::dyRangeSelector(out, strokeColor = "#00000099",
                                   fillColor = "#0000004D",
                                   height = 0)

  out <- dygraphs::dyHighlight(out,
                               highlightSeriesOpts = list(strokeWidth = 2),
                               highlightCircleSize = 3,
                               highlightSeriesBackgroundAlpha = 1,
                               hideOnMouseOut = TRUE)

  out <- dygraphs::dyLegend(out, show = "follow",
                            showZeroValues = TRUE,
                            labelsSeparateLines = TRUE,
                            hideOnMouseOut = TRUE)
  out <- dygraphs::dyCSS(out, odin_ui_file("css/dygraphs.css"))
  out
}


plot_model_output_replicates <- function(xy, graph_options) {
  include <- names(which(graph_options$include))
  cols <- graph_options$cols
  mean <- character(0)
  interval <- list()
  dygraph_multi(xy, include, cols, mean, interval)
}
