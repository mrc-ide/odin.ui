
## Internal function generating the plotting output.
## Author: Thibaut Jombart, 2018

## - 'xy' is a matrix whose first column is always time, but with varying labels
## ('t' or 'step')

## - 'include' is a named logical vector indicating which variables should be
## - included in the plot, matched against the columns of xy[, -1]

## - 'cols' is a named vector of colors to be used for plotting the variables,
## matched against the columns of xy[, -1]

## Note: we purposedly avoid the use of a piping operator here.

plot_model_output <- function(xy, include, cols) {

  ## Generation of the basic plot
  var_to_keep <- names(include)[include]
  df <- cbind.data.frame(time = xy[, 1],
                         xy[, var_to_keep, drop = FALSE])
  out <- dygraphs::dygraph(df)


  ## Customisation of the plot

  out <- dygraphs::dyOptions(out, colors = unname(cols[var_to_keep]))
  out <- dygraphs::dyAxis(out, "x", label = "Time", drawGrid = FALSE)
  out <- dygraphs::dyAxis(out, "y", label = NULL, drawGrid = FALSE)
  out <- dygraphs::dyRangeSelector(out, strokeColor = "#00000099",
                                   fillColor = "#0000004D")

  out
}
