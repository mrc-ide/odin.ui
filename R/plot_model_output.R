
## Internal function generating the plotting output.

## - 'xy' is a matrix whose first column is always time, but with varying labels
## ('t' or 'step')

## 'include' is a vector of names of outputs to be included in the plot, matched
## against the columns of xy[, -1]

## 'cols' is a vector of colors corresponding to 'include'



plot_model_output <- function(xy, include, cols) {

  df <- as.data.frame(xy[, c(TRUE, include), drop = FALSE])

  out <- dygraphs::dygraph(df)

  out
}
