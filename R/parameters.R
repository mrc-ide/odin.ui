parameter_plot <- function(dat) {
  out <- dygraphs::dygraph(
    dat, xlab = names(dat)[[1]],
    ylab = if (ncol(dat) == 2L) names(dat)[[2]] else NULL)
  out <- dygraphs::dyOptions(out,
                             strokeWidth = 1,
                             labelsKMB = TRUE,
                             animatedZooms = TRUE,
                             drawGrid = FALSE)
  out
}
