run_model <- function(model, pars, time) {
  mod <- model(user = pars)$run(time)
}


plot_model_output <- function(xy, include, cols) {
  op <- graphics::par(mar = c(4.6, 4.6, .1, .1))
  on.exit(graphics::par(op))

  x <- xy[, 1, drop = TRUE]

  if (any(include)) {
    y <- xy[, names(include)[include], drop = FALSE]
    graphics::matplot(x, y, type = "l", lty = 1, las = 1, col = cols[include],
                      xlab = "Time", ylab = "Variable")
  } else {
    graphics::plot(x, x, type = "n", las = 1, yaxt = "n",
                   xlab = "Time", ylab = "Variable")
    pos <- mean(range(x))
    graphics::text(pos, pos, "(nothing to plot)")
  }
}
