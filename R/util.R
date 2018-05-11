set_names <- function(x, nms) {
  names(x) <- nms
  x
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


drop_null <- function(x) {
  x[!vlapply(x, is.null)]
}


## TODO: we'll need to decide how to handle time once time outputs have been
## generalised in odin.
round_time <- function(x) {
  round(x, 2L)
}


## make a colour transparent
transp <- function (col, alpha = 0.5) {
  col_rgb <- grDevices::col2rgb(col) / 255
  grDevices::rgb(col_rgb[1, ], col_rgb[2, ],col_rgb[3, ], alpha)
}
