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
