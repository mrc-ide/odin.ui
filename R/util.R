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


cols <- function(n) {
  ## TODO: replace this with something better
  pos <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
           "#A65628", "#F781BF", "#999999")
  if (n > length(pos)) {
    stop("too many colours requested")
  }
  pos[seq_len(n)]
}


## TODO: we'll need to decide how to handle time once time outputs have been
## generalised in odin.
round_time <- function(x) {
  round(x, 2L)
}
