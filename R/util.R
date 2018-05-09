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



load_pkg <- function(x) {
  if (!suppressWarnings(require(x))) {
    stop(sprintf("package '%s' is needed by the app but missing", x))
  }
}

