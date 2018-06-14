assert_file_exists <- function(x, name = "File") {
  err <- !file.exists(x)
  if (any(err)) {
    msg <- sprintf("'%s'", x[err])
    stop(sprintf("%s does not exist: %s", name, paste(msg, collapse = ", ")),
         call. = FALSE)
  }
}


assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}


assert_has_fields <- function(x, required, optional,
                              name = deparse(substitute(x))) {
  msg <- setdiff(required, names(x))
  if (length(msg) > 0L) {
    stop(sprintf("Fields missing from %s: %s",
                 name, paste(msg, collapse = ", ")))
  }
  extra <- setdiff(names(x), c(required, optional))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown fields in %s: %s",
                 name, paste(extra, collapse = ", ")))
  }
}
