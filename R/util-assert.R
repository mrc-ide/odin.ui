assert_file_exists <- function(x, name = "File") {
  err <- !file.exists(x)
  if (any(err)) {
    msg <- sprintf("'%s'", x[err])
    stop(sprintf("%s does not exist: %s", name, paste(msg, collapse = ", ")),
         call. = FALSE)
  }
}
