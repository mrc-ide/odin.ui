set_names <- function(x, nms) {
  names(x) <- nms
  x
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
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


odin_ui_file <- function(path) {
  system.file(path, package = "odin.ui", mustWork = TRUE)
}


read_text <- function(filename) {
  readChar(filename, file.size(filename))
}


run_app <- function(app, run, ...) {
  if (run) {
    shiny::runApp(app, ...)
  } else {
    app
  }
}


write_csv <- function(data, filename) {
  utils::write.csv(data, filename, row.names = FALSE)
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


callback_shiny_progress <- function(i, n) {
  shiny::incProgress(1, detail = sprintf("%d / %d", i, n))
}

odin_footer <- function() {
  shiny::addResourcePath('images', system.file('images', package='odin.ui'))
  shiny::tags$footer(class="odin-footer",
    shiny::div(class = "logo-wrapper",
      shiny::img(src = "images/reside-logo-small.png", height = 60),
      shiny::div("RESIDE@IC"))
  )
}

odin_css <- function() {
  shiny::includeCSS(odin_ui_file("css/styles.css"))
}


yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full (true|yes) / (false|no):
  handlers <- list(
    "bool#yes" = function(x)
      if (tolower(x) %in% c("true", "yes")) TRUE else x,
    "bool#no" = function(x)
      if (tolower(x) %in% c("false", "no")) FALSE else x)
  yaml::yaml.load(string, handlers = handlers)
}


yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call. = FALSE)
  }
  tryCatch(yaml_load(read_text(filename)),
           error = catch_yaml)
}


read_csv <- function(filename) {
  utils::read.csv(filename, stringsAsFactors = FALSE, check.names = FALSE)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


list_to_character <- function(x, named = FALSE) {
  vcapply(x, identity, USE.NAMES = named)
}


list_to_numeric <- function(x, named = FALSE) {
  vnapply(x, identity, USE.NAMES = named)
}


list_to_logical <- function(x, named = FALSE) {
  vlapply(x, identity, USE.NAMES = named)
}


date_string <- function(time = Sys.time()) {
  format(time, "%Y%m%d-%H%M%S")
}


ensure_extension <- function(path, ext) {
  re <- paste0("\\.", ext, "$")
  if (!grepl(re, path, ignore.case = TRUE)) {
    path <- paste0(path, ".", ext)
  }
  path
}


list_to_df <- function(x) {
  data_frame(name = names(x), value = list_to_numeric(x))
}


df_to_list <- function(x) {
  set_names(as.list(x$value), x$name)
}


constrain <- function(x, min, max) {
  min(max(x, min), max)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


is_missing <- function(x) {
  is.null(x) || length(x) == 0 || identical(x, "") ||
    (length(x) == 1 && is.na(x))
}


combine_colwise <- function(x, fmt = "%s (%d)") {
  for (i in seq_along(x)) {
    colnames(x[[i]]) <- sprintf(fmt, colnames(x[[i]]), i)
  }
  do.call("cbind", x)
}


expand_and_name <- function(x, nms) {
  if (is.null(names(x))) {
    if (length(x) == 1) {
      x <- set_names(rep(x, length(nms)), nms)
    } else if (length(x) == length(nms)) {
      names(x) <- nms
    }
  }
  x
}


with_success <- function(expr) {
  res <- tryCatch(
    force(expr),
    error = identity)
  if (inherits(res, "error")) {
    unsuccessful(res$message)
  } else {
    successful(res)
  }
}


successful <- function(value) {
  list(success = TRUE, value = value, error = NULL)
}


unsuccessful <- function(error) {
  list(success = FALSE, value = NULL, error = error)
}


protect <- function(fun, fail = Inf) {
  function(...) {
    tryCatch(fun(...), error = function(e) fail)
  }
}


clean_name <- function(x) {
  gsub(" ", "-", tolower(x))
}


accept_csv <- function() {
  c("text/csv",
    "text/comma-separated-values,text/plain",
    ".csv")
}


has_function <- function(x, name) {
  is.function(x[[name]])
}


names_if <- function(x) {
  names(x)[x]
}


package_version <- function(name) {
  utils::packageVersion(name)
}


rbind_laxly <- function(a, b) {
  v <- union(colnames(a), colnames(b))
  res <- rbind(pad_matrix(a, v), pad_matrix(b, v))
  rownames(res) <- NULL
  res
}


pad_matrix <- function(m, v) {
  msg <- setdiff(v, colnames(m))
  extra <- matrix(NA, nrow(m), length(msg), dimnames = list(NULL, msg))
  cbind(m, extra)[, v, drop = FALSE]
}


strip_linefeed <- function(x) {
  gsub("\\r", "", x)
}
