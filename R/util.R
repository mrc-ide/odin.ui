set_names <- function(x, nms) {
  names(x) <- nms
  x
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
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
  addResourcePath('images', system.file('images', package='odin.ui'))
  shiny::tags$footer(class="navbar-fixed-bottom odin-footer",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(12,
          shiny::img(src = "images/reside-small.png"),
          shiny::span(class="logo", "RESIDE@IC"))
      )
    )
  )
}

odin_css <- function() {
  shiny::includeCSS(odin_ui_file("css/styles.css"))
}
