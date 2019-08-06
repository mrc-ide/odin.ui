launch_prototype <- function() {
  launch_app(function(port) {
    options(error = traceback)
    app <- odin.ui:::odin_prototype(character(0))
    shiny::runApp(app, port = port)
  })
}


launch_csv <- function() {
  launch_app(function(port) {
    options(error = traceback)
    app <- odin.ui:::odin_ui_csv_app()
    shiny::runApp(app, port = port)
  })
}


retry_until_element_exists <- function(driver, value, using = "id", ...) {
  retry(function() element_exists(driver, value, using),
        sprintf("Searching for element %s = %s", using, value))
  driver$findElement(using, value)
}


expect_with_retry <- function(expectation, fn, ..., timeout = 5, poll = 0.1) {
  give_up <- Sys.time() + timeout
  repeat {
    res <- tryCatch(
      expectation(fn(), ...),
      error = identity)
    if (!inherits(res, "error")) {
      return(invisible(res))
    }
    if (Sys.time() > give_up) {
      stop(res)
    }
    Sys.sleep(poll)
  }
}


element_exists <- function(driver, value, using = "id") {
  length(driver$findElements(using, value)) > 0
}


path_remote <- function(path) {
  file.path("/host", path, fsep = "/")
}


nth_tab <- function(dr, n, id = "odin_ui_navbar") {
  xpath <- sprintf('//ul[@id="%s"]/li[%d]', id, n)
  el <- dr$findElement("xpath", xpath)
  el
}
