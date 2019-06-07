## Create a selenium driver (if we have not already done so), skipping
## gracefully if it is not possible
selenium_driver <- function() {
  testthat::skip_on_cran()
  required <- identical(Sys.getenv("SHINY_REQUIRE_SELENIUM", ""), "true")
  if (!required) {
    testthat::skip_if_not_installed("RSelenium")
    ## No point running a test if we can't launch the application either
    testthat::skip_if_not_installed("callr")
  }
  tryCatch({
    dr <- RSelenium::remoteDriver()
    dr$open(silent = TRUE)
    dr$maxWindowSize()
    dr
  }, error = function(e) {
    if (required) {
      stop(e)
    } else {
      testthat::skip(e$message)
    }
  })
}


path_remote <- function(path) {
  file.path("/host", path, fsep = "/")
}


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


## This version is totally built around our demo app - we'll need to
## generalise this later.
launch_app <- function(driver) {
  port <- get_free_port()
  args <- list(port)

  process <- callr::r_bg(driver, args = args)

  url <- paste0("http://localhost:", port)
  ## It will take a short while before the server is responsive to
  ## connections, so we'll poll here until there's a successful read
  ## from this port.
  retry_until_server_responsive(url)

  list(process = process, port = port, url = url)
}


## Generic retry function - evaluate the function 'f' with no
## arguments with a break of 'poll' between evaluations until it
## returns TRUE, or until 'timeout' seconds, in which case throw an
## error (using the string 'description' to make it informative.
## Typically we will use wrappers around this.
retry <- function(f, explanation, timeout = 5, poll = 0.1) {
  give_up <- Sys.time() + timeout
  repeat {
    if (f()) {
      break
    }
    if (Sys.time() > give_up) {
      stop(sprintf("Timeout exceeded before %s", explanation))
    }
    Sys.sleep(poll)
  }
}


## Evaluate the function 'f' until it does not throw an error.
retry_until_no_error <- function(f, ...) {
  g <- function() {
    tryCatch({
      suppressWarnings(f())
      TRUE
    }, error = function(e) FALSE)
  }
  retry(g, ...)
}


## Try and read from the shiny url until it becomes responsive.
retry_until_server_responsive <- function(url, ...) {
  retry_until_no_error(function() readLines(url),
                       "shiny server is responsive")
}


free_port <- function(start, max_tries = 20) {
  force(start)
  force(max_tries)
  function() {
    port <- find_free_port(start, max_tries)
    start <<- start + 1
    port
  }
}


find_free_port <- function(start, max_tries = 20) {
  port <- seq(start, length.out = max_tries)
  for (p in port) {
    if (check_port(p)) {
      return(p)
    }
  }
  stop(sprintf("Did not find a free port between %d..%d",
               min(port), max(port)),
       call. = FALSE)
}


check_port <- function(port) {
  timeout <- 0.1
  con <- tryCatch(suppressWarnings(socketConnection(
    "localhost", port = port, timeout = timeout, open = "r")),
    error = function(e) NULL)
  if (is.null(con)) {
    return(TRUE)
  }
  close(con)
  FALSE
}


element_exists <- function(driver, value, using = "id") {
  length(driver$findElements(using, value)) > 0
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


get_free_port <- free_port(8000)
