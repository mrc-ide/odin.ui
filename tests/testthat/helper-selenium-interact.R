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


docker_list_files <- function(container, path) {
  res <- container$exec(c("ls", "-1", path), stream = FALSE)
  if (res$exit_code != 0L) {
    stop("Error listing directory")
  }
  output <- format(res$output, filter = "stdout", style = "plain")
  unlist(strsplit(output, "\n", fixed = TRUE))
}


download_file <- function(element, dat) {
  container <- dat$container
  download_dir <- dat$download_dir

  prev <- docker_list_files(container, download_dir)
  files <- character(0)

  appeared <- function() {
    files <<- setdiff(docker_list_files(container, download_dir), prev)
    length(files) > 0L
  }
  element$clickElement()
  retry(appeared, "downloaded file appeared")
  filename <- file.path(download_dir, files)

  size <- 0L
  finished <- function() {
    prev <- size
    size <<- container$path_stat(filename)$size
    size == prev
  }

  retry(finished, "file finished downloading")
  tmp <- tempfile()
  container$cp_out(filename, tmp)
  list(local = tmp,
       remote = filename)
}
