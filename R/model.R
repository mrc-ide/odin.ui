## This helper probably does belong in odin.  The first part though
## converts the
compile_model <- function(code, dest = NULL, safe = FALSE, skip_cache = FALSE) {
  ## TODO: this needs to capture all messages because there are
  ## informational ones there that are not used
  elapsed <- system.time(
    output <- utils::capture.output(
      suppressMessages(
        model <- tryCatch(
          odin::odin_(code, dest, verbose = TRUE,
                      compiler_warnings = FALSE,
                      skip_cache = skip_cache),
          error = identity))))

  is_error <- inherits(model, "error")

  if (is_error) {
    error <- model$message
    model <- NULL
  } else {
    error <- NULL
  }

  ## TODO: format the compiler output - in particular the compiler
  ## invocation bits are not very interesting and quite long, but the
  ## rest should be coloured up.

  list(
    success = !is_error,
    elapsed = elapsed,
    output = output,
    model = model,
    error = error)
}


run_model <- function(generator, pars, time) {
  if (length(pars) > 0L) {
    model <- generator(user = pars)
  } else {
    model <- generator()
  }
  list(model = model, output = model$run(time))
}


write_model_data <- function(data, filename, format) {
  if (is.null(format) || format == "auto") {
    ext <- tolower(tools::file_ext(filename))
    format <- switch(ext, rds = "rds", json = "json", "csv")
  }

  switch(format,
         rds = saveRDS(data$output, filename),
         json = write_model_data_json(data$model, data$output, filename),
         write_csv(data$output, filename))
}


write_model_data_json <- function(model, data, filename) {
  out <- model$transform_variables(data)
  jsonlite::write_json(out, filename, digits = NA)
}


## There's really quite a lot to do here.  This will be fairly lenient
## at first and we'll get a more agressive version as it becomes clear
## what is needed.
validate_model_parameters <- function(model, parameters) {
  p <- coef(model)

  if (!all(p$rank == 0L)) {
    stop("Only scalar parameters are currently supported")
  }
  ## This can be relaxed now:
  if (!all(p$has_default)) {
    stop("All parameters must have defaults")
  }

  f <- function(nm) {
    x <- parameters[[nm]] %||% list()
    assert_has_fields(x, NULL, c("description", "default", "range"))

    ## There's lots of tedious validation here to do that I'm not
    ## bothering with for now.
    x$has_range <- !is.null(x$range)
    x$range_min <- x$range[[1L]]
    x$range_max <- x$range[[2L]]
    x$has_description <- !is.null(x$description)
    if (is.null(x$default)) {
      x$default <- p$default_value[p$name == nm][[1L]]
    }
    x$name <- nm
    x
  }

  set_names(lapply(p$name, f), p$name)
}
