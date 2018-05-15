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
