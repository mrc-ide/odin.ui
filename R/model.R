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


run_model <- function(generator, pars, time, replicates, extra, time_scale) {
  if (length(pars) > 0L) {
    model <- generator(user = pars)
  } else {
    model <- generator()
  }

  if (!is.null(time_scale)) {
    dt <- model$contents()[[time_scale]]
    time$start <- time$start / dt
    time$end   <- time$end / dt
  }

  if (time$discrete) {
    t <- seq(time$start, time$end, by = round(time$detail))
    if (t[[length(t)]] < time$end) {
      t <- c(t, time$end)
    }
  } else {
    t <- seq(time$start, time$end, length.out = round(time$detail))
  }

  if (is.null(replicates)) {
    output <- model$run(t)
  } else {
    output <- model$run(t, replicate = replicates)
  }

  ## Rescale time
  if (!is.null(time_scale)) {
    t <- t * dt
    if (is.null(replicates)) {
      output[, 1] <- t
    } else {
      output[, 1, ] <- t
    }
  }

  output_expanded <- model$transform_variables(output)
  if (!is.null(extra)) {
    for (i in names(extra)) {
      output_expanded[[i]] <- extra[[i]](output_expanded)
    }
  }
  list(model = model, pars = pars, time = t, replicates = replicates,
       output = output, output_expanded = output_expanded)
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
## what is needed.  Part of the issue here is controlling when the
## error is thrown so that the app feeds back the error information in
## the most useful way.
validate_model_parameters <- function(model, parameters) {
  p <- stats::coef(model)

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


## This requires quite a lot of work really; there are lots of moving
## parts here.
run_model_parameters <- function(generator, target, values, common, time,
                                 replicates, extra, time_scale, collect,
                                 callback = NULL) {
  n <- length(values)
  ret <- vector("list", n)
  pars <- common
  for (i in seq_len(n)) {
    pars[[target]] <- values[[i]]
    res <- run_model(generator, pars, time, replicates, extra, time_scale)
    ret[[i]] <- collect(res$output_expanded)
    if (!is.null(callback)) {
      callback(i, n)
    }
  }

  ## This is not wonderful but will get things more or less sorted.
  len <- lengths(ret)
  stopifnot(all(len == len[[1]]))
  m <- matrix(unlist(ret, use.names = FALSE), n)
  colnames(m) <- names(ret[[1]])
  as.data.frame(cbind(matrix(values, dimnames = list(NULL, target)), m))
}


validate_extra <- function(extra, metadata) {
  if (!is.null(extra)) {
    assert_named(extra)
    if (!all(vlapply(extra, is.function))) {
      stop("All elements of 'extra' must be functions", call. = FALSE)
    }
    ## This is possibly not ideal but it's easy and will do for now:
    if (any(names(extra) %in% names(metadata$data$elements))) {
      stop("Names in 'extra' collide with model names")
    }
  }
  extra
}


validate_time_scale <- function(time_scale, metadata) {
  if (is.null(time_scale)) {
    return(NULL)
  }
  stop("fix metadata handling in validate_time_scale")
  ## Here, look to the the components to sort this out
  nodes <- metadata$nodes
  if (!(time_scale %in% nodes$id)) {
    stop(sprintf("'time_scale' value '%s' is not in model", time_scale),
         call. = FALSE)
  }
  ## time_scale_stage <- nodes$stage[nodes$id == time_scale]
  ## if (!(time_scale_stage %in% c("constant", "user"))) {
  ##   stop(sprintf("'time_scale' value '%s' is not constant or user-supplied",
  ##                time_scale),
  ##        call. = FALSE)
  ## }
  if (nodes$rank[nodes$id == time_scale] > 0L) {
    stop(sprintf("'time_scale' value '%s' is not scalar", time_scale),
         call. = FALSE)
  }
  time_scale
}


## organise the model metadata that we'll use all over the show
model_metadata <- function(model) {
  ir <- odin::odin_ir(model, TRUE)
  ir
}


model_info <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  metadata <- odin::odin_ir(model, TRUE)
  variables <- names(metadata$data$variable$contents)
  output <- names(metadata$data$output$contents)
  d <- metadata$data$elements[c(variables, output)]
  rank <- vapply(d, "[[", integer(1), "rank", USE.NAMES = FALSE)
  type <- rep("variable", length(d))
  type[names(d) %in% output] <- "output"
  vars <- data_frame(name = names(d), rank = rank, type = type)

  list(pars = coef(model), vars = vars)
}
