compare_union_metadata <- function(a, b, names) {
  ret <- rbind(b, a[!(a$name %in% b$name), , drop = FALSE])
  rownames(ret) <- NULL
  i <- 4 - (ret$name %in% b$name) - (ret$name %in% a$name) * 2
  lvls <- c("Shared", paste(names$long, "only"))
  ret$group <- factor(lvls[i], lvls)
  ret
}


compare_configuration <- function(model1, model2, run_options = NULL) {
  if (!isTRUE(model1$success) || !isTRUE(model2$success)) {
    return(NULL)
  }

  model <- list(model1, model2)
  cfg <- lapply(model, common_model_data_configuration,
                NULL, NULL, run_options)
  names <- list(long = vcapply(model, "[[", "name"),
                short = vcapply(model, "[[", "name_short"))

  pars <- compare_union_metadata(model1$info$pars, model2$info$pars, names)
  vars <- compare_union_metadata(model1$info$vars, model2$info$vars, names)

  ## Show/Hide/Disable is based off of model2 so port that back into
  ## model1 here
  shd <- c("show", "hide", "disable", "include")
  cfg[[1]]$vars[shd] <- vars[match(cfg[[1]]$vars$name, vars$name), shd]

  cols <- odin_colours(vars$name, NULL, NULL)

  for (i in seq_along(cfg)) {
    cfg[[i]]$cols <- cols
  }

  download_names <- download_names(
    display = c(names$long, "Parameters"),
    filename = c(names$short, "parameters"),
    data = c("model1", "model2", "user"))

  list(data = NULL, configuration = cfg, link = NULL,
       pars = pars, vars = vars, cols = cols, names = names,
       download_names = download_names)
}


compare_download_names <- function(res, model_names) {
  display <- lapply(res, function(x) x$configuration$download_names$display)
  filename <- lapply(res, function(x) x$configuration$download_names$filename)
  data <- lapply(res, function(x)
    match(x$configuration$download_names$data, names(x$simulation)))

  n <- lengths(display)
  i <- rep(seq_along(n), n)
  download_names(
    display = sprintf("%s (%s)", unlist(display), model_names$long[i]),
    filename = sprintf("%s-%s", unlist(filename), model_names$short[i]),
    data = Map2(c, i, unlist(data)))
}


compare_configuration_save <- function(configuration) {
  if (is.null(configuration)) {
    return(NULL)
  }
  configuration$configuration <- lapply(
    configuration$configuration,
    common_model_data_configuration_save)
  configuration
}


compare_configuration_restore <- function(configuration) {
  if (is.null(configuration)) {
    return(NULL)
  }
  configuration$configuration <- lapply(
    configuration$configuration,
    common_model_data_configuration_restore)
  configuration
}
