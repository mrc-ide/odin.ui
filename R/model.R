model_info <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  metadata <- odin::odin_ir(model, TRUE)
  variables <- names(metadata$data$variable$contents)
  ## TODO: this could be dealt with more nicely, but we need to remove
  ## any manually computed time here from discrete models.
  output <- setdiff(names(metadata$data$output$contents), "time")
  d <- metadata$data$elements[c(variables, output)]
  rank <- vapply(d, "[[", integer(1), "rank", USE.NAMES = FALSE)
  type <- rep("variable", length(d))
  type[names(d) %in% output] <- "output"
  vars <- data_frame(name = names(d), rank = rank, type = type)

  list(pars = stats::coef(model), vars = vars, features = metadata$features)
}


discrete_times <- function(t_end, t_n, dt) {
  by <- ceiling(t_end / dt / t_n)
  step_end <- ceiling(t_end / dt)
  union(seq(0, step_end, by = by), step_end)
}
