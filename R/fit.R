do_fit <- function(start, target, lower, upper, tolerance, method) {
  control <- list(factr = tolerance, pgtol = tolerance)
  t0 <- Sys.time()
  res <- switch(
    method,
    optim = do_fit_optim(start, target, tolerance, lower, upper),
    subplex = do_fit_subplex(start, target, tolerance),
    hjkb = do_fit_hjkb(start, target, tolerance, lower, upper),
    stop("Unknown method ", method))
  t1 <- Sys.time()
  ## only really interested in wall time here:
  res$elapsed <- as.numeric(t1 - t0, "secs")
  res
}


do_fit_optim <- function(start, target, tolerance, lower, upper) {
  control <- list(factr = tolerance, pgtol = tolerance)
  res <- stats::optim(start, target, lower = lower, upper = upper,
                      method = "L-BFGS-B", control = control)
  list(par = res$par,
       value = res$value,
       success = res$convergence == 0,
       message = res$message,
       evaluations = res$counts[[1]] + 2 * res$counts[[1]] * length(start))
}


do_fit_subplex <- function(start, target, tolerance) {
  control <- list(reltol = tolerance)
  res <- subplex::subplex(start, protect(target), control)
  list(par = res$par,
       value = res$value,
       success = res$convergence == 0,
       message = res$message,
       evaluations = res$counts)
}


do_fit_hjkb <- function(start, target, tolerance, lower, upper) {
  control <- list(tol = tolerance)
  res <- dfoptim::hjkb(start, target, lower, upper, control)
  list(par = res$par,
       value = res$value,
       success = res$convergence == 0,
       message = "Optimisation completed",
       evaluations = res$feval)
}


compare_sse <- function(modelled, real) {
  sum((modelled - real)^2, na.rm = TRUE)
}


odin_fit_model <- function(data_t, data_y, model, name_model_y, user, vary,
                           lower, upper, method = "subplex",
                           compare = compare_sse, tolerance = 1e-6) {
  if (any(is.na(user))) {
    stop(sprintf(
      "Starting parameter value needed for %s",
      paste(names(user)[is.na(user)], collapse = ", ")))
  }

  if (length(vary) == 0L) {
    stop("Select at least one parameter to vary")
  }

  objective <- odin_fit_objective(data_t, data_y, model, name_model_y,
                                  user, vary, compare)
  start <- list_to_numeric(user[vary], TRUE)
  value <- do_fit(start, objective, lower, upper,
                  tolerance = tolerance, method = method)

  user[vary] <- value$par
  value$user <- as.list(user)

  value
}


odin_fit_objective <- function(data_t, data_y, model, name_model_y,
                               user, vary, compare) {
  compare <- match.fun(compare)
  mod <- model(user = as.list(user))
  t_after_zero <- data_t[[1]] > 0

  if (t_after_zero) {
    data_t <- c(0, data_t)
  }

  function(p) {
    mod$set_user(user = set_names(as.list(p), vary))
    y <- mod$run(data_t)[, name_model_y]
    if (t_after_zero) {
      y <- y[-1]
    }
    compare(y, data_y)
  }
}
