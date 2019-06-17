do_fit <- function(start, target, lower, upper, tolerance, method) {
  control <- list(factr = tolerance, pgtol = tolerance)
  t0 <- Sys.time()
  res <- switch(
    method,
    ## optim = do_fit_optim(start, target, tolerance, lower, upper),
    ## subplex = do_fit_subplex(start, target, tolerance),
    ## hjkb = do_fit_hjkb(start, target, tolerance, lower, upper),
    nmkb = do_fit_nmkb(start, target, tolerance, lower, upper),
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
       evaluations = res$count)
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


do_fit_nmkb <- function(start, target, tolerance, lower, upper) {
  control <- list(tol = tolerance)
  res <- dfoptim::nmkb(start, target, lower, upper, control)
  list(par = res$par,
       value = res$value,
       success = res$convergence == 0,
       message = res$message,
       evaluations = res$feval)
}


compare_sse <- function(modelled, real) {
  sum((modelled - real)^2, na.rm = TRUE)
}
