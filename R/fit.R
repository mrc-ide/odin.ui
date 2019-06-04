## Create a closure that captures the model to data fit for a single
## response variable.
##
## @param data a data frame
## @param name_time the name of the time variable within the data
## @param name_data the name of the response variable within the data
## @param name_modelled the name of the response variable within the model
## @param a function that will take two identical length vectors and
##   return a scalar goodness of fit measurement (smaller is better)
make_compare <- function(data, name_time, name_data, name_modelled, compare) {
  real <- data[[name_data]]
  compare <- match.fun(compare)
  force(name_modelled)
  function(modelled) {
    compare(modelled[, name_modelled], real)
  }
}


## Create a closure that can be used for optimising a model fit
##
## @param model the odin model
##
## @param model coefficient information (coef(model)), augmented with
##   a column "vary" that is TRUE for values to be varied and "value",
##   which is the parameter value to use for non-varying parameters,
##   defaulting to default_value
##
## @param vector of times (greater than or equal to zero) that the
##   model should stop at
##
## @param compare a comparison function as created by make_compare
##
## @callback a callback function (e.g., print_every)
make_target <- function(model, coef, time, compare, callback = NULL) {
  user <- setNames(coef$value %||% coef$default_value, coef$name)
  mod <- model(user = as.list(user))
  nms <- coef$name[coef$vary]

  function(p) {
    mod$set_user(user = setNames(as.list(p), nms))
    y <- mod$run(c(0, time))[-1, , drop = FALSE]
    res <- compare(y)
    if (!is.null(callback)) {
      callback(p, res)
    }
    res
  }
}


## Fit a model
## @param target a target function to optimise, as created by make_target
## @param paramter information (as in make_target)
## @param tolerance to be passed to the underlying method
## @param method optimisation method
fit_model <- function(target, coef, tolerance = 1e-4, method = "optim") {
  control <- list(factr = tolerance, pgtol = tolerance)
  i <- coef$vary

  start <- unlist(coef$value[i])
  lower <- coef$min[i]
  upper <- coef$max[i]
  fit <- switch(
    method,
    optim = do_fit_optim(start, target, tolerance, lower, upper),
    subplex = do_fit_subplex(start, target, tolerance),
    hjkb = do_fit_hjkb(start, target, tolerance, lower, upper),
    nmkb = do_fit_nmkb(start, target, tolerance, lower, upper),
    stop("Unknown method ", method))

  coef$value[i] <- fit$par
  fit$coef <- coef
  fit
}


do_fit_optim <- function(start, target, tolerance, lower, upper) {
  control <- list(factr = tolerance, pgtol = tolerance)
  optim(start, target, lower = lower, upper = upper,
        method = "L-BFGS-B", control = control)
}


do_fit_subplex <- function(start, target, tolerance) {
  control <- list(reltol = tolerance)
  subplex::subplex(start, protect(target), control)
}


do_fit_hjkb <- function(start, target, tolerance, lower, upper) {
  control <- list(tol = tolerance)
  dfoptim::hjkb(start, target, lower, upper, control)
}


do_fit_nmkb <- function(start, target, tolerance, lower, upper) {
  control <- list(tol = tolerance)
  dfoptim::nmkb(start, target, lower, upper, control)
}


protect <- function(fun, fail = Inf) {
  function(...) {
    tryCatch(fun(...), error = function(e) fail)
  }
}


print_every <- function(every) {
  i <- 0L
  function(p, res) {
    if (i %% every == 0L) {
      message(sprintf("{%s} => %s", paste(p, collapse = ", "), res))
    }
    i <<- i + 1L
  }
}


plot_fit <- function(data, name_time, name_data, model_output, name_model,
                     cols) {
  p <- plotly::plot_ly()
  p <- plotly::config(p, collaborate = FALSE, displaylogo = FALSE)
  for (i in name_model) {
    p <- plotly::add_lines(p, x = model_output[, "t"], y = model_output[, i],
                           name = i, line = list(color = cols[[i]]))
  }

  data_time <- data[[name_time]]
  for (i in name_data) {
    j <- !is.na(data[[i]])
    p <- plotly::add_markers(p, x = data_time[j], y = data[[i]][j], name = i,
                             marker = list(color = cols[[i]]))
  }

  p
}


plot_data <- function(data, name_time, name_vars, cols) {
  p <- plotly::plot_ly()
  p <- plotly::config(p, collaborate = FALSE, displaylogo = FALSE)
  data_time <- data[[name_time]]
  for (i in seq_along(name_vars)) {
    nm <- name_vars[[i]]
    y <- data[[nm]]
    j <- !is.na(y)
    p <- plotly::add_markers(p, x = data_time[j], y = y[j], name = nm,
                             marker = list(color = cols[[i]]))
  }
  p
}


compare_sse <- function(modelled, real) {
  sum((modelled - real)^2, na.rm = TRUE)
}
