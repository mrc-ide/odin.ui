run_model <- function(model, pars, time) {
  mod <- model(user = pars)$run(time)
}
