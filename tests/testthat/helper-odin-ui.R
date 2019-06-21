mock_model <- function(pars) {
  force(pars)
  ret <- function() {}
  class(ret) <- "odin_generator"
  attr(ret, "user_info") <- function() pars
  ret
}


example_data_fit <- function() {
  code <- c("deriv(x) <- b",
            "initial(x) <- a",
            "a <- user(3, min = 0)",
            "b <- user(min = 0, max = 2)",
            "deriv(y) <- 2",
            "initial(y) <- b",
            "initial(z) <- 3",
            "deriv(z) <- 3")
  model <- common_odin_compile_from_code(code)

  ## some actual data to fit to for a:
  t <- 1:10
  ## round(2 + (rnorm(10) * 2 + t), 3)
  a <- c(1.75, 4.37, 3.33, 9.19, 7.66, 6.36, 9.97, 11.48, 12.15, 11.39)
  d <- data.frame(t = 1:10, a = a, b = runif(10), c = runif(10))
  data <- odin_data_source(d, "file.csv", "t")
  link <- link_result(list(a = "x", c = "y"))
  list(model = model, data = data, link = link,
       configuration = fit_configuration(model, data, link))
}
