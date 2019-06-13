mock_model <- function(pars) {
  force(pars)
  ret <- function() {}
  class(ret) <- "odin_generator"
  attr(ret, "user_info") <- function() pars
  ret
}
