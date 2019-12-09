env <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  env$ace_needs_ns <- packageVersion("shinyAce") <= numeric_version("0.4.0")
}
