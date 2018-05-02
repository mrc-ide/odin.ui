##' Odin shiny app
##' @title Odin Shiny Application
##' @param model An odin model
##' @param default_time Default time
##' @export
odin_app <- function(model, default_time) {
  shiny::shinyApp(
    ui = odin_ui(model, default_time),
    server = odin_server)
}


odin_server <- function(input, output, session) {
  output$result_plot <- shiny::renderPlot({
    x <- stats::rnorm(100)
    y <- stats::rnorm(100)
    graphics::plot(x, y, asp = 1)
  })
}


odin_ui <- function(model, default_time) {
  shiny::shinyUI(shiny::fluidPage(
    shiny::titlePanel("odin ui"),

    shiny::sidebarLayout(
      odin_ui_sidebar(model, default_time),
      shiny::mainPanel(
        shiny::plotOutput("result_plot")
      )
    )
  ))
}


odin_ui_sidebar <- function(model, default_time) {
  els <- c(odin_ui_parameters(model),
           odin_ui_time(default_time))
  do.call(shiny::sidebarPanel, unname(els), quote = TRUE)
}


odin_ui_parameters <- function(model) {
  x <- stats::coef(model)

  if (!all(x$model$rank)) {
    stop("Only scalar inputs are currently supported")
  }
  if (!all(x$model$has_default)) {
    stop("All inputs must have defaults")
  }

  ## TODO: can we have a real list structure here?
  if (nrow(x) > 0L) {
    pars <- c(list(shiny::h2("Parameters")),
              unname(Map(shiny::numericInput,
                         paste0("pars_", x$name), x$name, x$default_value)))
  } else {
    pars <- NULL
  }

  pars
}


odin_ui_time <- function(default_time) {
  if (length(default_time) == 1L) {
    default_time <- c(0, default_time)
  } else {
    ## assume default_time is a 1 or 2 element numeric
  }
  list(shiny::h2("Time"),
       shiny::numericInput("time_start", "start", default_time[[1L]]),
       shiny::numericInput("time_end", "start", default_time[[2L]]))
}
