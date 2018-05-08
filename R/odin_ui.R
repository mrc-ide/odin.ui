##' Odin shiny app
##' @title Odin Shiny Application
##' @param model An odin model
##' @param default_time Default time
##' @export
odin_app <- function(model, default_time) {
  shiny::shinyApp(
    ui = odin_ui(),
    server = odin_server(model, default_time))
}


odin_server <- function(model, default_time) {
  force(model)
  force(default_time)

  sidebar <- odin_ui_sidebar(model, default_time)

  function(input, output, session) {
    output$model_parameters <- shiny::renderUI(sidebar$tags)

    output$result_plot <- shiny::renderPlot({
      if (input$go_button == 0L) {
        return()
      }
      shiny::isolate(
        run_model_and_plot(model,
                           get_pars(input, sidebar$pars),
                           get_time(input)))
    })
  }
}


odin_ui <- function() {
  shiny::shinyUI(shiny::fluidPage(
    shiny::titlePanel("odin ui"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("model_parameters"),
        shiny::actionButton("go_button", "Run model")),
      shiny::mainPanel(
        shiny::plotOutput("result_plot")
      )
    )
  ))
}


odin_ui_sidebar <- function(model, default_time) {
  pars <- odin_ui_parameters(model)
  els <- c(pars$tags,
           odin_ui_time(default_time))
  list(tags = els,
       pars = pars$nms)
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
    nms <- set_names(paste0("pars_", x$name), x$name)
    tags <- c(list(shiny::h2("Parameters")),
              unname(Map(shiny::numericInput,
                         nms, x$name, x$default_value)))
  } else {
    nms <- character()
    tags <- list()
  }

  list(tags = tags, nms = nms)
}


odin_ui_time <- function(default_time) {
  if (length(default_time) == 1L) {
    default_time <- c(0, default_time)
  } else if (length(default_time) != 2L) {
    stop("default_time must be length 1 or 2")
  }
  list(shiny::h2("Time"),
       shiny::numericInput("time_start", "start", default_time[[1L]]),
       shiny::numericInput("time_end", "end", default_time[[2L]]))
}


## This is going to be subject to lots of change!  We'll want to split
## apart the generation from the plotting for sure.
##
## time detail (critical points, number of points to plot)
##
## Multiple plots of output - with different output variables being
## put onto different plots stacked above each other.
##
## Per line colour
##
## Axes labels
run_model_and_plot <- function(model, pars, time) {
  mod <- model(user = pars)
  y <- mod$run(time)
  op <- graphics::par(mar = c(4.6, 4.6, .1, .1))
  on.exit(graphics::par(op))
  graphics::matplot(y[, 1, drop = TRUE], y[, -1, drop = FALSE],
                    type = "l", lty = 1, xlab = "Time", ylab = "Variable",
                    las = 1)
}


get_pars <- function(x, map) {
  ret <- lapply(map, function(el) x[[el]])
  ret[lengths(ret) != 0L]
}


get_time <- function(x) {
  seq(x$time_start, x$time_end, length.out = 101)
}
