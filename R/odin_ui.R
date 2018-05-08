##' Odin shiny app
##' @title Odin Shiny Application
##' @param model An odin model
##' @param default_time Default time
##' @export
##' @importFrom odin odin
odin_app <- function(model, default_time) {
  shiny::shinyApp(
    ui = odin_ui(),
    server = odin_server(model, default_time))
}


odin_server <- function(model, default_time) {
  sidebar <- odin_ui_sidebar(model, default_time)

  function(input, output, session) {
    model_output <- shiny::reactiveValues(data = NULL)

    output$odin_sidebar <- shiny::renderUI({
      times <- input$reset_button
      model_output$data <- NULL
      shiny::div(id = paste("odin_sidebar_", times),
                 sidebar$tags)
    })

    shiny::observeEvent(
      input$go_button, {
        p <- get_pars(input, sidebar$parameter_name_map)
        t <- get_time(input, sidebar$has_start_time)
        model_output$data <- run_model(model, p, t)
      })

    output$result_plot <- shiny::renderPlot({
      if (is.null(model_output$data)) {
        return()
      }
      include <- get_output(input, sidebar$output_name_map)
      if (!any(include)) {
        return()
      }
      plot_model_output(model_output$data, include, sidebar$output_cols)
    })
  }
}


odin_ui <- function() {
  path_css <- system.file("styles.css", package = "odin.ui", mustWork = TRUE)

  shiny::shinyUI(shiny::fluidPage(
    shiny::includeCSS(path_css),

    shiny::titlePanel("odin ui"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("odin_sidebar"),
        shiny::hr(),
        ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
        shiny::actionButton("go_button", "Run model",
                            shiny::icon("play"),
                            class = "btn-primary"),
        shiny::actionButton("reset_button", "Reset",
                            shiny::icon("refresh"),
                            class = "btn-danger")),
      shiny::mainPanel(
        shiny::plotOutput("result_plot")
      )
    )
  ))
}


odin_ui_sidebar <- function(model, default_time) {
  pars <- odin_ui_parameters(model)
  time <- odin_ui_time(default_time)
  output <- odin_ui_output(model)
  els <- c(pars$tags, time$tags, output$tags)
  list(tags = els,
       parameter_name_map = pars$name_map,
       has_start_time = time$has_start_time,
       output_name_map = output$name_map,
       output_cols = output$cols)
}


odin_ui_sidebar_section <- function(title, ...) {
  ## TODO: this needs some css styling
  ## https://stackoverflow.com/questions/29030260/inline-checkbox-next-to-a-h3-header
  id <- sprintf("hide_%s", gsub(" ", "_", tolower(title)))
  list(
    shiny::div(class = "odin_sidebar_section_head",
               shiny::h2(title),
               shiny::checkboxInput(id, "hide")),
    shiny::conditionalPanel(
      condition = sprintf("input.%s != true", id),
      ...))
}


odin_ui_parameters <- function(model) {
  x <- stats::coef(model)

  if (!all(x$rank == 0L)) {
    stop("Only scalar parameters are currently supported")
  }
  if (!all(x$has_default)) {
    stop("All parameters must have defaults")
  }

  ## TODO: can we have a real list structure here?
  if (nrow(x) > 0L) {
    name_map <- set_names(paste0("pars_", x$name), x$name)
    tags <- odin_ui_sidebar_section(
      "Parameters",
      unname(Map(shiny::numericInput, name_map, x$name, x$default_value)))
  } else {
    name_map <- character()
    tags <- list()
  }

  list(tags = tags, name_map = name_map)
}


## TODO:
## * critical time
## * disable time selector entirely
## * max time?
## * solution tolerance
## * number of output points
odin_ui_time <- function(default_time) {
  if (length(default_time) == 1L) {
    default_time <- c(0, default_time)
    has_start_time <- FALSE
  } else if (length(default_time) == 2L) {
    has_start_time <- TRUE
  } else {
    stop("'default_time' must be length 1 or 2")
  }

  time_end <- shiny::numericInput("time_end", "end", default_time[[2L]])
  if (has_start_time) {
    time_start <- shiny::numericInput("time_start", "start", default_time[[2L]])
  } else {
    time_start <- NULL
  }

  tags <- odin_ui_sidebar_section(
    "Time",
    drop_null(list(time_start, time_end)))

  list(tags = tags, has_start_time = has_start_time)
}


odin_ui_output <- function(model) {
  x <- attr(model, "graph_data")()
  vars <- x$nodes[x$nodes$type %in% c("variable", "output"), ]
  if (!all(vars$rank == 0L)) {
    stop("Currently all output must be scalar")
  }

  name_map <- set_names(paste0("plot_", vars$name_target), vars$name_target)
  cols <- set_names(cols(length(name_map)), vars$name_target)

  tags <- odin_ui_sidebar_section(
    "Output",
    Map(shiny::checkboxInput, name_map, vars$name_target, value = TRUE))

  list(tags = tags, name_map = name_map, cols = cols)
}


## This is going to be subject to lots of change!  We'll want to split
## apart the generation from the plotting for sure.
##
## Multiple plots of output - with different output variables being
## put onto different plots stacked above each other.
##
## Per line colour
##
## Axes labels
run_model <- function(model, pars, time) {
  mod <- model(user = pars)$run(time)
}


plot_model_output <- function(xy, include, cols) {
  op <- graphics::par(mar = c(4.6, 4.6, .1, .1))
  on.exit(graphics::par(op))

  x <- xy[, 1, drop = TRUE]
  y <- xy[, names(include)[include], drop = FALSE]
  graphics::matplot(x, y, type = "l", lty = 1, las = 1, col = cols[include],
                    xlab = "Time", ylab = "Variable")
}


get_pars <- function(x, map) {
  ret <- lapply(map, function(el) x[[el]])
  ret[lengths(ret) != 0L]
}


get_time <- function(x, has_start_time) {
  seq(if (has_start_time) x$time_start else 0.0, x$time_end, length.out = 101)
}


get_output <- function(x, map) {
  vlapply(map, function(el) x[[el]])
}
