## An example shiny app for fitting a model
devtools::load_all()

model <- odin::odin_("anne/model/model1.R")
data <- read_csv("anne/data/combined.csv")

pars <- coef(model)
pars$value <- vapply(pars$default_value, identity, numeric(1))
pars$par_id <- sprintf("par_%s", pars$name)
pars$var_id <- sprintf("var_%s", pars$name)

pars$vary <- pars$name %in% c("I0", "cfr", "R0_before", "R0_after")
## pars$vary <- FALSE

fitter_ui <- function(model, pars, data) {
  ## Start with a bunch of rows:

  f <- function(i) {
    shiny::fluidRow(
      shiny::column(
        10,
        shiny::numericInput(
          pars$par_id[[i]], pars$name[[i]], pars$value[[i]])),
      shiny::column(
        2,
        shiny::checkboxInput(
          pars$var_id[[i]], "", pars$vary[[i]])))
  }

  pars_ui <- shiny::tagList(lapply(seq_len(nrow(pars)), f))

  shiny::shinyUI(
    shiny::fluidPage(
      shiny::titlePanel("Fit a model"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::actionButton("fit", "Fit model"),
          shiny::actionButton("reset", "Reset"),
          pars_ui),
        shiny::mainPanel(
          shiny::h2("Plot"),
          plotly::plotlyOutput("results_plot"),
          shiny::textOutput("goodness_of_fit")))))
}



fitter_server <- function(model, pars, data) {
  ## This needs to be more generalisable:
  compare_sse <- function(modelled, real) {
    sum((modelled - real)^2, na.rm = TRUE)
  }
  name_time <- "day"
  map <- c(weekly_death_h = "deaths", weekly_onset = "cases")
  pal <- odin_ui_palettes("odin")(2)
  cols <- setNames(c(pal, pal), c(unname(map), names(map)))
  compare <- make_compare(data, "day", "deaths", "weekly_death_h",
                          compare_sse)


  function(input, output, session) {
    output$results_plot <- plotly::renderPlotly({
      p <- setNames(vnapply(pars$par_id, function(x) input[[x]]), pars$name)
      plot_fit(data, name_time, model(user = as.list(p)), map, cols)
    })

    output$goodness_of_fit <- shiny::renderText({
      pars$value <- vnapply(pars$par_id, function(x) input[[x]])
      pars$vary <- TRUE
      value <- make_target(model, pars, data$day, compare)(pars$value)
      paste("Sum of squares:", value)
    })

    shiny::observeEvent(
      input$reset, {
        shiny::isolate({
          update_pars(session, pars, "default_value")
        })
      })

    shiny::observeEvent(
      input$fit, {
        pars$vary <- vlapply(pars$var_id, function(x) input[[x]])
        if (any(pars$vary)) {
          callback <- function(p, res) {
            ## This does not actually work to update the text
            ## unfortunately.  It's possible that we could do this
            ## with js perhaps, but there's no way that we could
            ## update the actual graph like that.
            output$goodness_of_fit <- shiny::renderText(as.character(runif(1)))
          }
          pars$name[pars$vary]
          pars$value <- vnapply(pars$par_id, function(x) input[[x]])
          target <- make_target(model, pars, data$day, compare)
          message("Starting fit")
          fit <- shiny::withProgress(
            message = "model fit in progress",
            detail = "this may take a while",
            value = 0,
            fit_model(target, pars, tolerance = 1e-6, method = "nmkb"))
          update_pars(session, fit$coef[pars$vary, , drop = FALSE])
          message("done")
        }
      })
  }
}

update_pars <- function(session, pars, field = "value") {
  for (i in seq_len(nrow(pars))) {
    id <- pars$par_id[[i]]
    value <- pars$value[[i]]
    message(sprintf("Setting %s => %s", id, value))
    shiny::updateNumericInput(session, id, value = value)
  }
}


shiny::shinyApp(fitter_ui(model, pars, data), fitter_server(model, pars, data))
