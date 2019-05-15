## An example shiny app for fitting a model
devtools::load_all()

model <- odin::odin_("anne/model/model1.R")
data <- read_csv("anne/data/combined.csv")

pars <- coef(model)
pars$value <- vapply(pars$default_value, identity, numeric(1))
pars$min[is.infinite(pars$min)] <- NA
pars$max[is.infinite(pars$max)] <- NA
pars$vary <- FALSE
pars$par_id <- sprintf("par_%s", pars$name)
pars$var_id <- sprintf("var_%s", pars$name)

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
          plotly::plotlyOutput("results_plot")))))
}



fitter_server <- function(model, pars, data) {
  name_time <- "day"
  map <- c(weekly_death_h = "deaths", weekly_onset = "cases")
  pal <- odin_ui_palettes("odin")(2)
  cols <- setNames(c(pal, pal), c(unname(map), names(map)))

  function(input, output, session) {
    output$results_plot <- plotly::renderPlotly(
      plot_fit(data, name_time, model(), map, cols))

    shiny::observeEvent(
      input$reset, {
        shiny::isolate({
          for (i in seq_len(nrow(pars))) {
            id <- pars$par_id[[i]]
            value <- pars$default_value[[i]]
            shiny::updateNumericInput(session, id, value = value)
          }
        })
      })
  }
}


shiny::shinyApp(fitter_ui(model, pars, data), fitter_server(model, pars, data))
