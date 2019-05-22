mod_fit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Fit a model"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::actionButton(ns("fit"), "Fit model"),
        shiny::actionButton(ns("reset"), "Reset"),
        shiny::selectInput(ns("target"), "Target to fit", character(0)),
        shiny::uiOutput(ns("pars"))),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("results_plot")),
        shiny::textOutput(ns("goodness_of_fit")))))
}


mod_fit_server <- function(input, output, session, data, model, configure) {
  shiny::observeEvent(
    input$reset, {
      browser()
    })

  ## TODO: all this colour palette stuff needs changing
  pal <- odin_ui_palettes("odin")
  rv <- shiny::reactiveValues(pars = NULL)

  shiny::observe({
    info <- configure()
    if (!info$configured) {
      target <- character(0)
    } else {
      ## TODO: this could be done by the configure module
      link <- info$link
      label <- paste(vcapply(link, identity), names(link), sep = " ~ ")
      target <- setNames(names(link), label)
    }
    ## TODO: retain previous choice if it's here
    shiny::updateSelectInput(session, "target", choices = target)
  })

  output$pars <- shiny::renderUI({
    m <- model()
    if (is.null(m)) {
      rv$pars <- NULL
      ui <- NULL
    } else {
      dat <- mod_fit_pars(coef(m$result$model), session$ns)
      rv$pars <- dat$pars
      ui <- dat$ui
    }
    ui
  })

  ## There's quite a bit that could be hardmonised here with getting
  ## the inputs arranged that is duplicated among the next few
  ## targets.
  output$results_plot <- plotly::renderPlotly({
    d <- data()
    m <- model()
    info <- configure()
    if (!is.null(d) && !is.null(m) && info$configured && !is.null(rv$pars)) {
      user <- set_names(lapply(rv$pars$par_id, function(x) input[[x]]),
                        rv$pars$name)
      if (!any(vlapply(user, is.null))) {
        name_data <- names(info$link)
        name_model <- vcapply(info$link, identity, USE.NAMES = FALSE)
        cols <- pal(length(name_data))
        cols <- set_names(c(cols, cols), c(name_data, name_model))
        plot_fit(d, info$time, name_data,
                 m$result$model(user = user), name_model,
                 cols)
      }
    }
  })

  shiny::observeEvent(
    input$fit, {
      pars <- rv$pars
      pars$vary <- vlapply(rv$pars$var_id, function(x) input[[x]],
                           USE.NAMES = FALSE)
      if (any(pars$vary)) {
        pars$value <- vnapply(pars$par_id, function(x) input[[x]],
                              USE.NAMES = FALSE)
        info <- configure()
        d <- data()

        name_time <- info$time
        target_data <- input$target
        target_model <- info$link[[target_data]]
        compare <- make_compare(d, name_time, target_data, target_model,
                                compare_sse)
        generator <- model()$result$model
        target <- make_target(generator, pars, d[[name_time]], compare)
        message("Starting fit")
        rv$fit <- shiny::withProgress(
          message = "model fit in progress",
          detail = "this may take a while",
          value = 0,
          fit_model(target, pars, tolerance = 1e-6, method = "nmkb"))
        mod_fit_pars_update(session, rv$fit$coef[pars$vary, , drop = FALSE])
        message("done")
      }
    })
}



mod_fit_pars <- function(pars, ns) {
  pars$value <- vapply(pars$default_value, identity, numeric(1))
  pars$par_id <- sprintf("par_%s", pars$name)
  pars$var_id <- sprintf("var_%s", pars$name)

  ## A little cheat for now:
  pars$vary <- pars$name %in% c("I0", "cfr", "R0_before", "R0_after")

  f <- function(i) {
    shiny::fluidRow(
      shiny::column(
        10,
        shiny::numericInput(
          ns(pars$par_id[[i]]), pars$name[[i]], pars$value[[i]])),
      shiny::column(
        2,
        shiny::checkboxInput(
          ns(pars$var_id[[i]]), "", pars$vary[[i]])))
  }

  ui <- shiny::tagList(lapply(seq_len(nrow(pars)), f))
  list(pars = pars, ui = ui)
}


mod_fit_pars_update <- function(session, pars, field = "value") {
  for (i in seq_len(nrow(pars))) {
    id <- pars$par_id[[i]]
    value <- pars$value[[i]]
    shiny::updateNumericInput(session, id, value = value)
  }
}
