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
    if (is.null(m$result)) {
      rv$pars <- NULL
      ui <- NULL
    } else {
      dat <- mod_fit_pars(coef(m$result$model), session$ns)
      rv$pars <- dat$pars
      ui <- dat$ui
    }
    ui
  })

  shiny::observe({
    d <- data()
    m <- model()
    info <- configure()
    if (isTRUE(d$configured) && !is.null(m) && info$configured &&
         nzchar(input$target) && !is.null(rv$pars)) {
      user <- set_names(mod_fit_read_inputs(rv$pars$par_id, input),
                        rv$pars$name)
      name_time <- d$name_time
      target_data <- input$target
      target_model <- info$link[[target_data]]
      mod <- m$result$model(user = user)
      ## Result aligned with the data
      result_data <- cbind(mod$run(d$data[[name_time]]), d$data)

      ## Result smoothly plotted
      t <- seq(0, max(d$data[[name_time]]), length.out = 501)
      result_smooth <- mod$run(t)

      ## Goodness of fit:
      compare <- make_compare(d$data, name_time, target_data, target_model,
                              compare_sse)

      ## Big whack of data to use later on:
      rv$result <- list(data = result_data,
                        smooth = result_smooth,
                        goodness_of_fit = compare(result_data),
                        name_time = name_time,
                        name_data = names(info$link),
                        name_model = list_to_character(info$link))
    } else {
      rv$result <- NULL
    }
  })

  ## There's quite a bit that could be hardmonised here with getting
  ## the inputs arranged that is duplicated among the next few
  ## targets.
  output$results_plot <- plotly::renderPlotly({
    if (!is.null(rv$result)) {
      name_time <- rv$result$name_time
      name_data <- rv$result$name_data
      name_model <- rv$result$name_model
      cols <- pal(length(name_data))
      cols <- set_names(c(cols, cols), c(name_data, name_model))
      plot_fit(rv$result$data, name_time, name_data, rv$result$smooth,
               name_model, cols)
    }
  })

  output$goodness_of_fit <- shiny::renderText({
    if (!is.null(rv$result)) {
      sprintf("Sum of squares: %s",
              format(rv$result$goodness_of_fit, big.mark = ","))
    }
  })

  shiny::observeEvent(
    input$fit, {
      pars <- rv$pars
      pars$vary <- vlapply(rv$pars$var_id, function(x) input[[x]],
                           USE.NAMES = FALSE)
      if (any(pars$vary)) {
        ## TODO: I am not sure this is robust to partially configured
        ## datasets?
        pars$value <- vnapply(pars$par_id, function(x) input[[x]],
                              USE.NAMES = FALSE)
        info <- configure()
        d <- data()

        name_time <- d$name_time
        target_data <- input$target
        target_model <- info$link[[target_data]]
        compare <- make_compare(d$data, name_time, target_data, target_model,
                                compare_sse)
        generator <- model()$result$model
        target <- make_target(generator, pars, d$data[[name_time]], compare)
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

  get_state <- function() {
    inputs <- list(value = mod_fit_read_inputs(rv$pars$par_id, input),
                   vary = mod_fit_read_inputs(rv$pars$var_id, input))
    list(fit = rv$fit,
         pars = rv$pars,
         target = input$target,
         inputs = inputs)
  }

  set_state <- function(state) {
    m <- model()
    output$pars <- shiny::renderUI({
      if (is.null(m)) {
        rv$pars <- NULL
        ui <- NULL
      } else {
        dat <- mod_fit_pars(coef(m$result$model), session$ns)
        rv$pars <- dat$pars
        ui <- dat$ui
      }
    })
    rv$fit <- state$fit
    shiny::updateSelectInput(session, "target", selected = state$target)
    mod_fit_set_inputs(state$inputs$value, session, shiny::updateNumericInput)
    mod_fit_set_inputs(state$inputs$vary, session, shiny::updateCheckboxInput)
  }

  shiny::outputOptions(output, "pars", suspendWhenHidden = FALSE)

  list(result = shiny::reactive(rv$fit),
       pars = shiny::reactive(rv$fit$pars),
       get_state = get_state,
       set_state = set_state)
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


mod_fit_pars_update <- function(session, pars) {
  ## data <- set_names(pars$value, pars$par_id)
  ## mod_fit_set_inputs(data, session, shiny::updateNumericInput)
  for (i in seq_len(nrow(pars))) {
    id <- pars$par_id[[i]]
    value <- pars$value[[i]]
    shiny::updateNumericInput(session, id, value = value)
  }
}


mod_fit_read_inputs <- function(names, input) {
  set_names(lapply(names, function(x) input[[x]]), names)
}


mod_fit_set_inputs <- function(data, session, update_fn) {
  for (i in seq_along(data)) {
    update_fn(session, names(data)[[i]], value = data[[i]])
  }
}
