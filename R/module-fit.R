mod_fit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Fit a model"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("status_link")),
        shiny::actionButton(ns("fit"), "Fit model"),
        shiny::actionButton(ns("reset"), "Reset"),
        shiny::selectInput(ns("target"), "Target to fit", character(0)),
        shiny::uiOutput(ns("pars"))),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("results_plot")),
        shiny::uiOutput(ns("graph_control")),
        shiny::textOutput(ns("goodness_of_fit")))))
}


mod_fit_server <- function(input, output, session, data, model, configure) {
  shiny::observeEvent(
    input$reset, {
      browser()
    })

  rv <- shiny::reactiveValues(pars = NULL)

  output$status_link <- shiny::renderUI({
    show_module_status_if_not_ok(configure()$status)
  })

  shiny::observe({
    info <- configure()
    if (!info$configured) {
      target <- character(0)
    } else {
      target <- set_names(names(info$link), info$label)
    }
    prev <- input$target
    if (!is.null(prev) && nzchar(prev) && prev %in% target) {
      selected <- prev
    } else {
      selected <- NULL
    }
    shiny::updateSelectInput(session, "target", choices = target,
                             selected = selected)
  })

  output$pars <- shiny::renderUI({
    m <- model()
    if (is.null(m$result)) {
      rv$pars <- NULL
      ui <- NULL
    } else {
      dat <- mod_fit_pars(stats::coef(m$result$model), session$ns)
      rv$pars <- dat$pars
      ui <- dat$ui
    }
    ui
  })

  output$graph_control <- shiny::renderUI({
    if (!is.null(rv$result)) {
      mod_fit_graph_control(rv$outputs, rv$cols, session$ns)
    }
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
      result_combined <- cbind(mod$run(d$data[[name_time]]), d$data)

      ## Result smoothly plotted
      t <- seq(0, max(d$data[[name_time]]), length.out = 501)
      result_smooth <- mod$run(t)

      ## Goodness of fit:
      compare <- make_compare(d$data, name_time, target_data, target_model,
                              compare_sse)

      name_data <- names(info$link)
      name_model <- list_to_character(info$link)
      rv$outputs <- data_frame(
        name = name_model,
        y2 = sprintf("y2_%s", name_model))
      rv$cols <- odin_colours(name_model, name_data, rv$result$info$link)
      rv$result <- list(data = d$data,
                        combined = result_combined,
                        smooth = result_smooth,
                        user = user,
                        goodness_of_fit = compare(result_combined),
                        info = info,
                        name_time = name_time,
                        name_data = name_data,
                        name_target_data = target_data,
                        name_target_model = target_model,
                        name_model = name_model)
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
      name_target_data <- rv$result$name_target_data
      name_target_model <- rv$result$name_target_model
      ## TODO: this will not work well if names are shared
      plot_fit(rv$result$combined, name_time, name_data, rv$result$smooth,
               name_model, name_target_data, name_target_model, rv$cols)
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

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      mod_fit_download_filename(input$download_filename, input$download_type)
    },
    content = function(filename) {
      data <- switch(input$download_type,
                     modelled = rv$result$smooth,
                     combined = rv$result$combined,
                     parameters = list_to_df(rv$result$user))
      write.csv(data, filename, row.names = FALSE)
    })

  get_state <- function() {
    inputs <- list(value = mod_fit_read_inputs(rv$pars$par_id, input),
                   vary = mod_fit_read_inputs(rv$pars$var_id, input))
    ## information for the control:
    list(fit = rv$fit,
         pars = rv$pars,
         info = rv$result$info,
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
        dat <- mod_fit_pars(stats::coef(m$result$model), session$ns)
        rv$pars <- dat$pars
        ui <- dat$ui
      }
    })
    rv$fit <- state$fit
    choices <- set_names(names(state$info$link), state$info$label)
    shiny::updateSelectInput(session, "target", choices = choices,
                             selected = state$target)
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


mod_fit_graph_control <- function(outputs, cols, ns) {
  graph_settings <- mod_fit_graph_settings(outputs, cols, ns)
  shiny::tagList(
    shiny::div(
      class = "pull-right",
      shiny::div(
        class = "form-inline mt-5",
        shiny::div(
          class = "form-group",
          raw_text_input(ns("download_filename"), placeholder = "filename",
                         value = "")),
        shiny::div(
          class = "form-group",
          raw_select_input(ns("download_type"),
                           choices = list("modelled", "combined",
                                          "parameters"))),
        shiny::downloadButton(ns("download_button"), "Download",
                              class = "btn-blue")),
      graph_settings))
}


mod_fit_graph_settings <- function(outputs, cols, ns) {
  if (is.null(outputs)) {
    return(NULL)
  }
  title <- "Graph settings"
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))
  labels <- Map(function(lab, col)
    shiny::span(lab, style = paste0("color:", col)),
    outputs$name, cols$model[outputs$name])

  tags <- shiny::div(class = "form-group",
                     raw_checkbox_input(ns("logscale_y"), "Log scale y axis"),
                     shiny::tags$label("Include in plot"),
                     Map(raw_checkbox_input, ns(outputs$y2),
                         labels, value = FALSE))

  head <- shiny::a(style = "text-align: right; display: block;",
                   "data-toggle" = "collapse",
                   class = "text-muted",
                   href = paste0("#", id),
                   title, shiny::icon("gear", lib = "font-awesome"))

  body <- shiny::div(id = id,
                    class = "collapse box",
                    style = "width: 300px;",
                    list(tags))

  shiny::div(class = "pull-right mt-3", head, body)
}


mod_fit_download_filename <- function(filename, type) {
  if (!is.null(filename) && nzchar(filename)) {
    filename <- ensure_extension(filename, "csv")
  } else {
    filename <- sprintf("odin-fit-%s-%s.csv", type, date_string())
  }
  filename
}
