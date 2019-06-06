mod_batch_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Sensitivity"),
    shiny::sidebarLayout(
      ## NOTE: almost the same as the visualiser
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("status_data")),
          shiny::uiOutput(ns("status_model")),
          shiny::uiOutput(ns("model_parameters")),
          shiny::uiOutput(ns("focal_parameter")),
          ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
          shiny::uiOutput(ns("import_button"), inline = TRUE),
          shiny::actionButton(ns("reset_button"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("go_button"), "Run model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::uiOutput(ns("graph_control")))))
}


mod_batch_server <- function(input, output, session, model, data,
                             import = NULL) {
  rv <- shiny::reactiveValues(pars = NULL)

  shiny::observe({
    m <- model()
    d <- data()
    if (is.null(m$result) || !isTRUE(d$configured)) {
      rv$pars <- NULL
      rv$outputs <- NULL
    } else {
      pars <- coef(m$result$model)
      pars$value <- vapply(pars$default_value, identity, numeric(1))
      pars$par_id <- sprintf("par_id_%s", pars$name)
      rv$pars <- pars

      metadata <- model_metadata(m$result$model)
      outputs <- mod_model_control_outputs(metadata, NULL, NULL, ns)
      outputs$vars$id <- outputs$name_map
      outputs$vars$include <- sprintf("include_%s", outputs$vars$name)
      rv$outputs <- outputs$vars
      rv$cols <- odin_colours_model(outputs$vars$name)
    }
  })

  ## TODO: eventually we should use time only - that should be easy
  ## enough though
  output$status_data <- shiny::renderUI({
    if (!isTRUE(data()$configured)) {
      simple_panel("danger", "Data not present",
                   "Please upload data using the data tab")
    }
  })

  output$status_model <- shiny::renderUI({
    m <- model()
    if (is.null(m)) {
      simple_panel("danger", "Model not present",
                   "Please create a model using the editor tab")
    } else if (!m$is_current) {
      simple_panel("warning", "Model out of date",
                   "You may need to recompile your model")
    }
  })

  output$model_parameters <- shiny::renderUI({
    if (!is.null(rv$pars)) {
      ns <- session$ns
      input <- function(name, id, value) {
        horizontal_form_group(
          shiny::span(name),
          raw_numeric_input(ns(id), value = value))
      }
      mod_model_control_section(
        "Model parameters",
        unname(Map(input, rv$pars$name, rv$pars$par_id, rv$pars$value)),
        ns = ns)
    }
  })

  output$graph_control <- shiny::renderUI({
    mod_batch_graph_control(rv$outputs, rv$cols, session$ns)
  })

  output$focal_parameter <- shiny::renderUI({
    if (!is.null(rv$pars)) {
      ns <- session$ns
      input <- function(name, id, value) {
        horizontal_form_group(
          shiny::span(name),
          raw_numeric_input(ns(id), value = value))
      }
      mod_model_control_section(
        "Vary parameter",
        horizontal_form_group(
          "Parameter to vary",
          raw_select_input(ns("focal_name"), rv$pars$name, selected = NA)),
        input("Variation (%)", "focal_pct", NA),
        input("Number of runs", "focal_n", NA),
        shiny::textOutput(ns("focal_status")),
        ns = ns)
    }
  })

  shiny::observe({
    rv$focal <- batch_focal(input$focal_name, input$focal_pct, input$focal_n,
                            rv$pars, input)
  })

  output$focal_status <- shiny::renderText({
    if (!is.null(rv$focal)) {
      sprintf("%s - %s - %s", rv$focal$from, rv$focal$value, rv$focal$to)
    }
  })

  shiny::observeEvent(
    input$focal_pct, {
      if (!is.na(input$focal_pct)) {
        id <- rv$pars$par_id[match(input$focal_name, rv$pars$name)]
        value <- input[[id]]
        if (!is.null(value) && !is.na(value)) {
          y <- value + c(-1, 1) * abs(input$focal_pct / 100 * value)
          shiny::isolate({
            shiny::updateNumericInput(session, "focal_from", value = y[[1]])
            shiny::updateNumericInput(session, "focal_to", value = y[[2]])
          })
        }
      }
    })

  shiny::observeEvent(
    input$go_button, {
      focal <- input
      user <- set_names(lapply(rv$pars$par_id, function(x) input[[x]]),
                        rv$pars$name)
      rv$result <- run_model_batch(model(), data(), user, rv$focal)
    })

  output$import_button <- shiny::renderUI({
    if (!is.null(import) && !is.null(import())) {
      shiny::actionButton(session$ns("import"), "Import",
                          shiny::icon("calculator"))
    }
  })

  shiny::observeEvent(
    input$import, {
      user <- import()
      if (!is.null(user)) {
        shiny::isolate({
          id <- rv$pars$par_id[match(names(user), rv$pars$name)]
          if (!any(is.na(id))) {
            for (i in seq_along(id)) {
              shiny::updateNumericInput(session, id[[i]], value = user[[i]])
            }
          }
        })
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result)) {
      include <- set_names(
        vlapply(rv$outputs$include, function(el) input[[el]]),
        rv$outputs$name)
      if (any(include)) {
        vars <- names(include)[include]
        cols <- odin_colours_model(rv$outputs$name)
        plot_batch(rv$result, vars, cols, input$logscale_y)
      }
    }
  })
}


run_model_batch <- function(model, data, user, focal) {
  if (is.null(focal)) {
    return(NULL)
  }
  mod <- model$result$model(user = user)
  n <- constrain(focal$n, 2, 20)
  pr <- seq(focal$from, focal$to, length.out = n)
  nm <- focal$name
  t <- seq(0, max(data$data[[data$name_time]]), length.out = 501)

  y <- mod$run(t)
  z <- lapply(pr, function(p) {
    mod$set_user(user = set_names(list(p), nm))
    mod$run(t)
  })

  list(t = t, y = y, z = z, p = pr, focal = focal)
}


batch_focal <- function(name, pct, n, pars, input) {
  if (is.null(pct) || is.na(pct)) {
    return(NULL)
  }
  if (is.null(name)) {
    return(NULL)
  }
  if (is.null(n)) {
    return(NULL)
  }
  id <- pars$par_id[match(name, pars$name)]
  value <- input[[id]]
  if (is.na(value)) {
    return(NULL)
  }
  dy <- abs(pct / 100 * value)
  list(name = name, value = value, n = n, from = value - dy, to = value + dy)
}


plot_batch <- function(output, vars, cols, logscale_y) {
  p <- plotly::plot_ly()
  p <- plotly::config(p, collaborate = FALSE, displaylogo = FALSE)
  for (i in vars) {
    for (j in seq_along(output$z)) {
      nm <- sprintf("%s (%s)", i, output$p[[j]])
      p <- plotly::add_lines(p, x = output$t, y = output$z[[j]][, i],
                             name = nm, legendgroup = i, showlegend = FALSE,
                             hoverlabel = list(namelength = -1),
                             line = list(color = cols[[i]], width = 1))
    }
    p <- plotly::add_lines(p, x = output$t, y = output$y[, i],
                           name = i, legendgroup = i,
                           line = list(color = cols[[i]]))
  }

  if (logscale_y) {
    p <- plotly::layout(p, yaxis = list(type = "log"))
  }

  p
}


mod_batch_graph_control <- function(outputs, cols, ns) {
  graph_settings <- mod_batch_graph_settings(outputs, cols, ns)
  shiny::tagList(
    shiny::div(
      class = "pull-right",
      graph_settings))
}


mod_batch_graph_settings <- function(outputs, cols, ns) {
  if (is.null(outputs)) {
    return(NULL)
  }
  title <- "Graph settings"
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))
  labels <- Map(function(lab, col)
    shiny::span(lab, style = paste0("color:", col)),
    outputs$name, cols[outputs$name])

  tags <- shiny::div(class = "form-group",
                     raw_checkbox_input(ns("logscale_y"), "Log scale y axis"),
                     shiny::tags$label("Include in plot"),
                     Map(raw_checkbox_input, ns(outputs$include),
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
