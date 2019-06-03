## This is basically the module from module-model-plotly.R but we need
## a few tweaks and I don't want to bog that down any further at
## present (though hopefully we'll join back up eventually).  We want:
##
## * reactive model input
## * time to be fixed
## * include data (optionally perhaps?)
## * no replicates
## * no extra
##
## Then we'll use this as a base for the batch plot and the phase plot

mod_vis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Visualise"),
    ## shiny::p(class = "mt-5"),
    shiny::sidebarLayout(
      shiny::div(
        class = "col-sm-4 col-lg-3",
        shiny::tags$form(
          class = "form-horizontal",
          shiny::uiOutput(ns("odin_control")),
          ## https://github.com/rstudio/shiny/issues/1675#issuecomment-298398997
          shiny::actionButton(ns("reset_button"), "Reset",
                              shiny::icon("refresh"),
                              class = "btn-grey pull-right ml-2"),
          shiny::actionButton(ns("go_button"), "Run model",
                              shiny::icon("play"),
                              class = "btn-blue pull-right"),
          shiny::div(
            class = "form-group pull-right", style = "clear:both;",
            shiny::div(
              class = "col-sm-12",
              raw_checkbox_input(ns("auto_run"), "Auto run", value = FALSE))))),
      shiny::mainPanel(
        shiny::div(class = "plotly-graph-wrapper",
                   plotly::plotlyOutput(ns("odin_output"))),
        shiny::uiOutput(ns("graph_control")))))
}


mod_vis_server <- function(input, output, session, data, model, configure) {
  rv <- shiny::reactiveValues(pars = NULL)

  shiny::observe({
    m <- model()
    if (is.null(m$result)) {
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
      outputs$vars$y2 <- sprintf("y2_%s", outputs$vars$name)

      outputs$vars$col <- odin_ui_palettes("odin")(nrow(outputs$vars))
      rv$outputs <- outputs$vars
    }
  })

  output$odin_control <- shiny::renderUI({
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
    mod_vis_graph_control(rv$outputs, session$ns)
  })

  shiny::observeEvent(
    input$go_button, {
      m <- model()
      d <- data()
      info <- configure()

      if (!is.null(m) && !is.null(d) && info$configured && !is.null(rv$pars)) {
        user <- set_names(lapply(rv$pars$par_id, function(x) input[[x]]),
                          rv$pars$name)
        n <- 501
        t <- seq(0, max(d[[info$time]]), length.out = 501)
        ## Put the parameters used and outputs into the result so
        ## we're always completely consistent, regardness of shiny's
        ## order of evaluation
        rv$result <- list(pars = rv$pars,
                          outputs = rv$outputs,
                          data = d,
                          info = info,
                          modelled = m$result$model(user = user)$run(t))
      }
    })

  output$odin_output <- plotly::renderPlotly({
    if (!is.null(rv$result)) {
      plot_vis(rv$result, input)
    }
  })
}


mod_vis_pars <- function(pars, ns) {
  if (!is.null(pars) && nrow(pars) > 0L) {
    pars$value <- vapply(pars$default_value, identity, numeric(1))
    pars$par_id <- sprintf("par_%s", pars$name)
    input <- function(name, id, value) {
      horizontal_form_group(
        shiny::span(name),
        raw_numeric_input(ns(id), value = value))
    }
    ui <- mod_model_control_section(
      "Model parameters",
      unname(Map(input, pars$name, pars$par_id, pars$value)),
      ns = ns)
    list(pars = pars, ui = ui)
  }
}


mod_vis_graph_control <- function(outputs, ns) {
  graph_settings <- mod_vis_graph_settings(outputs, ns)
  shiny::tagList(
    shiny::div(
      class = "pull-right",
      shiny::div(
        class = "form-inline mt-5",
        shiny::div(
          class = "form-group",
          raw_text_input(ns("download_filename"), placeholder = "filename",
                         value = "")),
        shiny::span("."),
        shiny::div(
          class = "form-group",
          raw_select_input(ns("download_format"),
                           choices = list("csv","rds","json"))),
        shiny::downloadButton(ns("download_button"), "Download",
                              class = "btn-blue")),
      graph_settings))
}


mod_vis_graph_settings <- function(outputs, ns) {
  if (is.null(outputs)) {
    return(NULL)
  }
  title <- "Graph settings"
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))
  labels <- Map(function(lab, col)
    shiny::span(lab, style = paste0("color:", col)),
    outputs$name, outputs$col)

  tags <- shiny::div(class = "form-group",
                     raw_checkbox_input(ns("logscale_y"), "Log scale y axis"),
                     shiny::tags$label("Plot on second y axis"),
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


## Just punting on this:
plot_vis <- function(result, input) {
  outputs <- result$outputs
  link <- result$info$link

  cols <- set_names(outputs$col, outputs$name)
  y2 <- set_names(vlapply(outputs$y2, function(el) input[[el]]),
                  outputs$name)

  p <- plotly::plot_ly()
  p <- plotly::config(p, collaborate = FALSE, displaylogo = FALSE)

  xy <- result$modelled
  for (i in outputs$name) {
    yaxis <- if (y2[[i]]) "y2" else NULL
    p <- plotly::add_lines(p, x = xy[, "t"], y = xy[, i], name = i,
                           line = list(color = cols[[i]]),
                           yaxis = yaxis)
  }

  if (any(y2)) {
    opts <- list(overlaying = "y",
                 side = "right",
                 showgrid = FALSE,
                 title = paste(names(y2)[y2], collapse = ", "))
    p <- plotly::layout(p, yaxis2 = opts)
  }

  data_time <- result$data[[result$info$time]]
  for (i in seq_along(link)) {
    nm <- names(link)[[i]]
    y <- result$data[[nm]]
    j <- !is.na(y)
    yaxis <- if (y2[[link[[i]]]]) "y2" else NULL
    p <- plotly::add_markers(p, x = data_time[j], y = y[j], name = nm,
                             marker = list(color = cols[[link[[i]]]]),
                             yaxis = yaxis)
  }

  p
}
