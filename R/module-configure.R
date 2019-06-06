mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Configure"),
    shiny::h3("Data"),
    shiny::textOutput(ns("data_summary")),
    shiny::h3("Model"),
    shiny::uiOutput(ns("model_status")),
    shiny::textOutput(ns("model_summary")),
    shiny::h3("Link"),
    shiny::uiOutput(ns("link")),
    shiny::textOutput(ns("link_status")))
}


mod_configure_server <- function(input, output, session, data, model) {
  output$data_summary <- shiny::renderText({
    d <- data()
    if (is.null(d)) {
      ## Ideally this would point us back to the data tab with a link
      ## but that requires passing in the parent session:
      ## https://stackoverflow.com/a/54751068
      msg <- "Please upload data"
    } else if (!isTRUE(d$configured)) {
      msg <- "Please select time variable for your data"
    } else {
      msg <- sprintf("%d rows of data have been uploaded", nrow(d$data))
    }
    msg
  })

  rv <- shiny::reactiveValues(link = NULL)

  shiny::observe({
    res <- model()
    status <- NULL

    if (is.null(res)) {
      msg <- "Please compile a model"
    } else {
      if (!res$is_current) {
        status <- shiny::div(
          class = "panel-group",
          shiny::div(
            class = "panel panel-warning",
            shiny::div(class = "panel-heading",
                       shiny::icon("exclamation fa-lg"),
                       "Warning: model is out of date"),
            shiny::div(class = "panel-body",
                       "Consider recompiling the model")))
      }

      pars <- coef(res$result$model)
      msg <- sprintf("Model with %d parameters", ncol(pars))
    }
    output$model_status <- shiny::renderUI(status)
    output$model_summary <- shiny::renderText(msg)
  })

  output$link <- shiny::renderUI({
    mod_configure_link_ui(input, session, rv, data, model, NULL)
  })

  shiny::observe({
    if (is.null(rv$map)) {
      rv$link <- NULL
      rv$label <- character(0)
    } else {
      link <- lapply(rv$map, function(x) input[[x]])
      rv$link <- link[vlapply(link, function(x) !is.null(x) && nzchar(x))]
      rv$label <- sprintf("%s ~ %s",
                          names(rv$link), vcapply(rv$link, identity))
    }
  })

  output$link_status <- shiny::renderText({
    if (length(rv$link) == 0L) {
      "No linked variables"
    } else {
      paste(rv$label, collapse = " & ")
    }
  })

  get_state <- function() {
    list(link = rv$link)
  }

  set_state <- function(state) {
    output$link <- shiny::renderUI(
      mod_configure_link_ui(input, session, rv, data, model, state$link))
  }

  shiny::outputOptions(output, "link", suspendWhenHidden = FALSE)

  list(result = shiny::reactive(list(
         link = rv$link,
         label = rv$label,
         configured = length(rv$link) > 0)),
       get_state = get_state,
       set_state = set_state)
}


selected <- function(prev, choices) {
  if (!is.null(prev) && prev %in% choices) prev else NA
}


mod_configure_link_ui <- function(input, session, rv, data, model, restore) {
  d <- data()
  m <- model()
  if (!isTRUE(d$configured) || is.null(m)) {
    rv$map <- NULL
  } else {
    shiny::isolate({
      if (is.null(restore)) {
        prev <- lapply(rv$map, function(x) input[[x]])
      } else {
        prev <- restore
      }

      vars_data <- d$name_vars

      ## TODO: push this into the editor module so that we always have
      ## this alongside the model as we use it in a couple of places
      ## (see also fit.R's run_model_data)
      metadata <- model_metadata(m$result$model)
      vars_model <- c(
        names(metadata$data$variable$contents),
        names(metadata$data$output$contents))

      ## Are any of these still current?
      fmt <- "link_data_%s"
      rv$map <- setNames(sprintf(fmt, vars_data), vars_data)
      opts <- list(
        placeholder = "Select variable",
        onInitialize = I('function() { this.setValue(""); }'))

      selected <- set_names(rep(list(NULL), length(vars_data)), vars_data)
      i <- names(prev) %in% vars_data &
        unlist(prev, FALSE, FALSE) %in% vars_model
      selected[names(prev)[i]] <- prev[i]

      ns <- session$ns
      lapply(vars_data, function(x)
        shiny::selectizeInput(
          ns(sprintf(fmt, x)), x, selected = selected[[x]],
          choices = vars_model, options = if (is.null(selected[[x]])) opts))
    })
  }
}
