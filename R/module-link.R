mod_link_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Link"),
    shiny::h3("Data"),
    shiny::uiOutput(ns("status_data")),
    shiny::h3("Model"),
    shiny::uiOutput(ns("status_model")),
    shiny::h3("Link"),
    shiny::uiOutput(ns("link")),
    shiny::uiOutput(ns("summary")))
}


## This should not take as much as inputs?  Can we strip it down to
## just the status element and data$name_vars, model$info$vars, which
## would invalidate less often
mod_link_server <- function(input, output, session, data, model,
                            link_status_body = NULL) {
  rv <- shiny::reactiveValues()

  output$status_data <- shiny::renderUI({
    data()$status$ui
  })

  output$status_model <- shiny::renderUI({
    model()$status$ui
  })

  output$summary <- shiny::renderUI({
    link_summary(rv$configuration, rv$result)
  })

  shiny::observe({
    rv$configuration <- link_configuration(data(), model())
  })

  shiny::observe({
    values <- get_inputs(input, rv$configuration$vars$id)
    if (!values_present(rv$values) || values_present(values)) {
      rv$values <- values
    }
  })

  output$link <- shiny::renderUI({
    link_ui(rv$configuration, rv$values, session$ns)
  })

  shiny::observe({
    if (!is.null(rv$configuration$vars$data)) {
      rv$result <- link_result(rv$values, rv$configuration$vars$data)
    }
  })

  shiny::observe({
    rv$status <- link_status(rv$result, link_status_body)
  })

  shiny::observeEvent(
    input$clear, {
      output$link <- shiny::renderUI(
        link_ui(rv$configuration, NULL, session$ns))
    })

  shiny::outputOptions(output, "link", suspendWhenHidden = FALSE)

  get_state <- function() {
    list(
      configuration = common_model_data_configuration_save(rv$configuration),
      values = rv$values,
      result = rv$result)
  }

  set_state <- function(state) {
    rv$configuration <- state$configuration
    restore_inputs(session, state$values, function(session, id, value) {
      shiny::updateSelectInput(session, id, selected = value)
    })
    rv$values <- state$values
  }

  list(result = shiny::reactive(add_status(rv$result, rv$status)),
       get_state = get_state,
       set_state = set_state)
}


link_ui <- function(configuration, prev, ns) {
  if (is.null(configuration)) {
    return(NULL)
  }
  opts <- list(
    placeholder = "Select variable",
    onInitialize = I('function() { this.setValue(""); }'))
  vars <- configuration$vars

  selected <- rep(NA, length(vars$id))
  i <- !vlapply(prev, is_missing)
  selected[i] <- list_to_character(prev[i])

  choices <- vars$model
  input <- function(id, name, selected) {
    shiny::selectizeInput(id, name, selected = selected, choices = choices,
                          options = if (is.na(selected)) opts)
  }
  shiny::tagList(
    Map2(input, ns(vars$id), vars$data, selected),
    shiny::actionButton(ns("clear"), "Clear", shiny::icon("times"),
                        class = "btn-danger"))
}


link_status <- function(result, body) {
  if (isTRUE(result$configured)) {
    class <- "success"
    title <- "Model/Data link is configured"
    body <- paste(result$label, collapse = " & ")
  } else {
    class <- "danger"
    title <- result$error %||% "Model/Data link is not configured"
  }
  module_status(class, title, body)
}


link_summary <- function(configuration, result) {
  if (is.null(configuration)) {
    return(NULL)
  }
  if (isTRUE(result$configured)) {
    class <- "success"
    title <- "Model/Data link is configured"
    body <- paste(result$label, collapse = " & ")
  } else if (!is.null(result$error)) {
    class <- "danger"
    title <- "Invalid Model/Data link"
    body <- result$error
  } else {
    class <- "info"
    title <- "Model/Data link is not configured"
    body <- "Select model outputs that correspond to each data series above"
  }
  simple_panel(class, title, body)
}


link_configuration <- function(data, model) {
  if (!isTRUE(model$success) || !isTRUE(data$configured)) {
    return(NULL)
  }

  vars_data <- data$name_vars
  vars_model <- model$info$vars$name[model$info$vars$include]
  vars_id <- sprintf("link_data_%s", vars_data)

  list(data = data, model = model,
       vars = list(data = vars_data, model = vars_model, id = vars_id))
}


## `link` here must be a named list where names are the *data*
## elements, and values are the *model* elements (possibly null or NA,
## which will be filtered)
link_result <- function(values, names) {
  ## This turns up during the resolution of reactives, which can
  ## happen in an unfortunate order
  if (length(values) == 0L) {
    names <- character(0)
  }
  map <- set_names(values, names)
  map <- map[!vlapply(map, is_missing)]

  targets <- list_to_character(map)
  dup <- unique(targets[duplicated(targets)])
  if (any(duplicated(targets))) {
    list(error = paste0("Duplicated link target: ", dup),
         configured = FALSE)
  } else {
    label <- sprintf("%s ~ %s", names(map), list_to_character(map))
    list(map = map, label = label, configured = length(map) > 0L)
  }
}


values_present <- function(x) {
  length(x) > 0 && !all(vlapply(x, is_missing))
}
