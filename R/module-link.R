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

  output$link <- shiny::renderUI({
    ## TODO: get previous first
    prev <- shiny::isolate(rv$result)
    link_ui(rv$configuration, prev, session$ns)
  })

  shiny::observe({
    vars <- rv$configuration$vars
    map <- get_inputs(input, vars$id, vars$data)
    rv$result <- link_result(map)
  })

  shiny::observe({
    rv$status <- link_status(rv$result, link_status_body)
  })

  shiny::observeEvent(
    input$clear, {
      output$link <- shiny::renderUI(
        link_ui(rv$configuration, NULL, session$ns))
    })

  get_state <- function() {
    list(result = rv$result)
  }

  set_state <- function(state) {
    rv$configuration <- link_configuration(data(), model())
    output$link <- shiny::renderUI(link_ui(
      rv$configuration, NULL, session$ns, state))
    rv$result <- state$result
  }

  shiny::outputOptions(output, "link", suspendWhenHidden = FALSE)

  list(result = shiny::reactive(add_status(rv$result, rv$status)),
       get_state = get_state,
       set_state = set_state)
}


link_ui <- function(configuration, prev, ns, restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }
  opts <- list(
    placeholder = "Select variable",
    onInitialize = I('function() { this.setValue(""); }'))
  vars <- configuration$vars

  selected <- rep(NA, length(vars$id))
  if (!is.null(restore)) {
    i <- !vlapply(restore$result$map, is_missing)
    selected[i] <- list_to_character(restore$result$map[i])
  } else if (isTRUE(prev$configured)) {
    i <- !vlapply(prev$map, is_missing)
    selected[i] <- list_to_character(prev$map[i])
  }

  choices <- vars$model
  input <- function(id, name, selected) {
    shiny::selectizeInput(id, name, selected = selected, choices = choices,
                          options = if (is.na(selected)) opts)
  }
  shiny::tagList(
    Map(input, ns(vars$id), vars$data, selected),
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
link_result <- function(map) {
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
