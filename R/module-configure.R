mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Configure"),
    shiny::h3("Data"),
    shiny::uiOutput(ns("status_data")),
    shiny::h3("Model"),
    shiny::uiOutput(ns("status_model")),
    shiny::h3("Link"),
    shiny::uiOutput(ns("link")),
    shiny::uiOutput(ns("status_link")))
}


mod_configure_server <- function(input, output, session, data, model,
                                 configure_status_body = NULL) {
  rv <- shiny::reactiveValues()

  output$status_data <- shiny::renderUI({
    data()$status$ui
  })

  output$status_model <- shiny::renderUI({
    model()$status$ui
  })

  output$status_link <- shiny::renderUI({
    ## Don't show this if not OK because it refers us back to this tab
    ## perhaps!
    show_module_status_if_ok(rv$status)
  })

  shiny::observe({
    rv$configuration <- configure_configuration(data(), model())
  })

  output$link <- shiny::renderUI({
    ## TODO: get previous first
    configure_link_ui(rv$configuration, session$ns)
  })

  shiny::observe({
    vars <- rv$configuration$vars
    map <- get_inputs(input, vars$id, vars$data)
    rv$result <- configure_result(map)
  })

  shiny::observe({
    rv$status <- configure_status(rv$result, configure_status_body)
  })

  get_state <- function() {
    list(result = rv$result)
  }

  set_state <- function(state) {
    rv$configuration <- configure_configuration(data(), model())
    output$link <- shiny::renderUI(configure_link_ui(
      rv$configuration, session$ns, state))
    rv$result <- state$result
  }

  shiny::outputOptions(output, "link", suspendWhenHidden = FALSE)

  list(result = shiny::reactive(add_status(rv$result, rv$status)),
       get_state = get_state,
       set_state = set_state)
}


configure_link_ui <- function(configuration, ns, restore = NULL) {
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
  }

  choices <- vars$model
  input <- function(id, name, selected) {
    shiny::selectizeInput(id, name, selected = selected, choices = choices,
                          options = if (is.na(selected)) opts)
  }
  Map(input, ns(vars$id), vars$data, selected)
}


## configure_link_ui_update <- function(map, input, data, model, restore) {
##   if (!isTRUE(data$configured) || is.null(model)) {
##     return(list(map = NULL, selected = NULL))
##   }

##   if (is.null(restore)) {
##     prev <- lapply(map, function(x) input[[x]])
##   } else {
##     prev <- restore
##   }

##   vars_data <- data$name_vars
##   vars_model <- model$info$vars$name

##   ## Are any of these still current?
##   fmt <- "link_data_%s"
##   map <- set_names(sprintf(fmt, vars_data), vars_data)

##   selected <- set_names(rep(list(NULL), length(vars_data)), vars_data)
##   i <- names(prev) %in% vars_data &
##     unlist(prev, FALSE, FALSE) %in% vars_model
##   selected[names(prev)[i]] <- prev[i]

##   list(map = map, selected = selected,
##        vars_data = vars_data, vars_model = vars_model)
## }


configure_status <- function(result, body) {
  if (isTRUE(result$configured)) {
    class <- "success"
    title <- "Model/Data link is configured"
    body <- paste(result$label, collapse = " & ")
  } else {
    class <- "danger"
    title <- "Model/Data link is not configured"
  }
  module_status(class, title, body)
}


configure_configuration <- function(data, model) {
  if (!isTRUE(model$success) || !isTRUE(data$configured)) {
    return(NULL)
  }

  vars_data <- data$name_vars
  vars_model <- model$info$vars$name
  vars_id <- sprintf("link_data_%s", vars_data)

  list(data = data, model = model,
       vars = list(data = vars_data, model = vars_model, id = vars_id))
}


## `link` here must be a named list where names are the *data*
## elements, and values are the *model* elements (possibly null or NA,
## which will be filtered)
configure_result <- function(map) {
  map <- map[!vlapply(map, is_missing)]
  label <- sprintf("%s ~ %s", names(map), list_to_character(map))
  list(map = map, label = label, configured = length(map) > 0L)
}
