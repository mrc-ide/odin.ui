mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Configure"),
    shiny::h3("Data"),
    shiny::uiOutput(ns("data_status")),
    shiny::h3("Model"),
    shiny::uiOutput(ns("model_status")),
    shiny::h3("Link"),
    shiny::uiOutput(ns("link")),
    shiny::textOutput(ns("link_status")))
}


mod_configure_server <- function(input, output, session, data, model,
                                 data_status = NULL, model_status = NULL,
                                 configure_status_body = NULL) {
  rv <- shiny::reactiveValues(link = NULL)

  output$data_status <- shiny::renderUI({
    data_status()
  })

  output$model_status <- shiny::renderUI({
    model_status()
  })

  output$link_status <- shiny::renderText({
    configure_link_status(rv$label)
  })

  output$link <- shiny::renderUI({
    configure_link_ui(session$ns, input, rv, data(), model(), NULL)
  })

  shiny::observe({
    if (is.null(rv$map)) {
      rv$link <- NULL
      rv$label <- character(0)
      rv$configured <- FALSE
    } else {
      link <- lapply(rv$map, function(x) input[[x]])
      rv$link <- link[vlapply(link, function(x) !is.null(x) && nzchar(x))]
      rv$label <- sprintf("%s ~ %s",
                          names(rv$link), vcapply(rv$link, identity))
      rv$configured <- length(rv$link) > 0
    }
  })

  get_state <- function() {
    list(link = rv$link)
  }

  set_state <- function(state) {
    output$link <- shiny::renderUI(
      configure_link_ui(session$ns, input, rv, data(), model(), state$link))
  }

  shiny::outputOptions(output, "link", suspendWhenHidden = FALSE)

  list(result = shiny::reactive(list(
         link = rv$link,
         label = rv$label,
         configured = rv$configured,
         status = configure_status(rv$configured, configure_status_body))),
       get_state = get_state,
       set_state = set_state)
}


configure_link_ui <- function(ns, input, rv, data, model, restore) {
  res <- configure_link_ui_update(rv$map, input, data, model, restore)
  rv$map <- res$map
  if (!is.null(rv$map)) {
    opts <- list(
      placeholder = "Select variable",
      onInitialize = I('function() { this.setValue(""); }'))
    lapply(res$vars_data, function(x)
      shiny::selectizeInput(
        ns(res$map[[x]]), x,
        selected = res$selected[[x]], choices = res$vars_model,
        options = if (is.null(res$selected[[x]])) opts))
  }
}


configure_link_ui_update <- function(map, input, data, model, restore) {
  if (!isTRUE(data$configured) || is.null(model)) {
    return(list(map = NULL, selected = NULL))
  }

  if (is.null(restore)) {
    prev <- lapply(map, function(x) input[[x]])
  } else {
    prev <- restore
  }

  vars_data <- data$name_vars
  vars_model <- model$result$info$vars$name

  ## Are any of these still current?
  fmt <- "link_data_%s"
  map <- setNames(sprintf(fmt, vars_data), vars_data)

  selected <- set_names(rep(list(NULL), length(vars_data)), vars_data)
  i <- names(prev) %in% vars_data &
    unlist(prev, FALSE, FALSE) %in% vars_model
  selected[names(prev)[i]] <- prev[i]

  list(map = map, selected = selected,
       vars_data = vars_data, vars_model = vars_model)
}


configure_link_status <- function(label) {
  if (length(label) == 0L) {
    "No linked variables"
  } else {
    paste(label, collapse = " & ")
  }
}


configure_status <- function(configured, body) {
  if (isTRUE(configured)) {
    NULL
  } else {
    simple_panel("danger", "Model/Data link is not configured", body)
  }
}
