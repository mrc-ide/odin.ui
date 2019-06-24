mod_lock_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("control_locked"))
}


mod_lock_server <- function(input, output, session, render, current,
                            set_current) {
  rv <- shiny::reactiveValues(hidden = FALSE)

  output$control_locked <- shiny::renderUI({
    lock_control(render(), session$ns)
  })

  shiny::observeEvent(
    input$set, {
      result <- current()
      if (isTRUE(result$success)) {
        rv$locked <- result
        rv$hidden <- FALSE
      }
    })

  shiny::observeEvent(
    input$hide, {
      rv$hidden <- !rv$hidden
    })

  shiny::observeEvent(
    input$clear, {
      rv$locked <- NULL
    })

  shiny::observeEvent(
    input$swap, {
      result <- current()
      ## Only set if the models have the same form:
      if (models_compatible(rv$locked$configuration, result$configuration)) {
        set_current(rv$locked)
        if (isTRUE(result$success)) {
          rv$locked <- result
        }
      }
    })

  ## TODO: this works off of the model *run*, not off the model
  ## itself, so we need another bit that indicates that is out of
  ## date.
  output$status <- shiny::renderUI({
    lock_status(rv$locked, current(), rv$hidden)
  })

  get_state <- function() {
    rv$locked
  }

  set_state <- function(state) {
    rv$locked <- state
  }

  list(result = shiny::reactive(if (!rv$hidden) rv$locked),
       get_state = get_state,
       set_state = set_state)
}


lock_control <- function(render, ns, collapsed = TRUE) {
  if (!isTRUE(render)) {
    return(NULL)
  }

  mod_model_control_section(
    "Locked parameter set",
    shiny::uiOutput(ns("status")),
    shiny::actionButton(ns("set"), "Lock current",
                        shiny::icon("lock"),
                        class = "btn-blue"),
    shiny::actionButton(
      ns("hide"), "Show/Hide locked", shiny::icon("eye")),
    shiny::actionButton(ns("clear"), "Clear locked",
                        shiny::icon("trash"),
                        class = "btn-danger"),
    shiny::actionButton(ns("swap"), "Swap locked and current",
                        shiny::icon("refresh"),
                        class = "btn-grey"),
    ns = ns, collapsed = TRUE)
}


lock_status <- function(locked, current, hidden) {
  if (is.null(locked)) {
    class <- "danger"
    title <- "No locked data"
    body <- NULL
  } else if (identical(locked, current)) {
    class <- "warning"
    title <- "Locked data present"
    body <- "Same as current"
  } else {
    compatible <- models_compatible(locked$value$configuration,
                                    current$value$configuration)
    title <- "Locked data present"
    if (compatible) {
      class <- "success"
      body <- "Different to current"
    } else {
      class <- "warning"
      body <- paste("Locked model is structurally different to current",
                    "with different parameters or variables/outputs",
                    "results may not be meaningfully comparable, and",
                    "swapping is not supported.")
    }
    if (hidden) {
      body <- paste(body, "(but hidden)")
    }
  }
  simple_panel(class, title, body)
}
