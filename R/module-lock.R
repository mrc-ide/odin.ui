mod_lock_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("control_locked"))
}


mod_lock_server <- function(input, output, session, render, current,
                            set_current) {
  rv <- shiny::reactiveValues()

  output$control_locked <- shiny::renderUI({
    lock_control(render(), session$ns)
  })

  shiny::observeEvent(
    input$set, {
      result <- current()
      if (isTRUE(result$success)) {
        rv$locked <- result
      }
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

  output$status <- shiny::renderUI({
    lock_status(rv$locked, current())
  })

  get_state <- function() {
    rv$locked
  }

  set_state <- function(state) {
    rv$locked <- state
  }

  list(result = shiny::reactive(rv$locked),
       get_state = get_state,
       set_state = set_state)
}


lock_control <- function(render, ns) {
  if (!isTRUE(render)) {
    return(NULL)
  }

  mod_model_control_section(
    "Locked parameter set",
    shiny::uiOutput(ns("status")),
    shiny::actionButton(ns("set"), "Lock current",
                        shiny::icon("lock"),
                        class = "btn-blue"),
    shiny::actionButton(ns("clear"), "Clear locked",
                        shiny::icon("trash"),
                        class = "btn-danger"),
    shiny::actionButton(ns("swap"), "Swap locked and current",
                        shiny::icon("refresh"),
                        class = "btn-grey"),
    ns = ns, collapsed = TRUE)
}


lock_status <- function(locked, current) {
  if (is.null(locked)) {
    class <- "danger"
    title <- "No locked data"
    body <- NULL
  } else if (identical(locked, current)) {
    class <- "warning"
    title <- "Locked data present"
    body <- "Same as current"
  } else {
    class <- "success"
    title <- "Locked data present"
    body <- "Different to current"
  }
  simple_panel(class, title, body)
}
