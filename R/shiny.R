update_select_input <- function(session, id, data) {
  shiny::updateSelectInput(session, id,
                           choices = data$choices, selected = data$selected)
}


clear_select_input <- function(session, id) {
  update_select_input(session, id, list(choices = character(0), selected = NA))
}


reactive_successful <- function(x) {
  shiny::reactive(isTRUE(x()$success))
}
