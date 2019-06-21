update_select_input <- function(session, id, data) {
  shiny::updateSelectInput(session, id,
                           choices = data$choices, selected = data$selected)
}
