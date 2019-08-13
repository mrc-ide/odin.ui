odin_sidebar <- function(run, reset, auto_run, controls, status = NULL) {
  if (is.character(auto_run)) {
    auto_run <- shiny::div(
      class = "col-sm-12",
      raw_checkbox_input(auto_run, "Auto run", value = FALSE))
  }
  if (is.character(run)) {
    run <- shiny::actionButton(
      run, "Run model", shiny::icon("play"), class = "btn-blue")
  }
  reset <- shiny::actionButton(
    reset, "Reset", shiny::icon("refresh"), class = "btn-danger ml-2")

  shiny::div(
    class = "col-sm-4 col-lg-3",
    shiny::div(class = "odin-sidebar-buttons", reset, run, auto_run),
    shiny::div(class = "odin-sidebar-status", status),
    shiny::tags$form(
      class = "form-horizontal",
      shiny::div(class = "list-group odin-options", controls)))
}


help_modal <- function(path, title = "odin help") {
  shiny::modalDialog(shiny::includeMarkdown(path), title = title,
                     size = "l", easyClose = TRUE, fade = FALSE)
}


help_button <- function(id, title = "Help") {
  shiny::actionButton(id, title, class = "btn-warning",
                      icon = shiny::icon("question-circle"))
}


odin_control_section <- function(title, ..., ns, collapsed = FALSE) {
  id <- ns(sprintf("hide_%s", gsub(" ", "_", tolower(title))))

  head <- shiny::a(
    class = "list-group-item",
    "data-toggle" = "collapse",
    "href" = paste0("#", id),
    title,
    shiny::icon("sort", lib = "font-awesome", class = "pull-right"))

  css_class <- if (collapsed) "" else " in"

  body <- shiny::div(
    id = id,
    class = paste0("pt-4 collapse", css_class),
    ...)

  list(head, body)
}
