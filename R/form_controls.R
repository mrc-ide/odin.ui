raw_numeric_input <- function(inputId, value) {
  value <- shiny::restoreInput(id = inputId, default = value)
  shiny::tags$input(id = inputId, type = "number", class = "form-control",
                    value = value, style = "width: 100px")
}

raw_checkbox_input <- function (inputId, label, value = FALSE) {
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- shiny::tags$input(id = inputId, type = "checkbox")

  if (!is.null(value) && value){
    inputTag$attribs$checked <- "checked"
  }

  shiny::div(class = "checkbox",
             shiny::tags$label(inputTag, shiny::tags$span(label)))
}

raw_select_input <- function (inputId, choices, selected = NULL, size = NULL) {
  selected <- shiny::restoreInput(id = inputId, default = selected)
  if (is.null(selected)) {
    selected <- choices[[1]][1]
  } else {
    selected <- as.character(selected)
  }

  options <- Map(function(label, value)
    shiny::tags$option(label, value = value),
    names(choices) %||% choices, choices)

  shiny::tags$select(id = inputId, class = "form-control", size = size,
                     options)
}

raw_text_input <- function (inputId, value = "", placeholder = NULL, ...) {
  value <- shiny::restoreInput(id = inputId, default = value)
  shiny::tags$input(id = inputId, type = "text", class = "form-control",
                    value = value, placeholder = placeholder, ...)
}

file_input <- function (inputId, label, multiple = FALSE, accept = NULL,
                        button_label = "Browse...", button_class = "btn-default", placeholder = "No file selected")
{
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- shiny::tags$input(id = inputId, name = inputId, type = "file",
                                style = "display: none;", `data-restore` = restoredValue)
  if (multiple){
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0){
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }

  if (!is.null(label)){
    label <- shiny::tags$label(label)
  }
  shiny::div(class = "form-group", label,
    shiny::div(class = "input-group", shiny::tags$div(class = "input-group-btn",
        shiny::tags$label(class = paste0("btn ", button_class), style="line-height: 1.5", button_label, inputTag)),
    shiny::tags$input(type = "text", class = "form-control", placeholder = placeholder, readonly = "readonly")),
    shiny::tags$div(id = paste(inputId, "_progress", sep = ""),
            class = "progress progress-striped active shiny-file-input-progress",
            shiny::tags$div(class = "progress-bar"))
  )
}


horizontal_form_group <- function(label_name, input, label_width = 6,
                                  label_class = "") {
  label_class <- sprintf("%s control-label col-sm-%d", label_class, label_width)
  shiny::div(
    class = "form-group",
    shiny::tags$label(label_name, class = label_class),
    shiny::div(class = sprintf("col-sm-%d", 12 - label_width), input))
}
