raw_numeric_input <- function(inputId, value) {
    value <- restoreInput(id = inputId, default = value)
    tags$input(id = inputId, type = "number", class = "form-control", value = value, style = "width: 100px")
}

raw_checkbox_input <- function (inputId, label, value = FALSE)
{
    value <- shiny::restoreInput(id = inputId, default = value)
    inputTag <- shiny::tags$input(id = inputId, type = "checkbox")

    if (!is.null(value) && value){
        inputTag$attribs$checked <- "checked"
    }

    shiny::div(class = "checkbox", shiny::tags$label(inputTag, shiny::tags$span(label)))
}

raw_select_input <- function (inputId, choices, selected = NULL, size = NULL)
{
    selected <- restoreInput(id = inputId, default = selected)
    if (is.null(selected)) {
        selected <- choices[[1]][1]
    }
    else {
        selected <- as.character(selected)
    }

    options <- lapply(choices, function (choice) { shiny::tags$option(choice, value=choice) })

    shiny::tags$select(id = inputId, class = "form-control", size = size, options)
}

raw_text_input <- function (inputId, value = "", placeholder = NULL)
{
    value <- restoreInput(id = inputId, default = value)
    tags$input(id = inputId, type = "text", class = "form-control", value = value, placeholder = placeholder)
}

horizontal_form_group <- function(label_name, input, label_width = 6, label_class = "") {
    shiny::div(class = "form-group",
    shiny::tags$label(label_name, class=paste0(label_class, paste0(" control-label col-sm-", label_width))),
    shiny::div(class=paste0("col-sm-", 12 - label_width), input)
    )
}
