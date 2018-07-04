editor_metadata_modal <- function(pars, data, success, ns) {
  ## Some of the prep here might be helpful to do elsewhere.
  defaults <- setNames(pars$default_value, pars$name)

  ## Most of the validation could be put into javascript too, which
  ## would reduce load considerably - none of the validation is
  ## particularly hard!  Just need to look up how to do it!  But it's
  ## also going to be a reasonable choice to get this done serverside
  ## for now and then later on move it.

  ## The other way of doing this, which would probably be nicer
  ## overall is to use shinyjs to update the class and title
  ## attributes of elements dynamically.  That would avoid redrawing
  ## the modal so many times.  But the logic remains essentially the
  ## same.  To get smooth redraws, fade is off.  It's not a great loss
  ## in any case though.

  ## Bootstrap tooltips would be *heaps* nicer; they appear straight
  ## away for example:
  ## https://www.w3schools.com/bootstrap/bootstrap_tooltip.asp -
  ## there's just some faff to get them included properly.
  modal_cell <- function(name, part) {
    if (!is.null(data)) {
      value <- data[[name]][[part]]
    } else if (part == "default") {
      value <- defaults[[name]]
    } else {
      value <- NA
    }
    if (is.na(value) && name == "t" && part == "max") {
      value <- 10
    }
    id <- ns(sprintf("%s_%s", name, part))
    type <- if (part == "description") "text" else "number"
    title <- data[[name]]$err[[part]]
    placeholder <- if (part == "description") "Optional description"
    class <- paste(c("form-control shiny-bound-input",
                     if (type == "number") "no-spinners",
                     if (!is.null(title)) "invalid-input"),
                   collapse = " ")
    shiny::tags$input(id = id, type = type, class = class, value = value,
                      title = title, placeholder = placeholder)
  }

  modal_row <- function(p) {
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(p), align = "right"),
      shiny::tags$td(modal_cell(p, "min")),
      shiny::tags$td(modal_cell(p, "default")),
      shiny::tags$td(modal_cell(p, "max")),
      shiny::tags$td(modal_cell(p, "description")))
  }
  modal_row_time <- function() {
    ## TODO: class against this tr for a thicker top and more margin
    ## to separate, but _only_ if parameters exist.
    shiny::tags$tr(
      shiny::tags$td("Time", align = "right"),
      shiny::tags$td(modal_cell("t", "min")),
      shiny::tags$td(),
      shiny::tags$td(modal_cell("t", "max")),
      shiny::tags$td())
  }

  rows <- c(lapply(pars$name, modal_row), list(modal_row_time()))

  validation_message <- paste(
    "There were validation errors; hover over highlighted cells for",
    "more details")

  shiny::modalDialog(
    shiny::h3("Parameters"),
    shiny::tags$table(
      class = "table",
      shiny::tags$thead(
        shiny::tags$td(""),
        shiny::tags$td("Min", class = "col-sm-2"),
        shiny::tags$td("Default", class = "col-sm-2"),
        shiny::tags$td("Max", class = "col-sm-2"),
        shiny::tags$td("Description")),
      shiny::tags$tbody(rows)),

    if (!is.null(data)) {
      shiny::p(validation_message)
    },
    size = "l",
    fade = FALSE,
    footer = shiny::tagList(
      shiny::actionButton(ns("editor_metadata_ok"), "OK",
                          class = "btn-primary"),
      shiny::modalButton("Cancel")))
}


editor_metadata_validate <- function(pars, input) {
  validate_par <- function(p) {
    res <- list(min = input[[sprintf("%s_min", p)]],
                max = input[[sprintf("%s_max", p)]],
                default = input[[sprintf("%s_default", p)]],
                description = input[[sprintf("%s_description", p)]])
    err <- list()

    if (is.na(res$default)) {
      err$default <- "Default value must be given"
    }

    if (is.na(res$min) && is.na(res$max)) {
    } else if (!is.na(res$min) && !is.na(res$max)) {
      if (res$min >= res$max) {
        err$min <- "Min must be less than max"
        err$max <- "Max must be more than min"
      } else {
        res$range <- c(res$min, res$max)
      }
      if (!is.na(res$default)) {
        if (res$default < res$min) {
          err$default <- "Default must be at least min"
        } else if (res$default > res$max) {
          err$default <- "Default must be at most max"
        }
      }
    } else if (is.na(res$min)) {
      err$min <- "Min must be given if max is"
    } else {
      err$max <- "Max must be given if min is"
    }
    res$err <- err
    res$ok <- length(err) == 0L
    res
  }

  validate_time <- function() {
    res <- list(min = input$t_min, max = input$t_max)
    err <- list()
    if (is.na(res$max)) {
      err$max <- "Max (end) time must be given"
    } else if (is.na(res$min) && res$max <= 0) {
      err$max <- "If min is missing, max must be positive"
    } else if (!is.na(res$min) && !is.na(res$max) && res$min >= res$max) {
      err$max <- "Max must be at greater than min"
    }
    res$err <- err
    res$ok <- length(err) == 0L
    if (res$ok) {
      res$range <- c(if (!is.na(res$min)) res$min, res$max)
    }
    res
  }

  data <- c(setNames(lapply(pars$name, validate_par), pars$name),
            list(t = validate_time()))

  list(data = data,
       success = all(vapply(data, "[[", TRUE, "ok")))
}
