mod_parameters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_parameters_server <- function(input, output, session, pars,
                                  with_option = FALSE, title = NULL,
                                  download_prefix = "parameters") {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$configuration <- parameters_configuration(pars(), with_option, title)
  })

  output$ui <- shiny::renderUI({
    parameters_ui(rv$configuration, session$ns)
  })

  shiny::observe({
    pars <- rv$configuration$pars
    rv$values <- get_inputs(input, pars$id_value, pars$name)
    output$status <- NULL
  })

  output$download <- shiny::downloadHandler(
    filename = function() {
      parameters_filename(input$download_filename, download_prefix)
    },
    content = function(con) {
      write_csv(list_to_df(rv$values), con)
    })

  shiny::observeEvent(
    input$upload, {
      res <- parameters_validate_file(
        input$upload$datapath, rv$configuration$pars)
      if (res$success) {
        set_inputs(session, res$value$id_value, res$value$value)
      }
      output$status <- shiny::renderUI(parameters_status(res))
    })

  set <- function(values, notify = TRUE) {
    res <- parameters_validate_user(values, rv$configuration$pars)
    if (res$success) {
      set_inputs(session, res$value$id_value, res$value$value)
    }
    output$status <- shiny::renderUI(parameters_status(res, notify))
    res$success
  }

  list(
    result = shiny::reactive(rv$values),
    set = set)
}

parameters_configuration <- function(pars, with_option, title) {
  if (is.null(pars)) {
    return(NULL)
  }
  pars$id_value <- sprintf("value_%s", pars$name)
  pars$id_option <- sprintf("option_%s", pars$name)
  title <- title %||% "Model parameters"
  list(pars = pars, with_option = with_option, title = title)
}


parameters_ui <- function(configuration, ns, restore = NULL) {
  if (is.null(configuration)) {
    return(NULL)
  }
  pars <- configuration$pars

  ## TODO: the id_value bit comes out to the configuration within the
  ## module
  value <- restore$value %||% pars$value
  if (configuration$with_option) {
    f <- function(name, id_value, value, id_option, option) {
      shiny::fluidRow(
        shiny::column(
          10,
          simple_numeric_input(name, id_value, value)),
        shiny::column(
          2, shiny::checkboxInput(id_option, "", option)))
    }
    option <- restore$option %||% pars$option
    controls <- Map(f, pars$name, ns(pars$id_value),
                    value, ns(pars$id_option), option)
  } else {
    controls <- unname(Map(simple_numeric_input,
                           pars$name, ns(pars$id_value), value))
  }

  if ("group" %in% names(pars)) {
    controls <- split(controls, pars$group, drop = TRUE)
    f <- function(label, value) {
      shiny::div(shiny::tags$b(label), value)
    }
    controls <- unname(Map(f, names(controls), controls))
  }

  status <- shiny::uiOutput(ns("status"))

  tags <- shiny::tagList(
    controls, shiny::hr(), parameters_io(ns), status)

  mod_model_control_section(configuration$title, tags, ns = ns)
}


parameters_io <- function(ns) {
  shiny::div(
    style = "margin-left:15px", # HACK: undo a later subtraction
    shiny::div(
      class = "form-inline mt-5",
      shiny::div(
        class = "form-group",
        raw_text_input(ns("download_filename"), placeholder = "filename",
                       value = "")),
      shiny::downloadButton(ns("download"), "Download", class = "btn-blue")),
    shiny::fileInput(ns("upload"),
                     "Upload parameters:",
                     accept = accept_csv()))
}


parameters_filename <- function(filename, prefix) {
  if (!is.null(filename) && nzchar(filename)) {
    filename <- ensure_extension(filename, "csv")
  } else {
    filename <- sprintf("odin-%s-%s.csv", prefix, date_string())
  }
  filename
}


parameters_validate_file <- function(path, pars) {
  input <- with_success(read_csv(path))
  if (!input$success) {
    return(input)
  }
  msg <- setdiff(c("name", "value"), names(input$value))
  if (length(msg) > 0L) {
    return(unsuccessful(sprintf("Missing columns: %s",
                                paste(msg, collapse = ", "))))
  }
  parameters_validate_user(df_to_list(input$value), pars)
}


parameters_validate_user <- function(values, pars) {
  msg <- setdiff(pars$name, names(values))
  if (length(msg) > 0L) {
    return(unsuccessful(sprintf("Missing parameters: %s",
                                paste(msg, collapse = ", "))))
  }
  extra <- setdiff(names(values), pars$name)
  if (length(msg) > 0L) {
    return(unsuccessful(sprintf("Extra parameters: %s",
                                paste(extra, collapse = ", "))))
  }
  pars$value <- list_to_numeric(values[pars$name])

  successful(pars[c("name", "id_value", "value")])
}


parameters_status <- function(res, notify) {
  if (notify) {
    if (res$success) {
      simple_panel("success", "Parameters updated", NULL)
    } else {
      simple_panel("danger", "Error uploading parameters", res$error)
    }
  }
}
