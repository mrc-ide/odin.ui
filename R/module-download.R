mod_download_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_download_server <- function(input, output, session, data, prefix) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$names <- data()$configuration$download_names
  })

  output$ui <- shiny::renderUI({
    download_ui(session$ns, rv$names)
  })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      download_filename(
        input$download_filename, prefix, input$download_type, rv$names)
    },
    content = function(filename) {
      download_data(filename, data()$simulation, input$download_type, rv$names)
    })
}


download_ui <- function(ns, names) {
  if (is.null(names)) {
    return(NULL)
  }
  shiny::div(
    class = "form-inline mt-5",
    shiny::div(
      class = "form-group",
      raw_text_input(
        ns("download_filename"), placeholder = "filename", value = "")),
    shiny::div(
      class = "form-group",
      raw_select_input(ns("download_type"), choices = names$display)),
    shiny::downloadButton(
      ns("download_button"), "Download", class = "btn-blue"))
}



download_filename <- function(filename, prefix, type, names) {
  if (is_missing(filename)) {
    suffix <- names$filename[[match(type, names$display)]]
    filename <- sprintf("odin-%s-%s-%s.csv", prefix, suffix, date_string())
  } else {
    filename <- ensure_extension(filename, "csv")
  }
  filename
}


download_data <- function(filename, data, type, names) {
  data <- data[[match(type, names$display)]]
  write_csv(data, filename)
}


download_names <- function(x, display = NULL, filename = NULL, data = NULL) {
  ret <- list(display = display %||% x,
              data = data %||% x,
              filename = filename %||% clean_name(x))
  n <- lengths(ret)
  if (length(unique(n)) != 1 || n[[1]] == 0) {
    stop("Invalid inputs")
  }
  ret
}
