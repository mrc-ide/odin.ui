mod_state_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h2("Load"),
    shiny::p(paste(
      "Load a previously saved state from this app.",
      "The state is specific to this application and loading a saved state",
      "from a previously loaded application will not work.")),
    shiny::div(
      class = "form-inline mt-5",
      file_input(ns("load"),
                 NULL,
                 multiple = FALSE,
                 accept = c("application/octet-stream", ".rds"),
                 button_class = "btn-grey")),
    shiny::uiOutput(ns("status")),
    shiny::hr(),
    shiny::h2("Save"),
    shiny::p(paste(
      "Save the state of this app.  This will create a file in your",
      "downloads folder with enough information to recreate the state of the",
      "app (data, model, graphs, etc)")),
    shiny::div(
      class = "form-inline mt-5",
      shiny::div(
        class = "form-group",
        raw_text_input("download_filename", placeholder = "filename",
                       value = "")),
      shiny::downloadButton(ns("save"), "Download", class = "btn-blue")))
}


mod_state_server <- function(input, output, session, modules, app_name) {
  rv <- shiny::reactiveValues()

  output$save <- shiny::downloadHandler(
    filename = function() {
      state_filename(input$download_filename)
    },
    content = function(con) {
      saveRDS(state_save(modules, app_name), con)
    })

  shiny::observeEvent(
    input$load, {
      rv$result <- with_success(
        state_load(input$load$datapath, modules, app_name))
    })

  output$status <- shiny::renderUI({
    state_status(rv$result)
  })
}


state_save <- function(modules, name) {
  data <- modules$get_state()
  list(name = name, time = Sys.time(), data = data)
}


state_load <- function(path, modules, name) {
  state <- readRDS(path)
  if (!identical(state$name, name)) {
    stop("Incorrect state")
  }
  modules$set_state(state$data)
  list(state = state, saved = state$time, loaded = Sys.time())
}


state_filename <- function(filename, prefix = "odin") {
  if (!is.null(filename) && nzchar(filename)) {
    filename <- ensure_extension(filename, "rds")
  } else {
    filename <- sprintf("%s-%s.rds", prefix, date_string())
  }
  filename
}


state_status <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (x$success) {
    msg <- sprintf("Saved on %s, loaded on %s", x$value$saved, x$value$loaded)
    simple_panel("success", "Loaded saved state", msg)
  } else {
    simple_panel("danger", "Error loading state state", x$error)
  }
}
