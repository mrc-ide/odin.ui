mod_state_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    remotesave::mod_cookies_ui(ns("cookies")),
    mod_help_ui(ns("help"), class = "pull-right"),
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
                 button_class = "btn-blue")),
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
        raw_text_input(ns("download_filename"), placeholder = "filename",
                       value = "")),
      shiny::downloadButton(ns("save"), "Download", class = "btn-blue")),
    shiny::hr(),
    shiny::h2("Remote"),
    remotesave::mod_remotesave_ui(ns("save")),
    shiny::hr(),
    odin_ui_version_information())
}


mod_state_server <- function(input, output, session, modules, app_name) {
  get_state <- function() {
    state_save(modules, app_name)
  }
  set_state <- function(state) {
    state_load(state, modules, app_name)
  }

  name_cookie <- Sys.getenv("ODIN_UI_USER", "odinuiuser")
  root_prefix <- Sys.getenv("ODIN_UI_ROOT", "odin.ui")
  valid <- 30

  cookies <- shiny::callModule(
    remotesave::mod_cookies_server, "cookies", name_cookie, valid)
  root <- shiny::reactive(sprintf(
    "%s:%s:%s", root_prefix, session$clientData$url_pathname, app_name))
  user <- shiny::reactive(cookies$value())
  save <- shiny::callModule(
    remotesave::mod_remotesave_server, "save",
    root, user, get_state, set_state)

  rv <- shiny::reactiveValues()

  help <- shiny::callModule(
    mod_help_server, "help", odin_ui_file("md/help/state.md"))

  output$save <- shiny::downloadHandler(
    filename = function() {
      state_filename(input$download_filename)
    },
    content = function(con) {
      saveRDS(get_state(), con)
    },
    contentType = "application/octet-stream")

  shiny::observeEvent(
    input$load, {
      rv$result <- with_success(set_state(readRDS(input$load$datapath)))
    })

  output$status <- shiny::renderUI({
    state_status(rv$result)
  })
}


state_save <- function(modules, name) {
  data <- modules$get_state()
  list(name = name, time = Sys.time(), data = data)
}


state_load <- function(state, modules, name) {
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
