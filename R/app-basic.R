odin_basic <- function(initial_code) {
  shiny::shinyApp(ui = odin_basic_ui(),
                  server = odin_basic_server(initial_code))
}


odin_basic_ui <- function() {
  shiny::shinyUI(
    shiny::navbarPage(
      "odin editor",
      id = "odin_ui_navbar",
      inverse = TRUE,
      shiny::tabPanel(
        "Editor",
        icon = shiny::icon("edit"),
        mod_editor_simple_ui("editor")),
      shiny::tabPanel(
        "Visualise",
        icon = shiny::icon("search"),
        mod_vis_ui("vis")),
      shiny::tabPanel(
        "Sensitivity",
        icon = shiny::icon("bars"),
        mod_batch_ui("batch")),
      shiny::tabPanel(
        shiny::tagList(
          "Load/Save",
          mod_status_ui("status")),
        icon = shiny::icon("list"),
        mod_state_ui("state"))))
}


odin_basic_server <- function(initial_code) {
  function(input, output, session) {
    model <- shiny::callModule(
      mod_editor_simple_server, "editor", initial_code,
      "Return to the Editor tab")
    data <- link <- shiny::reactive(NULL)

    run_options <- control_run_options(control_end_time = TRUE)
    vis <- shiny::callModule(
      mod_vis_server, "vis", data, model$result, link,
      run_options = run_options)
    batch <- shiny::callModule(
      mod_batch_server, "batch", model$result, data, link,
      run_options = run_options)

    modules <- submodules(model = model, vis = vis, batch = batch)
    state <- shiny::callModule(
      mod_state_server, "state", modules, "basic")
    status <- shiny::callModule(
      mod_status_server, "status",
      list(edit = model$result))
  }
}
