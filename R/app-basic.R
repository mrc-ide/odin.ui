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
        odin.ui:::mod_editor_simple_ui("editor")),
      shiny::tabPanel(
        "Visualise",
        icon = shiny::icon("search"),
        odin.ui:::mod_vis_ui("vis")),
      shiny::tabPanel(
        "Sensitivity",
        icon = shiny::icon("bars"),
        odin.ui:::mod_batch_ui("batch")),
      shiny::tabPanel(
        shiny::tagList(
          "Load/Save",
          odin.ui:::mod_status_ui("status")),
        icon = shiny::icon("list"),
        odin.ui:::mod_state_ui("state"))))
}


odin_basic_server <- function(initial_code) {
  function(input, output, session) {
    model <- shiny::callModule(
      odin.ui:::mod_editor_simple_server, "editor", initial_code,
      "Return to the Editor tab")
    data <- link <- shiny::reactive(NULL)

    run_options <- odin.ui:::control_run_options(control_end_time = TRUE)
    vis <- shiny::callModule(
      odin.ui:::mod_vis_server, "vis", data, model$result, link,
      run_options = run_options)
    batch <- shiny::callModule(
      odin.ui:::mod_batch_server, "batch", model$result, data, link,
      run_options = run_options)

    modules <- odin.ui:::submodules(model = model, vis = vis, batch = batch)
    state <- shiny::callModule(
      odin.ui:::mod_state_server, "state", modules, "prototype")
    status <- shiny::callModule(
      odin.ui:::mod_status_server, "status",
      list(edit = model$result))
  }
}
