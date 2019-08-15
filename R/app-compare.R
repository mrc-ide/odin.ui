odin_compare <- function() {
  shiny::shinyApp(ui = odin_compare_ui(),
                  server = odin_compare_server())
}


odin_compare_ui <- function() {
  shiny::shinyUI(
    shiny::tagList(
      odin_css(),
      shiny::navbarPage(
        "odin editor",
        id = "odin_ui_navbar",
        inverse = FALSE,
        shiny::tabPanel(
          "Base",
          icon = shiny::icon("align-justify"),
          mod_model_static_ui("model1")),
        shiny::tabPanel(
          "Editor",
          icon = shiny::icon("edit"),
          mod_editor_simple_ui("model2")),
        shiny::tabPanel(
          "Visualise",
          icon = shiny::icon("search"),
          mod_vis_compare_ui("vis")),
        shiny::tabPanel(
          "Sensitivity",
          icon = shiny::icon("bars"),
          mod_batch_compare_ui("batch")),
        shiny::tabPanel(
          shiny::tagList(
            "Load/Save",
            mod_status_ui("status")),
          icon = shiny::icon("list"),
          mod_state_ui("state")))))
}


odin_compare_server <- function() {
  function(input, output, session) {
    model1 <- shiny::callModule(
      mod_model_static_server, "model1",
      "models/malaria_model.R",
      "Base")

    model2 <- shiny::callModule(
      mod_editor_simple_server, "model2",
      readLines("models/malaria_model_latency.R"),
      "Return to the Editor tab")
    ## TODO: it should be possible to accept data here too (currently
    ## requires data missing and run_options sets control_end_time =
    ## TRUE)
    vis <- shiny::callModule(
      mod_vis_compare_server, "vis", model1$result, model2$result)
    batch <- shiny::callModule(
      mod_batch_compare_server, "batch", model1$result, model2$result)

    modules <- submodules(model1 = model1, model2 = model2,
                                    vis = vis, batch = batch)
    state <- shiny::callModule(
      mod_state_server, "state", modules, "prototype")
    status <- shiny::callModule(
      mod_status_server, "status", list(edit = model2$result))
  }
}
