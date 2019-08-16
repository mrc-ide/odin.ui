odin_stochastic <- function(path, docs = NULL) {
  shiny::shinyApp(ui = odin_stochastic_ui(docs),
                  server = odin_stochastic_server(path))
}


odin_stochastic_ui <- function(docs = NULL) {
  shiny::shinyUI(
    shiny::tagList(
      odin_css(),
      shiny::navbarPage(
        "Stochasticity",
        id = "odin_ui_navvar",
        inverse = FALSE,
        shiny::tabPanel(
          "Code",
          icon = shiny::icon("align-justify"),
          mod_model_static_ui("model", docs)),
        shiny::tabPanel(
          "Visualise",
          icon = shiny::icon("search"),
          mod_vis_ui("vis")),
        shiny::tabPanel(
          "Sensitivity",
          icon = shiny::icon("bars"),
          mod_batch_ui("batch"))
      )))
}


odin_stochastic_server <- function(path) {
  function(input, output, session) {
    data <- link <- shiny::reactive(NULL)
    run_options <- control_run_options(
      control_end_time = TRUE, replicates = TRUE, scale_time = TRUE,
      default_end_time = 100, default_replicates = 5)

    model <- shiny::callModule(
      mod_model_static_server, "model", path, "Model")
    vis <- shiny::callModule(
      mod_vis_server, "vis", data, model$result, link,
      run_options = run_options)
    batch <- shiny::callModule(
      mod_batch_server, "batch", model$result, data, link,
      run_options = run_options)
  }
}
