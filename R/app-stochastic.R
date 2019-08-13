
odin_stochastic <- function(path) {
  shiny::shinyApp(ui = odin_stochastic_ui(),
                  server = odin_stochastic_server(path))
}


odin_stochastic_ui <- function() {
  shiny::shinyUI(
    shiny::tagList(
      odin.ui:::odin_css(),
      shiny::navbarPage(
        "Stochasticity",
        id = "odin_ui_navvar",
        inverse = FALSE,
        shiny::tabPanel(
          "Code",
          icon = shiny::icon("align-justify"),
          odin.ui:::mod_model_static_ui("model")),
        shiny::tabPanel(
          "Visualise",
          icon = shiny::icon("search"),
          odin.ui:::mod_vis_ui("vis")),
        shiny::tabPanel(
          "Sensitivity",
          icon = shiny::icon("bars"),
          odin.ui:::mod_batch_ui("batch"))
      )))
}


odin_stochastic_server <- function(path) {
  function(input, output, session) {
    data <- link <- shiny::reactive(NULL)
    run_options <- odin.ui:::control_run_options(
      control_end_time = TRUE, replicates = TRUE, scale_time = TRUE,
      default_end_time = 100, default_replicates = 5)

    model <- shiny::callModule(
      odin.ui:::mod_model_static_server, "model", path, "Model")
    vis <- shiny::callModule(
      odin.ui:::mod_vis_server, "vis", data, model$result, link,
      run_options = run_options)
    batch <- shiny::callModule(
      odin.ui:::mod_batch_server, "batch", model$result, data, link,
      run_options = run_options)
  }
}
