odin_prototype <- function(initial_code) {
  shiny::shinyApp(ui = odin_prototype_ui(),
                  server = odin_prototype_server(initial_code))
}


odin_prototype_ui <- function() {
  shiny::shinyUI(
    shiny::navbarPage(
      "odin editor",
      id = "odin_ui_navbar",
      inverse = TRUE,
      shiny::tabPanel(
        "Data",
        icon = shiny::icon("table"),
        mod_csv_ui("odin_csv")),
      shiny::tabPanel(
        "Editor",
        icon = shiny::icon("edit"),
        mod_editor_simple_ui("odin_editor")),
      shiny::tabPanel(
        "Link",
        icon = shiny::icon("random"),
        mod_link_ui("odin_link")),
      shiny::tabPanel(
        "Visualise",
        icon = shiny::icon("search"),
        mod_vis_ui("odin_vis")),
      shiny::tabPanel(
        "Fit",
        icon = shiny::icon("calculator"),
        mod_fit_ui("odin_fit")),
      shiny::tabPanel(
        "Sensitivity",
        icon = shiny::icon("bars"),
        mod_batch_ui("odin_batch")),
      shiny::tabPanel(
        shiny::tagList(
          "Load/Save",
          mod_status_ui("status")),
        icon = shiny::icon("list"),
        mod_state_ui("state"))))
}


odin_prototype_server <- function(initial_code) {
  function(input, output, session) {
    data <- shiny::callModule(
      mod_csv_server, "odin_csv",
      "Return to the Data tab")
    model <- shiny::callModule(
      mod_editor_simple_server, "odin_editor", initial_code,
      "Return to the Editor tab")
    link <- shiny::callModule(
      mod_link_server, "odin_link", data$result, model$result,
      "Return to the Link tab")

    fit <- shiny::callModule(
      mod_fit_server, "odin_fit", data$result, model$result, link$result)
    vis <- shiny::callModule(
      mod_vis_server, "odin_vis", data$result, model$result, link$result,
      import_from_fit(fit$user))
    batch <- shiny::callModule(
      mod_batch_server, "odin_batch", model$result, data$result, link$result,
      import_from_fit(fit$user))

    modules <- submodules(data = data, model = model, link = link,
                          vis = vis, fit = fit, batch = batch)
    state <- shiny::callModule(
      mod_state_server, "state", modules, "prototype")
    status <- shiny::callModule(
      mod_status_server, "status",
      list(table = data$result, edit = model$result,
           random = link$result, calculator = fit$result))
  }
}
