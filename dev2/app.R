devtools::load_all()
## odin_ui_csv_app(run = FALSE)

odin_fit_ui <- function(initial_code) {
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
        mod_editor_simple_ui("odin_editor", initial_code, NULL)),
      shiny::tabPanel(
        "Configure",
        icon = shiny::icon("random"),
        shiny::h2("CONFIGURE")),
      shiny::tabPanel(
        "Visualise",
        icon = shiny::icon("search"),
        shiny::h2("VISUALISE")),
      shiny::tabPanel(
        "Fit",
        icon = shiny::icon("calculator"),
        shiny::h2("VISUALISE"))))
}


odin_fit_server <- function(initial_code) {
  function(input, output, session) {
    data <- shiny::callModule(mod_csv_server, "odin_csv")
    editor <- shiny::callModule(
      mod_editor_simple_server, "odin_editor", initial_code)
  }
}


code <- readLines("../anne/model/model1.R")
shiny::shinyApp(ui = odin_fit_ui(code),
                server = odin_fit_server(code))
