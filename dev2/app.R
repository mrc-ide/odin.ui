devtools::load_all()
## odin_ui_csv_app(run = FALSE)

code <- readLines("../anne/model/model1.R")

odin_fit_ui <- function() {
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
        "Model",
        icon = shiny::icon("edit"),
        shiny::h2("EDITOR")),
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


odin_fit_server <- function() {
  function(input, output, session) {
    data <- shiny::callModule(mod_csv_server, "odin_csv")
  }
}


shiny::shinyApp(ui = odin_fit_ui(),
                server = odin_fit_server())
