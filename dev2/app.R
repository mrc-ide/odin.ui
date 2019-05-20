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
        odin_fit_configure_ui()),
      shiny::tabPanel(
        "Visualise",
        icon = shiny::icon("search"),
        shiny::h2("VISUALISE")),
      shiny::tabPanel(
        "Fit",
        icon = shiny::icon("calculator"),
        shiny::h2("VISUALISE"))))
}


odin_fit_configure_ui <- function() {
  shiny::tagList(
    shiny::titlePanel("Configure"),
    shiny::h3("Data"),
    shiny::textOutput("data_status"),
    shiny::selectInput("data_time_variable",
                       "Select time variable",
                       character(0)))
}


odin_fit_server <- function(initial_code) {
  function(input, output, session) {
    data <- shiny::callModule(mod_csv_server, "odin_csv")
    editor <- shiny::callModule(
      mod_editor_simple_server, "odin_editor", initial_code)

    rv <- shiny::reactiveValues()

    output$data_status <- shiny::renderText({
      if (is.null(data())) {
        msg <- "Please upload data"
      } else {
        msg <- sprintf("%d rows of data have been uploaded", nrow(data()))
        vars <- names(data())
        prev <- input$data_time_variable
        selected <- if (!is.null(prev) && prev %in% vars) prev else NULL
        shiny::updateSelectInput(session, "data_time_variable",
                                 choices = vars, selected = selected)
      }
      msg
    })
  }
}


code <- readLines("../anne/model/model1.R")
shiny::shinyApp(ui = odin_fit_ui(code),
                server = odin_fit_server(code))
