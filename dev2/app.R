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
        mod_configure_ui("odin_configure")),
      shiny::tabPanel(
        "Visualise",
        icon = shiny::icon("search"),
        shiny::h2("VISUALISE")),
      shiny::tabPanel(
        "Fit",
        icon = shiny::icon("calculator"),
        shiny::h2("VISUALISE")),
      shiny::tabPanel(
        shiny::uiOutput("status", inline = TRUE),
        icon = shiny::icon("list"),
        shiny::h2("STATUS"))))
}


odin_fit_server <- function(initial_code) {
  function(input, output, session) {
    data <- shiny::callModule(mod_csv_server, "odin_csv")
    model <- shiny::callModule(
      mod_editor_simple_server, "odin_editor", initial_code)
    configure <- shiny::callModule(
      mod_configure_server, "odin_configure", data, model)

    rv <- shiny::reactiveValues(status = NULL)
    shiny::observe({
      if (is.null(data())) {
        status_data <- "missing"
      } else {
        status_data <- "ok"
      }

      dat <- model()
      if (is.null(dat)) {
        status_model <- "missing"
      } else if (dat$is_current) {
        status_model <- "ok"
      } else {
        status_model <- "outofdate"
      }

      if (length(configure()) == 0) {
        status_link <- "missing"
      } else {
        status_link <- "ok"
      }

      rv$status <- list(data = status_data,
                        model = status_model,
                        link = status_link)
    })

    output$status <- shiny::renderUI({
      map <- c(missing = "text-danger",
               outofdate = "text-warning",
               ok = "text-success")
      shiny::tagList(
        "Status",
        shiny::icon("table", class = map[[rv$status$data]]),
        shiny::icon("edit", class = map[[rv$status$model]]),
        shiny::icon("random", class = map[[rv$status$link]]))
    })
  }
}


code <- readLines("../anne/model/model1.R")
shiny::shinyApp(ui = odin_fit_ui(code),
                server = odin_fit_server(code))
