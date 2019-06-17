odin_prototype <- function(initial_code) {
  shiny::shinyApp(ui = odin_prototype_ui(initial_code),
                  server = odin_prototype_server(initial_code))
}


odin_prototype_ui <- function(initial_code) {
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
        shiny::uiOutput("status", inline = TRUE),
        icon = shiny::icon("list"),
        shiny::tagList(
          shiny::h2("Status"),
          shiny::hr(),

          shiny::div(
            class = "pull-right",
            shiny::div(
              class = "form-inline mt-5",
              shiny::div(
                class = "form-group",
                raw_text_input("download_filename", placeholder = "filename",
                               value = "")),
              shiny::downloadButton("download_everything", "Download",
                                    class = "btn-blue"))),
          shiny::column(
            6,
            file_input("restore",
                       "Load",
                       multiple = FALSE,
                       accept = c("application/octet-stream", ".rds"),
                       button_class = "btn-grey"))))))
}


odin_prototype_server <- function(initial_code) {
  function(input, output, session) {
    state <- shiny::reactiveValues(state = NULL)

    rv <- shiny::reactiveValues(status = NULL)

    data_tab <- "Return to the Data tab"
    editor_tab <- "Return to the Editor tab"
    configure_tab <- "Return to the Configure tab"

    data <- shiny::callModule(mod_csv_server, "odin_csv", data_tab)
    model <- shiny::callModule(
      mod_editor_simple_server, "odin_editor", initial_code, editor_tab)
    configure <- shiny::callModule(
      mod_configure_server, "odin_configure", data$result, model$result,
      configure_tab)

    fit <- shiny::callModule(
      mod_fit_server, "odin_fit", data$result, model$result, configure$result)
    vis <- shiny::callModule(
      mod_vis_server, "odin_vis", data$result, model$result, configure$result,
      fit$user)
    batch <- shiny::callModule(
      mod_batch_server, "odin_batch", model$result, data$result,
      configure$result, fit$user)

    output$status <- shiny::renderUI({
      class_data <- text_module_status(data$result()$status)
      class_model <- text_module_status(model$result()$status)
      class_configure <- text_module_status(configure$result()$status)
      class_fit <- text_module_status(fit$result()$status)
      shiny::tagList(
        "Status",
        shiny::icon("table", class = class_data),
        shiny::icon("edit", class = class_model),
        shiny::icon("random", class = class_configure),
        shiny::icon("calculator", class = class_fit))
    })

    output$download_everything <- shiny::downloadHandler(
      filename = function() {
        state_filename(input$download_filename)
      },
      content = function(con) {
        dat <- list(data = data$get_state(),
                    model = model$get_state(),
                    configure = configure$get_state(),
                    fit = fit$get_state())
        saveRDS(dat, con)
      })

    shiny::observeEvent(
      input$restore, {
        state <- readRDS(input$restore$datapath)
        shiny::isolate({
          data$set_state(state$data)
          model$set_state(state$model)
          configure$set_state(state$configure)
          fit$set_state(state$fit)
        })
      })
  }
}


state_filename <- function(filename) {
  if (!is.null(filename) && nzchar(filename)) {
    filename <- ensure_extension(filename, "rds")
  } else {
    filename <- sprintf("odin-%s.rds", date_string())
  }
  filename
}
