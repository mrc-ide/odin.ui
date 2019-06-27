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
          "Status",
          shiny::uiOutput("status", inline = TRUE)),
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

    modules <- list(date = data, model = model, link = link,
                    vis = vis, fit = fit, batch = batch)

    output$status <- shiny::renderUI({
      class_data <- text_module_status(data$result()$status)
      class_model <- text_module_status(model$result()$status)
      class_link <- text_module_status(link$result()$status)
      class_fit <- text_module_status(fit$result()$status)
      shiny::tagList(
        shiny::icon("table", class = class_data),
        shiny::icon("edit", class = class_model),
        shiny::icon("random", class = class_link),
        shiny::icon("calculator", class = class_fit))
    })

    output$download_everything <- shiny::downloadHandler(
      filename = function() {
        app_filename(input$download_filename)
      },
      content = function(con) {
        saveRDS(app_get_state(modules), con)
      })

    shiny::observeEvent(
      input$restore, {
        app_set_state(readRDS(input$restore$datapath), modules)
      })
  }
}


app_filename <- function(filename, prefix = "odin") {
  if (!is.null(filename) && nzchar(filename)) {
    filename <- ensure_extension(filename, "rds")
  } else {
    filename <- sprintf("%s-%s.rds", prefix, date_string())
  }
  filename
}


app_get_state <- function(modules) {
  lapply(modules, function(m) m$get_state())
}


app_set_state <- function(state, modules) {
  for (i in names(modules)) {
    modules[[i]]$set_state(state[[i]])
  }
}
