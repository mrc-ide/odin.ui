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

    data <- shiny::callModule(mod_csv_server, "odin_csv")
    model <- shiny::callModule(
      mod_editor_simple_server, "odin_editor", initial_code)
    configure <- shiny::callModule(
      mod_configure_server, "odin_configure", data$result, model$result)
    fit <- shiny::callModule(
      mod_fit_server, "odin_fit", data$result, model$result, configure$result)
    vis <- shiny::callModule(
      mod_vis_server, "odin_vis", data$result, model$result, configure$result,
      fit$pars)
    batch <- shiny::callModule(
      mod_batch_server, "odin_batch", model$result, data$result, fit$pars)

    shiny::observe({
      if (!isTRUE(data$result()$configured)) {
        status_data <- "missing"
      } else {
        status_data <- "ok"
      }

      dat <- model$result()
      if (is.null(dat)) {
        status_model <- "missing"
      } else if (dat$is_current) {
        status_model <- "ok"
      } else {
        status_model <- "outofdate"
      }

      if (!configure$result()$configured) {
        status_link <- "missing"
      } else {
        status_link <- status_model
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
