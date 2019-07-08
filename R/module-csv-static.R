mod_csv_static_ui <- function(id, path_docs) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Upload data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(shiny::includeMarkdown(path_docs)),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("data_plot")),
        shiny::dataTableOutput(ns("data_table")))))
}


mod_csv_static_server <- function(input, output, session,
                                  path_csv, name_time) {
  data <- csv_static_import(path_csv, name_time)

  output$data_plot <- plotly::renderPlotly({
    csv_plot(data$result)
  })

  output$data_table <- shiny::renderDataTable(
    data$imported$value$data,
    options = list(paging = FALSE, dom = "t", searching = FALSE))

  list(result = shiny::reactive(add_status(data$result, data$status)))
}


csv_static_import <- function(path, name_time) {
  imported <- csv_import(path, basename(path), min_cols = 2, min_rows = 1)
  result <- csv_result(imported$value, name_time)
  status <- csv_status(result, NULL)

  list(imported = imported, result = result, status = status)
}


csv_static_docs <- function(path_docs) {
  path_docs %||% odin_ui_file("md/csv_static.md")
}
