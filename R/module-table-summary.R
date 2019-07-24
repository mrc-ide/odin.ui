mod_table_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_table_summary_server <- function(input, output, session, result) {
  rv <- shiny::reactiveValues()

  output$ui <- shiny::renderUI({
    table_summary_ui(result(), session$ns)
  })

  output$table <- shiny::renderDataTable(
    table_summary_data(result()),
    options = list(paging = FALSE, dom = "t", searching = FALSE))

  NULL
}


table_summary_ui <- function(result, ns) {
  if (isTRUE(result$success)) {
    target <- ns("container")
    icon <- shiny::icon(sprintf("%s fa-lg", "table"))
    head <- shiny::div(class = "panel-heading", icon,
                       "Show final variables",
                       "data-toggle" = "collapse",
                       "data-target" = paste0("#", target))
    body <- shiny::div(class = "panel-body collapse", id = target,
                       shiny::dataTableOutput(ns("table")))
    shiny::div(
      class = "row",
      shiny::div(
        class = "panel-group",
        shiny::div(
          class = "panel panel-info", head, body)))
  }
}


## At the moment this is rigged up for the *compare* case, but in
## general we'll want this to work for the simpler single model too.
## Not sure how we easily tell the difference.
table_summary_data <- function(result) {
  if (!isTRUE(result$success) || is.null(result$value)) {
    return(NULL)
  }

  simulation <- result$value$simulation
  if (is.null(names(simulation)) && length(simulation) == 2L) {
    end <- lapply(result$value$simulation, function(x)
      utils::tail(x$smooth, 1))
    d <- as.data.frame(rbind_laxly(end[[1]], end[[2]]))
    cbind(model = result$value$configuration$names$long, d,
          stringsAsFactors = FALSE)
  } else {
    stop("Writeme")
  }
}
