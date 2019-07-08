mod_table_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_table_summary_server <- function(input, output, session, result) {
  rv <- shiny::reactiveValues()
  ns <- session$ns

  output$ui <- shiny::renderUI({
    if (!is.null(result())) {
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
  })

  output$table <- shiny::renderDataTable(
    table_summary_data(result()),
    options = list(paging = FALSE, dom = "t", searching = FALSE))
}


## At the moment this is rigged up for the *compare* case, but in
## general we'll want this to work for the simpler single model too.
## Not sure how we easily tell the difference.
table_summary_data <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }

  if (!identical(names(result$value$simulation),
                 c("model1", "model2", "user"))) {
    stop("Writeme")
  }

  d <- as.data.frame(rbind_laxly(
    tail(result$value$simulation$model1, 1),
    tail(result$value$simulation$model2, 1)))
  cbind(model = result$value$configuration$names$long, d,
        stringsAsFactors = FALSE)
}


## TODO: respect exclusion lists
rbind_laxly <- function(a, b) {
  v <- union(colnames(a), colnames(b))
  ma <- setdiff(v, colnames(a))
  mb <- setdiff(v, colnames(b))
  res <- rbind(
    cbind(a, matrix(NA, nrow(a), length(ma), dimnames = list(NULL, ma))),
    cbind(b, matrix(NA, nrow(b), length(mb), dimnames = list(NULL, mb))))
  rownames(res) <- NULL
  res
}
