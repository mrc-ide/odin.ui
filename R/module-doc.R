mod_doc_config <- function(path_config) {
  assert_file_exists(path_config)
  config <- yaml::yaml.load_file(path_config)
  mod_doc_config_validate(config, dirname(path_config))
}


## In general validation of yaml is hard.  Getting this robust is
## going to be a job for later and only the basics are done here.
mod_doc_config_validate <- function(config, path) {
  expected <- c("title", "code", "docs", "default_time")
  stopifnot(all(expected %in% names(config)))

  path_code <- file.path(path, config$code)
  path_docs <- file.path(path, config$docs)

  assert_file_exists(path_code, "$code")
  assert_file_exists(path_docs, "$docs")

  model <- odin::odin_(path_code)

  list(path_code = path_code,
       path_docs = path_docs,
       code = read_text(path_code),
       model = model,
       title = config$title,
       default_time = config$default_time,
       parameters = config$parameters)
}


mod_doc_ui <- function(id, config) {
  ns <- shiny::NS(id)

  code <- shiny::tagList(
    shiny::h2("Code"),
    shinyAce::aceEditor(ns("code"), mode = "r", readOnly = TRUE,
                        value = config$code))
  docs <- shiny::includeMarkdown(config$path_docs)
  shiny::tagList(
    odin_css(),
    shiny::titlePanel(config$title),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Code & documentation",
        shiny::fluidRow(
          shiny::column(6, code),
          shiny::column(6, docs))),
      shiny::tabPanel(
        "Run",
        mod_model_ui(ns("model"), NULL))))
}


mod_doc_server <- function(input, output, session, config) {
  shiny::callModule(mod_model_server, "model",
                    config$model, config$default_time, config$parameters)
}
