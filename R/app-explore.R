##' Create an odin app for exploring a model
##' @title Create odin app for exploring a model
##'
##' @param path_config Path to YAML configuration
##'
##' @param env Environment to evaluate any "extra" functions in.  If
##'   these functions are pure (and they should be!) this can be
##'   safely ignored.
##'
##' @export
odin_ui_explore_app <- function(path_config, env = .GlobalEnv) {
  config <- odin_ui_explore_config(path_config, env)
  shiny::shinyApp(odin_ui_explore_ui(config),
                  odin_ui_explore_server(config))
}


odin_ui_explore_ui <- function(config) {
  code <- shiny::tagList(
    shiny::h2("Code"),
    shinyAce::aceEditor("code", mode = "r", readOnly = TRUE,
                        value = read_text(config$code)))
  docs <- shiny::withMathJax(shiny::includeMarkdown(config$docs))
  title <- config$title

  if (config$explore_tab) {
    explore <- shiny::tabPanel(
      "Explore",
      icon = shiny::icon("cogs"),
      mod_parameter_input("odin_parameter", NULL))
  } else {
    explore <- NULL
  }

  ui <- shiny::shinyUI(
    shiny::tagList(
      odin_css(),
      shiny::navbarPage(
        title,
        id = "odin_ui_navbar",
        inverse = TRUE,
        shiny::tabPanel(
          "Code & documentation",
          icon = shiny::icon("edit"),
          shiny::fluidRow(
            shiny::column(6, code),
            shiny::column(6, docs))),
        shiny::tabPanel(
          "Run",
          icon = shiny::icon("play"),
          mod_model_ui("model", NULL)),
        explore,
        footer = odin_footer())))
}


odin_ui_explore_server <- function(config) {
  model <- odin::odin_(config$code)
  default_time <- config$default_time
  default_replicates <- config$default_replicates
  parameters <- config$parameters
  extra <- config$extra
  output_control <- config$output
  time_scale <- config$time_scale

  function(input, output, session) {
    shiny::callModule(mod_model_server, "model",
                      model, default_time, parameters, extra, output_control,
                      default_replicates, time_scale)
    shiny::callModule(mod_parameter_server, "odin_parameter",
                      model, default_time, parameters, extra, output_control,
                      time_scale)
  }
}


odin_ui_explore_config <- function(path_config, env) {
  config <- yaml_read(path_config)

  required <- "default_time"
  optional <- c("title", "code", "docs", "parameters", "extra", "output",
                "default_replicates", "explore_tab", "time_scale")
  assert_has_fields(config, required, optional, basename(path_config))

  base <- tools::file_path_sans_ext(basename(path_config))
  wd <- normalizePath(dirname(path_config), mustWork = TRUE)

  f <- function(field) {
    sprintf("%s:%s", basename(path_config), field)
  }

  assert_scalar_numeric(config$default_time, f("default_time"))

  if (is.null(config$title)) {
    config$title <- base
  } else {
    assert_scalar_character(config$title, f("title"))
  }

  if (is.null(config$code)) {
    config$code <- sprintf("%s.R", base)
  }
  assert_file_exists(config$code, f("code"), wd = wd)
  ## TODO: this is not great with absolute paths!
  config$code <- file.path(wd, config$code)

  if (is.null(config$docs)) {
    config$docs <- sprintf("%s.md", base)
  }
  assert_file_exists(config$docs, f("docs"), wd = wd)
  ## TODO: this is not great with absolute paths!
  config$docs <- file.path(wd, config$docs)

  if (!is.null(config$extra)) {
    assert_named(config$extra, TRUE, f("extra"))
    config$extra <- lapply(config$extra, function(def)
      eval(parse(text = def), env))
  }

  if (is.null(config$default_replicates)) {
    config$default_replicates <- 1L
  } else {
    assert_scalar_numeric(config$default_replicates, f("default_replicates"))
  }

  if (is.null(config$explore_tab)) {
    config$explore_tab <- TRUE
  } else {
    assert_scalar_logical(config$explore_tab, f("explore_tab"))
  }

  if (!is.null(config$time_scale)) {
    assert_scalar_character(config$time_scale, f("time_scale"))
  }

  config
}
