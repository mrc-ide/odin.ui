mod_variable_order_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_variable_order_server <- function(input, output, session, variables) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$variables <- variables()
  })

  output$ui <- shiny::renderUI({
    variable_order_ui(rv$variables, session$ns, prev = rv$result)
  })

  shiny::observe({
    if (!is.null(rv$variables)) {
      result <- list(show = input$show_order,
                     hide = input$hide_order,
                     disable = input$disable_order)
      if (sum(lengths(result)) == nrow(rv$variables)) {
        rv$result <- result
      }
    }
  })

  output$status <- shiny::renderUI({
    variable_order_status(rv$result)
  })

  shiny::outputOptions(output, "ui", suspendWhenHidden = FALSE)

  get_state <- function() {
    list(variables = rv$variables,
         result = list(
           show = input$show_order,
           hide = input$hide_order,
           disable = input$disable_order))
  }

  set_state <- function(state) {
    rv$variables <- state$variables
    ## TODO: compat
    if ("show" %in% names(state)) {
      state$result <- state[c("show", "hide", "disable")]
    }
    rv$result <- state$result
  }

  reset <- function() {
    output$ui <- shiny::renderUI(
      variable_order_ui(rv$variables, session$ns))
  }

  list(result = shiny::reactive(rv$result),
       reset = reset,
       get_state = get_state,
       set_state = set_state)
}


variable_order_ui <- function(variables, ns, prev = NULL) {
  if (is.null(variables)) {
    return(NULL)
  }

  show <- include <- vars <- variables$name

  if (!is.null(prev)) {
    hide <- intersect(prev$hide, vars)
    disable <- intersect(prev$disable, vars)
    show <- union(intersect(prev$show, vars),
                  setdiff(vars, c(hide, disable)))
  } else {
    show <- vars
    hide <- character()
    disable <- character()
  }

  simple_panel(
    "info",
    "Outputs to include in graphs",
    icon_name = "gear",
    shiny::tagList(
      shiny::includeMarkdown(odin_ui_file("md/editor-info.md")),
      shinyjqui::orderInput(ns("show"), "Reorder variables", show,
                            connect = ns(c("hide", "disable")),
                            class = "editor_include_show"),
      shinyjqui::orderInput(ns("hide"), "Hide variables", hide,
                            connect = ns(c("show", "disable")),
                            placeholder = "Drag items here to hide",
                            class = "editor_include_hide"),
      shinyjqui::orderInput(ns("disable"), "Disable variables", disable,
                            connect = ns(c("hide", "show")),
                            placeholder = "Drag items here to disable",
                            class = "editor_include_disable"),
      shiny::uiOutput(ns("status"))))
}


variable_order_status <- function(result) {
  if (!is.null(result) && length(result$show) == 0L) {
    simple_panel("warning", "No variables are shown", NULL)
  }
}
