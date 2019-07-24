mod_variable_order_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_variable_order_server <- function(input, output, session, variables) {
  rv <- shiny::reactiveValues()

  output$ui <- shiny::renderUI({
    variable_order_ui(variables(), session$ns, prev = get_state())
  })

  shiny::observe({
    if (!is.null(variables())) {
      rv$result <- get_state()
    }
  })

  output$status <- shiny::renderUI({
    variable_order_status(rv$result)
  })

  shiny::outputOptions(output, "ui", suspendWhenHidden = FALSE)

  get_state <- function() {
    list(show = input$show_order,
         hide = input$hide_order,
         disable = input$disable_order)
  }

  set_state <- function(state) {
    ## TODO: this is broken - we should be able to pass in the state
    ## here, but that means that all subsequent updates end up using
    ## the same state.  At the same time, I am not seeing the 'prev'
    ## come through *ever* below (see commented out debug).
    ui <- shiny::isolate(
      variable_order_ui(variables(), session$ns, restore = state))
    output$ui <- shiny::renderUI(ui)
  }

  reset <- function() {
    output$ui <- shiny::renderUI(
      variable_order_ui(variables(), session$ns))
  }

  list(result = shiny::reactive(rv$result),
       reset = reset,
       get_state = get_state,
       set_state = set_state)
}


variable_order_ui <- function(variables, ns, prev = NULL, restore = NULL) {
  if (is.null(variables)) {
    return(NULL)
  }

  ## message(sprintf("Drawing order ui prev = %s, restore = %s",
  ##                 !is.null(prev), !is.null(restore)))
  show <- include <- vars <- variables$name

  if (!is.null(restore)) {
    show <- restore$show
    hide <- restore$hide
    disable <- restore$disable
  } else if (!is.null(prev)) {
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
