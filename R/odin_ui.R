##' Odin shiny app
##' @title Odin Shiny Application
##' @export
odin_app <- function() {
  shiny::shinyApp(
    ui = odin_ui(),
    server = odin_server)
}


odin_server <- function(input, output) {
  output$result_plot <- shiny::renderPlot({
    x <- stats::rnorm(input$n)
    y <- stats::rnorm(input$n)
    graphics::plot(x, y, asp = 1)
  })
}


odin_ui <- function() {
  shiny::shinyUI(shiny::fluidPage(
    shiny::titlePanel("odin ui"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("n",
                           "Number of points:",
                           min = 1,
                           max = 1000,
                           value = 500)
      ),

      shiny::mainPanel(
        shiny::plotOutput("result_plot")
      )
    )
  ))
}
