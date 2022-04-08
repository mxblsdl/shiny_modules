library(shiny)
library(reactable)

# There is no direct way to edit cells with reactable
# There is a method where you can register an onclick event
# in the table to increment values in the table
# not shown here

ui <- fluidPage(
  reactableOutput("table"),
  actionButton("button", "Add Row")
)

server <- function(input, output) {
  output$table <- renderReactable({
    reactable(iris, selection = "single")
  })
  
  observeEvent(input$button, {
    browser()
  }, ignoreInit = T)
}


shinyApp(ui, server)