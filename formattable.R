library(shiny)
library(formattable)

# formattable only creates a static table with formatting
# There is no interactivity in the table

ui <- fluidPage(
  titlePanel("Formattable"),
  formattableOutput("table"),
  actionButton("button", "Add Row")
)

server <- function(input, output, session) {

  # Formatting is done through format objects
  form <- formatter("span", style = x ~ style(
    color = "green", 
    font.weight = "bold"))
  
  output$table <- renderFormattable({
    formattable(mtcars, list(
      disp = form,
      wt = form
    ))
    })
  
  # There is no way to interact with the created table
  observeEvent(input$button, {
    browser()
  }, ignoreInit = T)
}

shinyApp(ui, server)