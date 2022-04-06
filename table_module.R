library(shiny)
library(rhandsontable)
library(tibble)

# Creating a module for a table that you can add rows to

options(shiny.autoreload = TRUE)


handsOnTableUi <- function(id, buttonLabel) {
  ns <- NS(id)
  tagList(rHandsontableOutput(ns("table")),
          actionButton(ns("button"), buttonLabel, icon = icon("plus")))
}

handsOnTableServer <- function(id, dat) {
  moduleServer(id,
               function(input, output, session) {
                 values <- reactiveValues(data = dat)
                 observeEvent(input$button, {
                   values$data <- add_row(values$data)
                 })
                 # A second observe event triggers when values are changed
                 # and make the changes to the underlying data
                 observeEvent(input$table$changes$changes, {
                   # The + 1 is to account for the JS indexes that are used
                   row <- input$table$changes$changes[[1]][[1]] + 1
                   col <- input$table$changes$changes[[1]][[2]] + 1
                   val <- input$table$changes$changes[[1]][[4]]
                   values$data[row, col] <- val
                   }, ignoreInit = T)
                 
                 output$table <-
                   renderRHandsontable({
                     rhandsontable(values$data,
                                   rowHeaders = F,
                                   overflow = 'visible')
                   })
                 values
               })
}


# Test the module with the mtcars data
ui <- fluidPage(titlePanel("table module"),
                
                handsOnTableUi("table_test", buttonLabel = "Add Row!"))

server <- function(input, output, session) {
  handsOnTableServer(id = "table_test", dat = mtcars[1:10, 1:10])
  
}

shinyApp(ui, server)