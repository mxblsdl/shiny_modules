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
                
                handsOnTableUi("table1", buttonLabel = "Add Row!"))

server <- function(input, output, session) {
  handsOnTableServer(id = "table1", dat = mtcars[1:10, 1:10])
  
}

shinyApp(ui, server)