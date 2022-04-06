library(shiny)
library(DT)

options(shiny.autoreload = TRUE)


dtUi <- function(id, buttonLable) {
  ns <- NS(id)
  tagList(DTOutput(ns("table")),
          actionButton(ns("button"), buttonLable, icon = icon("plus")))
}

dtServer <- function(id, dat, rownames = F) {
  moduleServer(id, 
               function(input, output, session) {
                 output$table <- renderDT(dat, 
                                          editable = TRUE,
                                          server = TRUE,
                                          rownames = rownames)
                 
                 observeEvent(input$table_cell_edit, {
                   dat <<- editData(data = dat,
                                    info = input$table_cell_edit,
                                    proxy = "table", 
                                    rownames = rownames,
                                    resetPaging = FALSE)
                 }, ignoreInit = TRUE)
                 
                 observeEvent(input$button, {
                   replaceData(dataTableProxy("table"),
                               tibble::add_row(dat),
                               resetPaging = FALSE, 
                               rownames = rownames) # must explicitly pass this argument
                 })
               })
}


# Example of module running 
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      
      # Plain code tab
      tabPanel("non-module", "Non Modular Code",
                   titlePanel("test"),
                   DTOutput("table"),
                   actionButton("button", "Add Row", icon = icon("plus"))
                     ),
      # Module tab
      tabPanel("module", "Modular Code",
               dtUi("table_test", "Add Row!!")
               )
    )
  )
)


server <- function(input, output, session) {
  # Set data for both tables
  df <- iris[1:10,]
  
  # Non-mod code
  output$table <- renderDT(df, editable = TRUE, server = TRUE, rownames = TRUE)
  
  observeEvent(input$table_cell_edit, {
    df <<-
      editData(
        data = df,
        info = input$table_cell_edit,
        proxy = "table",
        rownames = T,
        resetPaging = F
      )
  }, ignoreInit = TRUE)
  
  observeEvent(input$button, {
    replaceData(dataTableProxy("table"), tibble::add_row(df), resetPaging = FALSE)
  })
  
  # Mod
  dtServer("table_test", df)
  
}

shinyApp(ui, server)