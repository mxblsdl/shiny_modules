library(shiny)
library(DT)
library(shinythemes)

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
ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "superhero"),
  tags$head(
   
    tags$style("
    table.dataTable tbody th, table.dataTable tbody td {
    padding: 1px 1px !important;
    text-align: right;
    }
    .table th, .table td {
    padding: .25rem;
    text-align: right;
    }
               ")
  ),
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

options(DT.options = list(ordering = F, pageLength = 10, dom = "t"))

server <- function(input, output, session) {
  # Set data for both tables
  df <- iris[1:10,]
  
  # Non-mod code
  # Note that additional parameters must be passed through ... of renderDT instead of 
  # through datatable()
  output$table <- renderDT(df,
                           editable = TRUE,
                           server = TRUE,
                           rownames = TRUE,
                           class = "my_tab",
                           caption = "Tester Tester"
                           )
  
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