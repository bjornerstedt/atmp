# Checkbox interface from 
# https://community.rstudio.com/t/shiny-app-matrix-checkbox/108251/3

library(tidyverse)
library(shiny)
library(DT)

ui <- fluidPage(
  dataTableOutput("myTable"),
  verbatimTextOutput("print_x")
  
)

CHECK = as.character(icon("ok", lib = "glyphicon"))
server <- function(input, output, session) {
  
  #The proxy to update the DT
  proxy <- dataTableProxy('myTable')
  
  #The initial data for the checkboxes
  checkboxes = data.frame(
    Name = c(NA, CHECK),
    Time = c(NA, CHECK),
    Budget = c(CHECK, NA),
    row.names = paste("Project", 1:2)
  )
  
  #The reactive version of the data
  tableData = reactiveValues(checkboxes = checkboxes)
  
  #Update the table when clicked
  observeEvent(req(input$myTable_cells_selected), {
    tableData$checkboxes[input$myTable_cells_selected] =
      ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
             CHECK, NA)
    
    #Send proxy (no need to refresh whole table)
    replaceData(proxy, tableData$checkboxes)
    
  })
  
  #The "checkbox" table
  output$myTable = renderDataTable({
    checkboxes
  }, 
  #These are options to make the table look like checkboxes
  selection = list(mode = "single", target = 'cell'), 
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    dom = "t", ordering = FALSE
  ),
  escape = FALSE)
  
  output$print_x <- renderPrint({
    cb = !is.na(tableData$checkboxes) %>% 
      as.data.frame() %>% 
      rownames_to_column("property") %>% 
      pivot_longer(cols = -property, names_to = "variable", values_to = "value") %>% 
      filter(value) %>% 
      select(-value) %>% 
      mutate(v1 = NA_real_, v2 = NA_real_, v3 = NA_real_)
    cb
  })
  
  
}

shinyApp(ui, server)