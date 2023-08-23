# Checkbox interface from 
# https://community.rstudio.com/t/shiny-app-matrix-checkbox/108251/3

library(tidyverse)
library(shiny)
library(DT)

ui <- fluidPage(
  dataTableOutput("myTable"),
  verbatimTextOutput("print_x"),
  verbatimTextOutput("print_info")
  
)

CHECK = as.character(icon("ok", lib = "glyphicon"))
server <- function(input, output, session) {
  
  #The proxy to update the DT
  proxy <- dataTableProxy('myTable')
  
  #The initial data for the checkboxes
  checkboxes = data.frame(
    Name = paste("Project", 1:2),
    Greek = c(NA, CHECK),
    Time = c(NA, CHECK),
    Budget = c(CHECK, NA)
  )
  
  #The reactive version of the data
  tableData = reactiveValues(checkboxes = checkboxes)
  
  info = NULL 
  #Update the table when clicked
  observeEvent(req(input$myTable_cells_selected), {
    info <<- tableData$checkboxes[input$myTable_cells_selected]
    
    # if (is.null(info) || is.na(info)) return()
    
    tableData$checkboxes[input$myTable_cells_selected] =
      ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
             CHECK, NA)
    
    #Send proxy (no need to refresh whole table)
    replaceData(proxy, tableData$checkboxes)
    
  })
  
  #The "checkbox" table
  output$myTable = renderDataTable({
    checkboxes
  } , 
  # rownames = FALSE,
  #These are options to make the table look like checkboxes
  selection = list(mode = "single", target = 'cell'), 
  # editable = list(target = 'cell', disable = list(columns = c(1))),
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = c(2))),
    dom = "t", ordering = FALSE
  ),
  escape = FALSE)
  
  output$print_x <- renderPrint({
    cb = (!is.na(tableData$checkboxes)) %>% 
      as.data.frame() %>% 
      rownames_to_column("property") %>% 
      pivot_longer(cols = -property, names_to = "variable", values_to = "value") %>% 
      filter(value) %>% 
      select(-value) %>% 
      mutate(v1 = NA_real_, v2 = NA_real_, v3 = NA_real_)
    cb
  })
  output$print_info <- renderPrint({
    info
  })
  
  
}

shinyApp(ui, server)