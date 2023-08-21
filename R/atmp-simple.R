# 
library(shiny)
library(dplyr)
library(readxl)
library(purrr)

ui <- fluidPage(

                 fileInput("upload", NULL, accept = c(".xlsx")),
                 DT::dataTableOutput('treatments'),
                 DT::dataTableOutput('contracts'),
                 tableOutput("summary_analysis")
                

)


server <- function(input, output, session) {
  indata = NULL
  treatment_table = NULL
  data <- reactive({
    req(input$upload)
    
    # if (is.null(input$upload)) return(NULL)
    dt = open_indata(input$upload$datapath)
    indata <<- dt
    treatment_table <<- dt$treatment_table
    # xlsx = readxl::read_xlsx(input$upload$datapath)
    treatment_table
  })
  
  
  dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)

  output$treatments = 
    DT::renderDataTable(
      treatment_table, rownames = FALSE, selection = 'none', options = dtoptions, 
                                          editable = list(target = 'cell', disable = list(columns = c(1))))
  output$contracts = DT::renderDataTable(indata$contract_table, rownames = FALSE, selection = 'none', options = dtoptions, 
                                         editable = list(target = 'cell', disable = list(columns = c(1))))
  output$summary_analysis = renderTable({
    treatment_table
  })
  
  observeEvent(input$treatment_cell_edit, {
    treatment_table <<- editData(indata$treatment_table, input$treatment_cell_edit, 'treatment')
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
