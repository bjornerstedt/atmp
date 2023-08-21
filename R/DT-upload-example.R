# https://stackoverflow.com/questions/54426548/how-to-edit-a-table-using-dt-and-shiny-from-an-uploaded-file

library(tidyverse)
library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    fluidRow(
      fileInput("upload", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      downloadButton("download")
    ),
    fluidRow(
      DT::dataTableOutput('tro'),
      verbatimTextOutput("print")
    )
  ),
  server = function(input, output, session) {
    
    # In this edited example tr is now a reactive expression, dependent on input$upload
    
    # Key to the solution is the use of reactiveValues, stored as vals
    vals <- reactiveValues(tr = NULL, con = NULL, gl = NULL)
    
    observe({
      
      
      # input$upload will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$upload)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          tr <- read.csv(input$upload$datapath,
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = TRUE,
                        row.names = NULL)
          
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      # Reactive values updated from tr
      vals$tr <- tr
    })
    
    output$print <- renderPrint({
      vals$tr
    })
    output$tro = DT::renderDataTable(vals$tr, selection = 'none', rownames = FALSE, edit = TRUE)
    
    proxy = dataTableProxy('tro')
    
    observeEvent(input$tro_cell_edit, {
      info = input$tro_cell_edit
      str(info)
      i = info$row
      j = info$col + 1
      v = info$value
      # Below is the crucial spot where the reactive value is used where a reactive expression cannot be used
      vals$tr[i, j] <<- DT:::coerceValue(v, vals$tr[i, j])
      replaceData(proxy, vals$tr, resetPaging = FALSE, rownames = FALSE)
    })
    
    output$download <- downloadHandler("example.csv", 
                                       content = function(file){
                                         write.csv(vals$tr, file, row.names = F)
                                       },
                                       contentType = "text/csv")
    
  }
)