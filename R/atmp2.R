library(tidyverse)
library(shiny)
library(DT)

dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)

dt_output = function(title, id) {
  fluidRow(column(
    12, h4( title),
    hr(), DT::dataTableOutput(id)
  ))
}

shinyApp(
  ui = fluidPage(
    fluidRow(
      fileInput("upload", "Choose Model input File", accept = c(".xlsx")),
      downloadButton("download")
    ),
    fluidRow(
      dt_output('Treatments', 'tro'),
      dt_output('Contracts', 'cono'),
      dt_output('Globals', 'glo'),
      tableOutput("summary_analysis"), 
      
      verbatimTextOutput("print_tr"),
      verbatimTextOutput("print_con"),
      verbatimTextOutput("print_gl")
    )
  ),
  server = function(input, output, session) {
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL)
    
    observe({
      req(input$upload)
      
      tryCatch(
        {
          indata = open_indata(input$upload$datapath)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      # Reactive values updated from treatment_table
      # vals$treatment_table <- read_excel(input$upload$datapath, sheet = "Treatments", col_types = 'text') 
      # vals$contract_table <- read_excel(input$upload$datapath, sheet = "Contracts", col_types = 'text') 
      
      # DT::coerceValue wants a data.frame
      vals$treatment_table <- as.data.frame( read_excel(input$upload$datapath, sheet = "Treatments"))
      vals$contract_table <- as.data.frame(read_excel(input$upload$datapath, sheet = "Contracts"))
      vals$global_table <- as.data.frame(read_excel(input$upload$datapath, sheet = "Globals"))
      
      # vals$treatment_table <- indata$treatment_table
      # vals$contract_table <- indata$contract_table
    })
    
    output$print_tr <- renderPrint({
      vals$treatment_table
    })
    output$print_con <- renderPrint({
      vals$contract_table
    })
    output$print_gl <- renderPrint({
      vals$global_table
    })
    
    output$summary_analysis <- renderTable({
      rubriker = c(Treatment = "name", Payment = "contract")
      contract_analysis(vals)
    })
    
    # RENDER AND UPDATE DATA
    
    # Treatments
    output$tro = DT::renderDataTable(vals$treatment_table, selection = 'none', options = dtoptions, rownames = FALSE, edit = TRUE)
    
    proxy_tr = dataTableProxy('tro')
    
    observeEvent(input$tro_cell_edit, {
      info = input$tro_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      vals$treatment_table[i, j] <<- DT:::coerceValue(v, vals$treatment_table[i, j])
      replaceData(proxy_tr, vals$treatment_table, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Contracts
    output$cono = DT::renderDataTable(vals$contract_table, selection = 'none', options = dtoptions, rownames = FALSE, edit = TRUE)
    
    proxy_con = dataTableProxy('cono')
    
    observeEvent(input$cono_cell_edit, {
      info = input$cono_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      vals$contract_table[i, j] <<- DT:::coerceValue(v, vals$contract_table[i, j])
      replaceData(proxy_con, vals$contract_table, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Globals
    output$glo = DT::renderDataTable(vals$global_table, selection = 'none', options = dtoptions, rownames = FALSE, edit = TRUE)
    
    proxy_gl = dataTableProxy('glo')
    
    observeEvent(input$glo_cell_edit, {
      info = input$glo_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      vals$global_table[i, j] <<- DT:::coerceValue(v, vals$global_table[i, j])
      replaceData(proxy_gl, vals$global_table, resetPaging = FALSE, rownames = FALSE)
    })
    
    # DOWNLOAD
        
    output$download <- downloadHandler("example.csv", 
                                       content = function(file){
                                         write.csv(vals$treatment_table, file, row.names = F)
                                       },
                                       contentType = "text/csv")
    
  }
)