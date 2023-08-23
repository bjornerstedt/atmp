library(tidyverse)
library(shiny)
library(DT)


dt_output = function(title, id) {
  fluidRow(column(
    12, h4( title),
    DT::dataTableOutput(id), hr()
  ))
}

example_list = c("Example A", "Example B", "Example C")

load_data <- function(vals, filename) {
  tryCatch(
    {
      indata = open_indata(filename)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  # Reactive values updated from treatment_table
  # vals$treatment_table <- read_excel(filename, sheet = "Treatments", col_types = 'text') 
  # vals$contract_table <- read_excel(filename, sheet = "Contracts", col_types = 'text') 
  
  # DT::coerceValue wants a data.frame
  vals$treatment_table <- as.data.frame( read_excel(filename, sheet = "Treatments"))
  vals$contract_table <- as.data.frame(read_excel(filename, sheet = "Contracts"))
  vals$global_table <- as.data.frame(read_excel(filename, sheet = "Globals"))
  
  # vals$treatment_table <- indata$treatment_table
  # vals$contract_table <- indata$contract_table
  vals
}

shinyApp(
  ui = fluidPage(
    tabsetPanel(
      id = "tabset",
      tabPanel("Input",
        column(6,
        selectInput("example", "Select example", example_list) ,
        actionButton("loadexample", "Load")
        ),
        column(6,
          fileInput("upload", "Choose Model input File", accept = c(".xlsx")),
        ),
          dt_output('Treatments', 'tro'),
          dt_output('Contracts', 'cono'),
          dt_output('Globals', 'glo'),
        downloadButton("download")
      ),
      
      tabPanel("Analysis", 
         # actionButton("analysis", "Analyse"), 
         tableOutput("partial_analysis") ,
         tableOutput("summary_analysis"),
         plotOutput('costs'),
         
        verbatimTextOutput("print_tr"),
        verbatimTextOutput("print_con"),
        verbatimTextOutput("print_gl")
      ),      
    )
  ),
  server = function(input, output, session) {
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL)
    
    observe({
      req(input$upload)
      load_data(vals, input$upload$datapath)
    })

    observe({
      req(input$loadexample)
      filenames = list(
        "Example A" = "examples.xlsx",
        "Example B" = "models.xlsx"
      )
      load_data(vals, filenames[[input$example]])
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
    
    output$partial_analysis <- renderTable({
      # req(input$analysis)
      rubriker = c(Treatment = "name", Payment = "contract")
      contract_analysis(vals, show_details = TRUE) %>% 
        select(-plan) %>% rename(any_of(rubriker)) 
    })
    output$summary_analysis <- renderTable({
      rubriker = c(Treatment = "name", Payment = "contract")
      contract_analysis(vals)
    })
  
    output$costs <- renderPlot({
      contract_analysis(vals, over_time = TRUE)  %>% 
        rename(Contract = contract) %>% 
        ggplot() + 
        aes(time, Cost, fill = Contract) + 
        geom_col()  + facet_grid(rows = vars(name)) +
        labs(title = "Costs over time", x = "")
    })
    
    
    # RENDER AND UPDATE DATA
    
    dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)
    
    # Treatments
    output$tro = DT::renderDataTable(vals$treatment_table, selection = 'none', editable =list(target = 'cell', disable = list(columns = c(0,1))), options = dtoptions, rownames = FALSE)

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
    output$cono = DT::renderDataTable(vals$contract_table, selection = 'none', editable =list(target = 'cell', disable = list(columns = c(0,1))), options = dtoptions, rownames = FALSE)
    
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
    output$glo = DT::renderDataTable(vals$global_table, selection = 'none', editable =list(target = 'cell', disable = list(columns = c(0))), options = dtoptions, rownames = FALSE)
    
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