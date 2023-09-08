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

load_data <- function(vals, indata, filename) {
  tryCatch(
    {
      # indata = open_indata(filename)
      indata$treatment_table = read_excel(filename, sheet = "Treatments") 
      indata$contract_table = read_excel(filename, sheet = "Contracts") 
      indata$global_table = read_excel(filename, sheet = "Globals") 
      indata$treatment_description = read_excel(filename, sheet = "Treatment_fields") 
      indata$contract_description = read_excel(filename, sheet = "Contract_fields")
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
  indata
}

shinyApp(
  ui = fluidPage(
    tabsetPanel(
      id = "tabset",
      tabPanel("Overview",
               includeMarkdown("overview.md")
      ),
      # tabPanel("Overview",
      #   h3("ATMP payment models"),
      #   p("Here is an intro"),
      #   a(href="http://google.com", "Click Here!")
      # ),
      tabPanel("Model",
               h3('Model'),
               h4(textOutput("data_loaded", inline = TRUE)),
               selectInput("example", "Select example model", example_list) ,
               actionButton("loadexample", "Load"),
               hr(), 
               h4('Upload model file'), 
               
               fileInput("upload", "Choose Model input File", accept = c(".xlsx"))
      ),
      
      tabPanel("Input", 
           h3('Model Input'),
           dt_output('Treatments', 'tro'),
          dt_output('Contracts', 'cono'),
          dt_output('Globals', 'glo'),
          hr(), 
          h4('Download modified model file'), 
          downloadButton("download")
      ),
      
      tabPanel("Analysis", 
           h3('Analysis of model'),
         tableOutput("partial_analysis") ,
         tableOutput("summary_analysis"),
         plotOutput('costs'),
         
        # verbatimTextOutput("print_tr"),
        # verbatimTextOutput("print_con"),
        # verbatimTextOutput("print_gl")
      ),
      tabPanel("Report", 
               h3('Generate report'),
               helpText(),
               selectInput('x', 'Select form template:',
                           choices = c("Short", "Complete")),
               radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                            inline = TRUE),
               downloadButton('downloadReport')
      )    
    )
  ),
  server = function(input, output, session) {
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL, filename = NULL)
    indata <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL, treatment_description = NULL, contract_description = NULL)
    # indata <- NULL
    
    
    observe({
      req(input$upload)
      indata <- load_data(vals, indata, input$upload$datapath)
      vals$filename <- input$upload$name
    })

    observe({
      req(input$loadexample)
      filenames = list(
        "Example A" = "examples.xlsx",
        "Example B" = "models.xlsx"
      )
      indata <- load_data(vals, indata, filenames[[input$example]])
      vals$filename <- filenames[[input$example]]
    })
    
    output$data_loaded <- renderText({
      if_else(is.null(vals$filename), 
      'No model loaded',
      str_c('Model: ', vals$filename)
      )
    })
    
    output$print_tr <- renderPrint({
      vals$treatment_table %>% 
        with_titles( indata$treatment_description)
    })
    output$print_con <- renderPrint({
      vals$contract_table
    })
    output$print_gl <- renderPrint({
      vals$global_table
    })
    
    output$partial_analysis <- renderTable({
      req(vals$filename )
      rubriker = c(Treatment = "name", Payment = "contract")
      contract_analysis(vals, show_details = TRUE) %>% 
        select(-plan) %>% rename(any_of(rubriker)) 
    })
    output$summary_analysis <- renderTable({
      req(vals$filename )
      rubriker = c(Treatment = "name", Payment = "contract")
      contract_analysis(vals)
    })
  
    output$costs <- renderPlot({
      req(vals$filename )
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
    output$tro = vals$treatment_table  %>% 
      with_titles( indata$treatment_description) %>%
      DT::renderDataTable( selection = 'none', 
                    editable =list(target = 'cell', disable = list(columns = c(0,1))), options = dtoptions, rownames = FALSE)

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
    output$cono = vals$contract_table %>% 
      with_titles( indata$contract_description) %>%
      DT::renderDataTable(selection = 'none', 
                    editable =list(target = 'cell', disable = list(columns = c(0,1))), options = dtoptions, rownames = FALSE)
    
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
    output$glo = DT::renderDataTable(vals$global_table, selection = 'none', 
                   editable =list(target = 'cell', disable = list(columns = c(0))), options = dtoptions, rownames = FALSE)
    
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