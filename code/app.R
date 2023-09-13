library(tidyverse)
library(shiny)
library(DT)
library(writexl)
library(readxl)

source("atmp.R")

dt_output = function(title, id) {
  fluidRow(column(
    12, h4( title),
    DT::dataTableOutput(id), hr()
  ))
}

example_list = c("Example A", "Example B", "Example A2")

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
        p('Select example dataset or upload Excel sheet.'),
        column(6,
        selectInput("example", "Select example model", example_list) ,
        actionButton("loadexample", "Load")
        ),
        column(6,
        fileInput("upload", "Choose Model input File", accept = c(".xlsx")),
        ),
        column(12,
          hr(),
          h4(textOutput("data_loaded", inline = TRUE))
        ),
        column(6,plotOutput('QoL')),
        column(6,plotOutput('payment_plans')),
      ),
      
      tabPanel("Input", 
           h3('Model Input'),
           p('Modify values in tables'),
           dt_output('Treatments', 'tro'),
          dt_output('Payment plans', 'cono'),
          dt_output('Globals', 'glo'),
          hr(), 
          h4('Download modified data'), 
          p('Download modified model file to your computer as Excel file.'),
          downloadButton("download")
      ),
      
      tabPanel("Analysis", 
           h3('Analysis of model'),
         tableOutput("partial_analysis") ,
         tableOutput("summary_analysis"),
         hr(),
         h4('Plots over time'),
         column(6, plotOutput('costs')),
         column(6, plotOutput('QALY')),
         
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
    theme_set(theme_bw()) 
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL, filename = NULL)
    indata <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL, treatment_description = NULL, contract_description = NULL)

    
    observe({
      req(input$upload)
      indata <- load_data(vals, indata, input$upload$datapath)
      vals$filename <- input$upload$name
    })

    observe({
      req(input$loadexample)
      filenames = list(
        "Example A" = "Example_A.xlsx",
        "Example A2" = "Example_A2.xlsx",
        "Example B" = "Example_B.xlsx"
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
      rubriker = c(Treatment = "name", Arm = "plan", Payment = "contract")
      contract_analysis(vals, show_details = TRUE) %>%
        # select(-plan) %>% 
        rename(any_of(rubriker))
    })
    output$summary_analysis <- renderTable({
      req(vals$filename )
      contract_analysis(vals)
    })
  
    output$QoL <- renderPlot({
      req(vals$filename )
      plot_QoL(vals)
    })
    
    output$payment_plans <- renderPlot({
      req(vals$filename )
      plot_payment_plans(vals)
    })
    
    output$costs <- renderPlot({
      req(vals$filename )
      plot_payments(vals)
    })
    
    output$QALY <- renderPlot({
      req(vals$filename )
      plot_QALY(vals)
    })
    
    
    # RENDER AND UPDATE DATA
    
    dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)
    
    # Treatments
    output$tro =  
      # with_titles( indata$treatment_description) %>%
      DT::renderDataTable( vals$treatment_table, selection = 'none', colnames=get_titles(vals$treatment_table, indata$treatment_description), 
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
    output$cono = 
      # vals$contract_table %>% 
      # with_titles( indata$contract_description) %>%
      DT::renderDataTable(vals$contract_table , selection = 'none', 
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
                                     # rownames=left_join(vals$global_table, indata$global_description) %>% pull(title),
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
        
    output$download <- downloadHandler(vals$filename, 
                                       content = function(file) {
                                         indata$treatment_table = vals$treatment_table 
                                         indata$contract_table = vals$contract_table 
                                         indata$global_table = vals$global_table 
                                         write_xlsx(indata, file)
                                       },
                                       contentType="application/xlsx" 
                                       )
    
  }
)

