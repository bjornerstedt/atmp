library(tidyverse)
library(shiny)
library(DT)
library(writexl)
library(readxl)
library(rmarkdown)

source("atmp.R")
source("atmp_new.R")

dt_output = function(title, id) {
  fluidRow(column(
    12, h4( title),
    DT::dataTableOutput(id)
  ))
}

example_list = c("Example A", "Example B", "Example A2")

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$style(HTML(
        ".tabbable ul li:nth-child(6) { float: right; }"
      ))
    ),
    tabsetPanel(
      id = "tabset",
      tabPanel("Overview", includeMarkdown("overview.md")
      ),
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
        h4('Download modified data'), 
        p('Download modified model file to your computer as Excel file.'),
        downloadButton("download")
      ),
      
      tabPanel("Analysis", 
         div( tableOutput("errors"),
             style = "color: #FF0000; font-size: 120%"),
         div(tableOutput("input_errors"),
             style = "color: #FF0000; font-size: 120%"),
         h3('Analysis of model'),
         p('Payments and QALY for each treatment'),
         tableOutput("partial_analysis") ,
         h4('Summary'), 
         tableOutput("summary_analysis"),
         hr(),
         h4('Plots over time'),
         column(6, plotOutput('costs')),
         column(6, plotOutput('QALY')),
      ),
      
      tabPanel("Report",
        h3('Generate report'),
        helpText("Save a report in the selected ouput format"),
        # selectInput('x', 'Select form template:',
        #            choices = c("Short", "Complete")),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                    inline = TRUE),
        downloadButton('downloadReport')
      ),
      
      tabPanel("Help", includeMarkdown("ATMP-package.md")
      )    
    )
  ),
  
  server = function(input, output, session) {
    theme_set(theme_bw()) 
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL, 
                           state_table = NULL, payment_table = NULL, 
                           treatment_description = NULL, contract_description = NULL, errors = NULL, 
                           filename = NULL)
    
    indata <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL, 
                           state_table = NULL, payment_table = NULL, 
                           treatment_description = NULL, contract_description = NULL, errors = NULL)

    
    observeEvent(input$upload, {
      indata <- load_data(vals, indata, input$upload$datapath)
      vals$filename <- input$upload$name
    })

    observeEvent(input$loadexample, {
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
    
    output$partial_analysis <- renderTable({
      req(vals$filename )
      analyse_treatments(get_user_input(), show_details = TRUE)
    }, na = "")
    
    get_user_input = reactive(vals)
    
    output$summary_analysis <- renderTable({
      req(vals$filename )
      analyse_treatments(get_user_input())
    })
    
    output$errors <- renderTable({
      req(indata$errors )
      if (nrow(indata$errors)) {
        indata$errors
      }
    })

    output$input_errors <- renderTable({
      vals$errors <- check_indata(vals)
      req(vals$errors )
      if (nrow(vals$errors)) {
        vals$errors
      }
    })

    output$QoL <- renderPlot({
      req(vals$filename )
      plot_QoL2(vals)
    })
    
    output$payment_plans <- renderPlot({
      req(vals$filename )
      plot_payment_plans2(vals)
    })
    
    output$costs <- renderPlot({
      req(vals$filename )
      plot_payments2(vals)
    })
    
    output$QALY <- renderPlot({
      req(vals$filename )
      plot_QALY2(indata)
    })
    
    
    # RENDER AND UPDATE DATA
    
    dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)
    
    # Treatments
    output$tro =  
      DT::renderDataTable( {
        req(vals$filename )
        vals$state_table
        }, selection = 'none', colnames=get_titles(vals$state_table, indata$state_description), 
                    editable =list(target = 'cell', disable = list(columns = c(0, ncol(vals$state_table)-1))),  # Last column is key var to payments
          options = dtoptions, rownames = FALSE)

    proxy_tr = dataTableProxy('tro')
    
    observeEvent(input$tro_cell_edit, {
      info = input$tro_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      vals$state_table[i, j] <<- DT:::coerceValue(v, vals$state_table[i, j])
      replaceData(proxy_tr, vals$state_table, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Payments
    output$cono = 
      DT::renderDataTable({
        req(vals$filename )
        vals$payment_table
        } , selection = 'none', colnames=get_titles(vals$payment_table, indata$payment_description), 
                    editable =list(target = 'cell', disable = list(columns = c(0))), options = dtoptions, rownames = FALSE)
    
    proxy_con = dataTableProxy('cono')
    
    observeEvent(input$cono_cell_edit, {
      info = input$cono_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      vals$payment_table[i, j] <<- DT:::coerceValue(v, vals$payment_table[i, j])
      replaceData(proxy_con, vals$payment_table, resetPaging = FALSE, rownames = FALSE)
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
    
    get_indata = reactive({
      indata$state_table = vals$state_table
      indata$payment_table = vals$payment_table
      indata$global_table = vals$global_table
      indata
    })    
        
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        # location = "/Users/jonasbjornerstedt/GitHub/atmp/R"
        # src <- file.path(location, "report.Rmd")
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        indata = get_indata()
        out <- render('report.Rmd', 
                      params = list(model = vals$filename),
                      envir = environment(), 
                      output_format = switch(
                        input$format,
                        PDF = pdf_document(), HTML = html_document(), Word = word_document()
                      ))
        file.rename(out, file)
      }
    )    
  }
)

