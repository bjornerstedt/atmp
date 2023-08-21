# 
library(shiny)
library(dplyr)
library(readxl)
library(purrr)

ui <- fluidPage(
      tabsetPanel(
        id = "tabset",
        tabPanel("Input",
                 fileInput("upload", NULL, accept = c(".xlsx")),
                 DT::dataTableOutput('treatments'),
                 DT::dataTableOutput('contracts')
        ),
        tabPanel("Analysis", 
                 actionButton("analysis", "Analyse"), 
                 tableOutput("partial_analysis") ,
                 tableOutput("summary_analysis")
                 ),
        tabPanel("Report", 
                 helpText(),
                 selectInput('x', 'Select form template:',
                             choices = c("Short", "Complete")),
                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                              inline = TRUE),
                 downloadButton('downloadReport')
                 )
      )
)


server <- function(input, output, session) {
  indata = NULL
  data <- reactive({
    req(input$upload)
    
    # if (is.null(input$upload)) return(NULL)
    dt = open_indata(input$upload$datapath)
    if (is.null(indata)) indata <<- dt
    # xlsx = readxl::read_xlsx(input$upload$datapath)
    indata
  })
  
  dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)
  output$treatments = 
    DT::renderDataTable(
      data()$treatment_table, rownames = FALSE, selection = 'none', options = dtoptions, 
                                          editable = list(target = 'cell', disable = list(columns = c(1))))
  output$contracts = DT::renderDataTable(data()$contract_table, rownames = FALSE, selection = 'none', options = dtoptions, 
                                         editable = list(target = 'cell', disable = list(columns = c(1))))
  
  output$partial_analysis <- renderTable({
    req(input$analysis)
    rubriker = c(Treatment = "name", Payment = "contract")
    contract_analysis(indata, show_details = TRUE) %>% 
      select(-plan) %>% rename(any_of(rubriker)) 
  })
  output$summary_analysis <- renderTable({
    rubriker = c(Treatment = "name", Payment = "contract")
    contract_analysis(data())
  })
  observeEvent(input$treatment_cell_edit, {
    indata$treatment_table <<- editData(indata$treatment_table, input$treatment_cell_edit, 'treatment')
  })
  
  file_format = reactive({
    input$fileformat
  })
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      location = "/Users/jonasbjornerstedt/GitHub/atmp/R"
      src <- file.path(location, "report.Rmd")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', 
        params = list(n = 30),
        output_format = switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
