# 
library(shiny)
library(dplyr)
library(readxl)
library(purrr)


shinyApp(
  ui = fluidPage(
    title = 'Download a PDF report',
    sidebarLayout(
      sidebarPanel(
        helpText(),
        selectInput('x', 'Select form template:',
                    choices = c("Short", "Complete")),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        downloadButton('downloadReport')
      ),
      mainPanel(
        plotOutput('regPlot')
      )
    )
  ),
  server = function(input, output) {
    indata = open_indata("/Users/jonasbjornerstedt/GitHub/atmp/models.xlsx")
    
    regFormula <- reactive({
      as.formula(paste('mpg ~', input$x))
    })
    
    output$regPlot <- renderPlot({
      par(mar = c(4, 4, .1, .1))
      plot(regFormula(), data = mtcars, pch = 19)
    })
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
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
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
    
  }
)

## Rmarkdown in app ---------------------------------------

  ui = fluidPage(
    sliderInput("slider", "Slider", 1, 100, 50),
    downloadButton("report", "Generate report"),
    htmlOutput("report")
  )
  
  
  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.doc",
      content = function(file) {
        # req(input$slider)
        # For PDF output, change this to "report.pdf"
        filetype = "doc"
        filename = str_c("report.", filetype)
        outputformat = if_else(filetype == "doc", "word_document", "pdf_document")
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        location = "/Users/jonasbjornerstedt/GitHub/atmp/R"
        tempReport <- file.path(location, "report.Rmd")
        # file.copy("/Users/jonasbjornerstedt/GitHub/atmp/R/report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        # params <- list(n = input$slider)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file.path(location, filename),
                          output_format = outputformat, 
                          # params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)

## Combined app -----------------------------------  


excelinputApp <- function(...) {

ui <- fluidPage(
      tabsetPanel(
        id = "tabset",
        tabPanel("Input",
                 fileInput("upload", NULL, accept = c(".xlsx")),
                 DT::dataTableOutput('treatments'),
                 DT::dataTableOutput('contracts')
        ),
        tabPanel("Analysis", 
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
  data <- reactive({
    req(input$upload)
    open_indata(input$upload$datapath)
    # xlsx = readxl::read_xlsx(input$upload$datapath)
  })
  
  dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)
  output$treatments = DT::renderDataTable(data()$treatment_table, rownames = FALSE, selection = 'none', options = dtoptions, 
                                          editable = list(target = 'cell', disable = list(columns = c(1))))
  output$contracts = DT::renderDataTable(data()$contract_table, rownames = FALSE, selection = 'none', options = dtoptions, 
                                         editable = list(target = 'cell', disable = list(columns = c(1))))
  
  output$partial_analysis <- renderTable({
    rubriker = c(Treatment = "name", Payment = "contract")
    contract_analysis(data(), show_details = TRUE) %>% 
      select(-plan) %>% rename(any_of(rubriker)) 
  })
  output$summary_analysis <- renderTable({
    rubriker = c(Treatment = "name", Payment = "contract")
    contract_analysis(data())
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
}

excelinputApp()  
  
## Create download of report ---------------------------------------

reportApp <- function(...) {

ui = fluidPage(
    sliderInput("slider", "Slider", 1, 100, 50),
    downloadButton("report", "Generate report")
  )

  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("/Users/jonasbjornerstedt/GitHub/atmp/R/report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(n = input$slider)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }

# Run the application 
shinyApp(ui = ui, server = server)
}


## Dynamic UI --------------------------

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))

dynamicControlsApp <- function(...) {
  
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = dfs),
      uiOutput("filter")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  data <- reactive({
    get(input$dataset, "package:datasets")
  })
  vars <- reactive(names(data()))
  
  output$filter <- renderUI(
    map(vars(), ~ make_ui(data()[[.x]], .x))
  )
  
  selected <- reactive({
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })
  
  output$data <- renderTable(head(data()[selected(), ], 12))
}

shinyApp(ui = ui, server = server)
}

## Multi page output ---------------------------------------

multiPageApp <- function(...) {
  
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("panel")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Input", "one"),
        tabPanel("Analysis", "two"),
        tabPanel("Report", "three")
      )
    )
  )
)

server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
}
shinyApp(ui = ui, server = server)
}
