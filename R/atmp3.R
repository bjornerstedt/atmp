library(tidyverse)
library(readxl)
library(shiny)
library(DT)

dtoptions = list(paging = FALSE, ordering = FALSE, searching = FALSE, info =FALSE)

dt_output = function(title, id) {
  fluidRow(column(
    12, h4( title), DT::dataTableOutput(id),
    hr()
  ))
}

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
  # vals$treatment_table <- as.data.frame( read_excel(filename, sheet = "Treatments", col_types = 'text'))
  # vals$contract_table <- as.data.frame(read_excel(filename, sheet = "Contracts", col_types = 'text'))
  # vals$global_table <- as.data.frame(read_excel(filename, sheet = "Globals", col_types = 'text'))
  
  # vals$treatment_table <- indata$treatment_table
  # vals$contract_table <- indata$contract_table
  indata
}

shinyApp(
  ui = fluidPage(
      dt_output('Treatments', 'tro'),
      # actionButton("analysis", "Analyse"), 
      hr(), h4( "Results"),
      tableOutput("summary_analysis"),
      verbatimTextOutput("print_tr")
  ),
  server = function(input, output, session) {
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL)
    
    indata = load_data(vals, 'examples.xlsx')
      
    vals$treatment_table <- as.data.frame( read_excel('examples.xlsx', sheet = "Treatments", col_types = 'text'))

    output$summary_analysis <- renderTable({
        # req(input$analysis)
      tt = suppressWarnings(vals$treatment_table %>% mutate(across(-c('plan','name'), ~DT:::coerceValue(.x, 1.0) )) %>% as_tibble())
      tna = tt %>% select(-plan) %>% pivot_longer(-name, names_to = 'column_name', values_to = 'value')
      tm = vals$treatment_table %>% 
        select(-plan) %>% 
        pivot_longer(-name, names_to = 'column_name', values_to = 'value') %>% 
        add_column(table_name = "treatment_table", .before = 1) %>% 
        rename(row_name = name)
      indata$treatment_table = tt
      indata$select_table = tm[is.na(tna$value),] %>% 
        separate_longer_delim(value, delim = " ") %>% 
        mutate(value = as.numeric(value))
      
      multipleInput = nrow(indata$select_table) > 0
      if (multipleInput) {
        compare_with_variations(indata, indata$select_table ) %>% 
          rename(Costdiff. = Cost, QALYdiff. = QALY)
          # %>% 
          # rename(firm_discount = value) %>% 
          # select(-Jämförelse)
      } else {
        contract_analysis(indata) %>% 
          rename(Costdiff. = Cost, QALYdiff. = QALY)
      }
    })
      
    output$print_tr <- renderPrint({
        indata
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
      
      if(str_detect(v,"^[0-9. ]+$")) {
        vals$treatment_table[i, j] <<- v
        replaceData(proxy_tr, vals$treatment_table, resetPaging = FALSE, rownames = FALSE)
      } else {
        showNotification("Only numerical values can be entered", type = "error")
      }
      
    })
    
    # Contracts
    output$cono = DT::renderDataTable(vals$contract_table, selection = 'none', 
                                      editable =list(target = 'cell', disable = list(columns = c(0,1))), options = dtoptions, rownames = FALSE)
    
    proxy_con = dataTableProxy('cono')
    
    observeEvent(input$cono_cell_edit, {
      info = input$cono_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      if(str_detect(v,"^[0-9. ]+$")) {
        vals$treatment_table[i, j] <<- v
        replaceData(proxy_tr, vals$treatment_table, resetPaging = FALSE, rownames = FALSE)
      } else {
        showNotification("Only numerical values can be entered", type = "error")
      }
    })
    
    
  }    
)