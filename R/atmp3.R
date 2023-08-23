library(tidyverse)
library(readxl)
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
      dt_output('Treatments', 'tro'),
      actionButton("analysis", "Analyse"), 
      verbatimTextOutput("print_tr")
  ),
  server = function(input, output, session) {
    
    vals <- reactiveValues(treatment_table = NULL, contract_table = NULL, global_table = NULL)
    
      vals$treatment_table <- as.data.frame( read_excel('examples.xlsx', sheet = "Treatments", col_types = 'text'))

    output$print_tr <- renderPrint({
      req(input$analysis)
      tt = vals$treatment_table %>% mutate(across(-c('plan','name'), ~DT:::coerceValue(.x, 1.1) )) %>% as_tibble()
      tna = tt %>% select(-plan) %>% pivot_longer(-name, names_to = 'column_name', values_to = 'value')
      tm = vals$treatment_table %>% 
        select(-plan) %>% 
        pivot_longer(-name, names_to = 'column_name', values_to = 'value') %>% 
        add_column(table_name = "treatment_table", .before = 1) %>% 
        rename(row_name = name)
      list(
        tt,
        tm[is.na(tna$value),] %>% separate_longer_delim(value, delim = " ")
      )
    })

    
    # RENDER AND UPDATE DATA
    render_DT <- function(table) {
      DT::renderDataTable(table, selection = 'none', options = dtoptions, rownames = FALSE, edit = TRUE)
    }
    # Treatments
    output$tro = render_DT(vals$treatment_table)
    
    proxy_tr = dataTableProxy('tro')
    
    observeEvent(input$tro_cell_edit, {
      info = input$tro_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      
      if(str_detect(v,"^[0-9. ,]+$")) {
        vals$treatment_table[i, j] <<- v
        replaceData(proxy_tr, vals$treatment_table, resetPaging = FALSE, rownames = FALSE)
      } else {
        showNotification("Only numerical values can be entered", type = "error")
      }
    })
  }    
)