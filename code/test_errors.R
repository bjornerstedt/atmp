library(tidyverse)
source('atmp.R')
source('atmp_new.R')

indata = load_data(filename = "example_A.xlsx")

payment_description = read_csv(show_col_types = FALSE, "
name, title, value,               min,max, description
payment, Payment, ,            ,  ,  Payment planname
tot_payment,  Tot. payment,   0,    0,  ,  Total payment or yearly payment if payment is continuous
cont_payment, Cont. payment,  0,   0,  ,  Payment each year that the patient is alive (traditional) 
cost_trend, Cost Trend,       0, -.1,  .1,  Cost trend over time - for continuous payment
contract_length, Periods,    20,   0,  100,  Total length of payment
initial_payment, Initial,     0,   0,  1,  Share of payment in initial period
refund, Refund,               0,   0,  1,  Share of payments refunded upon failure (progression)
aggregate_failure, Agg. Fail, 0,   0,  1,  Threshold share of population for aggregate failure
")

indata$state_description = read_csv(show_col_types = FALSE, "
name, title,          value,    min,max,   description
treatment, Treatment,      ,       ,   ,   Treatment name
payment, payment,          ,       ,   ,   Payment plans for treatment stage
state,     State           ,      1,  ,    Health state
p_prog, Pr.prog,          0,      0,  1,   Probability of progression starting
p_death, Pr.death,        0,      0,  1,   Probability of dying 
QoL, QoL,                  ,      0,  1,   Quality of life
")

# -------------- EXAMPLE A ---------------------------

indata$state_table = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        -0.05,     -1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,        ,    0.02,   For comparator tr.
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    -0.02,   For comparator tr.
Comparison,   6,        0.00,     0.0,    2.00,   
")

indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      10,         0,        1000
For comparator tr., 0,          0.5, 
") 

table_name = 'state'
check_min_max <- function(indata, table_name) {
  table = indata[[str_c(table_name, '_table')]] 
  left_join(by = join_by(colname),
            table %>% pivot_longer(-where(negate(is.numeric)) , names_to = 'colname') ,
            indata[[str_c(table_name, '_description')]] %>% select(colname = name, title, min, max)
  ) %>% 
    mutate(
      table = table_name,
      row = row_number(),
      min_ok = value >= min, 
      max_ok = value <= max, 
    ) %>% 
    replace_na(list(min_ok = TRUE, max_ok = TRUE)) %>% 
    filter(!min_ok | !max_ok) %>% 
    mutate(
      message = str_c(sprintf("Value %.2f is ", value), if_else(!min_ok, "below minimum value", "above maximum value") )
    ) %>% 
    select(table, column = title, row, message)
}

check_indata <- function(indata) {
  
  errors = bind_rows(
    check_min_max(indata, 'state'),
    check_min_max(indata, 'payment')
  )
  errors
}


check_indata_out <- function(indata) {
  
  #   Contracts
  #     end should be after beginning
  #     Not both tot_payment > 0 and cont_payment > 0
  #   Joint
  #     contract$end <= treatment$health_states for each plan in treatments
  start = 1 # default value if column start does not exist
  errors2 = left_join(by = join_by(plan),
                      indata$contract_table , 
                      indata$treatment_table %>% select(plan, health_states)
  ) %>% 
    transmute(
      table = "contracts",
      row = row_number(), 
      column = "end, health_states",
      message = 
        if_else(end > health_states, 'end is set greater than number of health states', 
                if_else(end <= start, "end has to be later than start", NA)
        )
    ) %>% 
    filter(!is.na(message))
  bind_rows(errors, errors2)
  
  # Check for other logical inconsistencies 
  #   Treatments
  #     QoLend should be greater than QoLstart
  #   
  #   States
  #     treatment and payment exist, except last entry in each treatment (ie. death)
  #     payment exists in payments table
  #     probabilities add up 
  #     states start at 1, end at higher value and is increasing for is.na
  #     if QoL does not exist, add column with 1 for first row and 0 for last in each group
  #     
}  
ncol(indata$state_table)

check_indata(indata)