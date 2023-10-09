library(tidyverse)
setwd("~/GitHub/atmp/code")

source('atmp.R')

indata = open_indata("example_A2.xlsx")

# -------------- EXAMPLE A ---------------------------

indata$state_table = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     1.0,    0.01,   For ATMP tr.
ATMP,         3,        1.00,        ,    0.02,   For comparator tr.
ATMP,         6,        0.00,     0.0,    0.00,
ATMP certain, 1,        0.00,     1.0,    0.01,   For ATMP tr.
ATMP certain, 6,        0.00,     0.0,    0.00,
Comparison 1, 1,        1.00,     1.0,    0.02,   For comparator 1 tr.
Comparison 1, 6,        0.00,     0.0,    0.00,   
Comparison 2, 1,        1.00,     1.0,    0.015,   For comparator 2 tr.
Comparison 2, 6,        0.00,     0.0,    0.00,   
")


indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      10,         0,        10
For comparator 1 tr., 0,          0.5, 
For comparator 2 tr., 0,          0.75, 
") 

indata$global_table = read_csv(show_col_types = FALSE, "
name, value
discount, 0.03
firm_discount, 0.03
time_horizon, 20
control_count, 2
"
) 

analyse_treatments(indata)

analyse_treatments(indata, show_details = TRUE)

df = analyse_treatments(indata, over_time = TRUE) %>% 
  group_by(treatment, costben) %>% 
  summarise(value = sum(value)) %>% 
  mutate(
    cost = if_else(costben != "QALY", value, NA) ,
    QALY = if_else(costben == "QALY", value, NA) ,
    costben = if_else(costben != "QALY", costben, NA) 
  ) %>% select(-value) %>% rename(payment = costben) %>% group_by(treatment) %>% 
  summarise(
    Cost = sum(cost, na.rm = TRUE) , 
    QALY = sum(QALY, na.rm = TRUE)
  )  


column_to_rownames(indata$global_table, "name")[['control_count',1]]
