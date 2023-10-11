library(tidyverse)
setwd("~/GitHub/atmp/code")

source('atmp.R')

# -------------- EXAMPLE A ---------------------------

global_table = read_csv(show_col_types = FALSE, "
name, value
discount, 0.0
firm_discount, 0.0
time_horizon, 20
control_count, 1
"
) 

treatment_table = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.04,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,     0.67,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.33,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.01,   For comparator tr.
Comparison,   2,        1.00,     0.67,    0.02,   For comparator tr.
Comparison,   3,        1.00,     0.33,    0.02,   For comparator tr.
Comparison,   4,        0.00,     0.0,    0.00,   
")

payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      15,         0,            10
For comparator tr., 0,          0.1, 
") 

models = list(
  Treatments = treatment_table,
  Payments = payment_table,
  Globals = global_table ,
  Treatment_fields = treatment_description,
  Payment_fields = payment_description,
  Global_fields = global_description
)

# Compare to result for ICER 2023-10-10:
res = analyse_treatments(indata)
res %>% pull(ICER) %>% round(2) == 1.0
