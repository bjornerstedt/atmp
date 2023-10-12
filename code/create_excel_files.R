library(tidyverse)
library(writexl)

treatment_description = read_csv(show_col_types = FALSE, "
name, title,          value,    min,max,   description
treatment, Treatment,      ,       ,   ,   Treatment name
payment, Payment,          ,       ,   ,   Payment plans for treatment stage
state,     State,          ,      1,  ,    Health state
p_prog, Pr.prog,          0,      0,  1,   Probability of progression starting
p_death, Pr.death,        0,      0,  1,   Probability of dying 
QoL, QoL,                  ,      0,  1,   Quality of life
")

payment_description = read_csv(show_col_types = FALSE, "
name, title, value,               min,max, description
payment, Payment, ,              ,  ,  Payment planname
tot_payment,  Tot. payment, 0,    0,  ,  Total payment or yearly payment if payment is continuous
cont_payment, Cont. payment, 0,   0,  ,  Payment each year that the patient is alive (traditional) 
cost_trend, Cost Trend,      0, -.1,  .1,  Cost trend over time - for continuous payment
contract_length, Periods,   20,   0,  100,  Total length of payment
initial_payment, Initial,   0,    0,  1,  Share of payment in initial period
refund, Refund,             0,    0,  1,  Share of payments refunded upon failure (progression)
aggregate_failure, Agg. Fail, 0,  0,  1,  Threshold share of population for aggregate failure
")

global_description = read_csv(show_col_types = FALSE, "
name,           title,        value,  min,max,  description
discount,       HA discount,    0,    0,  .2,
firm_discount,  Firm discount,  0,    0,  .2,
time_horizon,   Time horizon,   20,   1,  100,  Time horizon of analysis
control_count,  Control count,  1,    1,    ,   Number of control treatments
"
) 

#
# -------------- EXAMPLE A ---------------------------
# Simple example with two treatments 
#
 
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

write_xlsx(models, "Example_A.xlsx")

#
# -------------- EXAMPLE B ---------------------------
# Allow comparison with ATMP with certain effect

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.04,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,     0.67,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.33,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.00,
ATMP certain, 1,        0.00,     1.0,    0.01,   For ATMP tr.
ATMP certain, 4,        0.00,     0.0,    0.00,
Comparison, 1,        1.00,     1.0,    0.02,   For comparator tr.
Comparison, 4,        0.00,     0.0,    0.00,   
")


models$Payments = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      10,         0,        10
For comparator tr., 0,          0.5, 
") 

write_xlsx(models, "Example_B.xlsx")

#
# -------------- EXAMPLE C ---------------------------
# Waiting with treatment, with discounting

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.04,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,     0.8,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.4,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.00,
ATMP wait,    1,        1.00,     1.0,    0.01,   For comparator tr.
ATMP wait,    2,        0.04,     0.8,    0.01,   For ATMP tr. later
ATMP wait,    3,        1.00,     0.4,    0.02,   For comparator tr.
ATMP wait,    4,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.01,    For comparator tr.
Comparison,   2,        1.00,     0.8,    0.02,    For comparator tr.
Comparison,   3,        1.00,     0.4,    0.02,    For comparator tr.
Comparison,   4,        0.00,     0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,  tot_payment, cont_payment,     contract_length
For ATMP tr.,             15,         0,        1
For ATMP tr. later,       10,         0,        1
For comparator tr.,       0,        0.5, 
") 

models$Globals = read_csv(show_col_types = FALSE, "
name, value
discount, 0.03
firm_discount, 0.1
time_horizon, 20
control_count, 1
"
) 

write_xlsx(models, "Example_C.xlsx")

#
# -------------- EXAMPLE D ---------------------------
# More complex payment method

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.04,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,     0.9,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.4,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.01,    For comparator tr.
Comparison,   2,        1.00,     0.9,    0.02,    For comparator tr.
Comparison,   3,        1.00,     0.4,    0.02,    For comparator tr.
Comparison,   4,        0.00,     0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,  tot_payment, cont_payment, contract_length, initial_payment, refund, aggregate_failure
For ATMP tr.,      15,         0,              10,       1,               0.5,    0.3            
For comparator tr., 0,       0.5,              ,          ,                  ,
") 

write_xlsx(models, "Example_D.xlsx")

#
# -------------- EXAMPLE E ---------------------------
# Cost saving ATMP

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     0.8,    0.02,   ATMP
ATMP,         2,        0,        0.7,    0.02,   Comparison
ATMP,         3,        0,        0.0,    0.00,
Comparison,   1,        0,        0.7,    0.02,   Comparison
Comparison,   2,        0,        0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,  tot_payment, cont_payment, contract_length, cost_trend
ATMP,     10,           0,          10,       0.05
Comparison,0,           0.5,          ,       0.05
") 

write_xlsx(models, "Example_E.xlsx")

#
# -------------- EXAMPLE F ---------------------------
# Compare payment lengths and hazard

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     1.0,     0.01,   For ATMP tr. 1
ATMP,         2,        1.00,     0.67,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.33,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,     0.00,
ATMP 10 yr,   1,        0.05,     1.0,     0.01,   For ATMP tr. 10
ATMP 10 yr,   2,        1.00,     0.67,    0.02,   For comparator tr.
ATMP 10 yr,   3,        1.00,     0.33,    0.02,   For comparator tr.
ATMP 10 yr,   4,        0.00,     0.0,     0.00,
ATMP cert,         1,   0.00,     1.0,     0.01,   For ATMP tr. 1
ATMP cert,         2,   1.00,     0.67,    0.02,   For comparator tr.
ATMP cert,         3,   1.00,     0.33,    0.02,   For comparator tr.
ATMP cert,         4,   0.00,     0.0,     0.00,
ATMP cert 10 yr,   1,   0.00,     1.0,     0.01,   For ATMP tr. 10
ATMP cert 10 yr,   2,   1.00,     0.67,    0.02,   For comparator tr.
ATMP cert 10 yr,   3,   1.00,     0.33,    0.02,   For comparator tr.
ATMP cert 10 yr,   4,   0.00,     0.0,     0.00,
Comparison,   1,        1.00,     1.0,     0.01,   For comparator tr.
Comparison,   2,        1.00,     0.67,    0.02,   For comparator tr.
Comparison,   3,        1.00,     0.33,    0.02,   For comparator tr.
Comparison,   4,        0.00,     0.0,     0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. 1,      10,         0,            1
For ATMP tr. 10,     10,         0,            10
For comparator tr., 0,          0.1, 
") 

write_xlsx(models, "Example_F.xlsx")