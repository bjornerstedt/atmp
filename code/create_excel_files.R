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
"
) 

#
# -------------- SIMPLE EXAMPLE ---------------------------

global_table = read_csv(show_col_types = FALSE, "
name, value
discount, 0.0
firm_discount, 0.0
time_horizon, 20
"
) 

treatment_table = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,     0.67,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.33,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,   For comparator tr.
Comparison,   2,        1.00,     0.67,    0.02,   For comparator tr.
Comparison,   3,        1.00,     0.33,    0.02,   For comparator tr.
Comparison,   4,        0.00,     0.0,    0.00,   
")

payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      10,         0,        10
For comparator tr., 0,          0.5, 
") 

models = list(
  Treatments = treatment_table,
  Payments = payment_table,
  Globals = global_table ,
  Treatment_fields = treatment_description,
  Payment_fields = payment_description,
  Global_fields = global_description
)

write_xlsx(models, "Simple.xlsx")

# -------------- EXAMPLE A ---------------------------

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,        ,    0.02,   For comparator tr.
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,   For comparator tr.
Comparison,   6,        0.00,     0.0,    0.00,   
")

write_xlsx(models, "Example_A.xlsx")

#
# -------------- EXAMPLE B ---------------------------
# Cost saving ATMP

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,      0.8,    0.02,   ATMP
ATMP,         2,        0.02,     0.7,    0.02,   Comparison
ATMP,         3,        0,        0.0,    0.00,
Comparison,   1,        0.02,     0.7,    0.02,   Comparison
Comparison,   2,        0,        0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,  tot_payment, cont_payment, contract_length, cost_trend
ATMP,     10,           0,          10,       0.05
Comparison,0,           0.5,          ,       0.05
") 

write_xlsx(models, "Example_B.xlsx")

#
# -------------- EXAMPLE A2 ---------------------------
# More complex payment schemes

models$Globals = read_csv(show_col_types = FALSE, "
name, value
discount, 0.03
firm_discount, 0.03
time_horizon, 20
"
) 

# models$Treatments = read_csv(show_col_types = FALSE, "
# plan, name, p_HU, p_HD, p_UD, health_states
# 1, ATMP,              0.04, 0.01, 0.02, 6
# 1, ATMP Certain,      0, 0.01, 0.02, 6
# 0, Comparison,        1,    0.01, 0.02, 6
# ") 
# 
# models$Contracts = read_csv(show_col_types = FALSE, "
# plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
# 1, For ATMP tr.,10,0,        10,0,             0, 1,2
# 1, For Hospital tr.,0.5,0,           1,0,           0,1,6
# 1, For failure tr.,0,0.5,      0,0,             0,2,6
# 0, For comparator tr.,0,0.5,            0,0,             0,1,6
# ")

# Same payments as in previous

write_xlsx(models, "Example_A2.xlsx")

#
# -------------- EXAMPLE C ---------------------------
# Waiting with treatment

models$Treatments = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        1.00,      1.0,    0.02,   Comparison
ATMP,         2,        0.05,         ,    0.01,   ATMP
ATMP,         3,        1.00,         ,    0.02,   Comparison
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,    Comparison
Comparison,   6,        0.00,     0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,  tot_payment, cont_payment,     contract_length
ATMP,             10,         0,        10
Comparison,         0,        0.5, 
") 

write_xlsx(models, "Example_C.xlsx")
