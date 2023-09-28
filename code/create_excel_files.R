library(tidyverse)
library(writexl)


treatment_description = read_csv(show_col_types = FALSE, "
name, title, value,               min,max, description
plan, Arm,        ,               0,  ,   Arm combining Treatment and Payment 
name, Treatment,    ,             ,  ,    Treatment name
p_HU, Pr. PF->P, 0,               0,  1,   Probability of progression starting
p_HD, Pr. PF->D,          0,      0,  1,   Probability of dying when progression free
p_UD, Pr. P->D, 0,                0,  1,   Probability of dying in progression state
health_states, Health states,  0, 2,  ,   Number of health states
QoL_column, QoL Column, ,         0,  ,   Name of column in QoL table (non-linear progression)
QoL_start, QoL Start,   1,        0,  1,   Initial QoL
QoL_end, QoL End,       0,        0,  1,   Final QoL
random_state, Random state, 1,    1,  ,   Which state is the random state. To model progression before treatment
")

contract_description = read_csv(show_col_types = FALSE, "
name, title, value,               min,max, description
plan, Arm, ,                       ,  ,  Arm combining Treatment and Payment 
name, Payment plan, ,              ,  ,  Payment name
tot_payment,  Tot. payment, 0,    0,  ,  Total payment or yearly payment if payment is continuous
cont_payment, Cont. payment, 0,   0,  ,  Payment each year that the patient is alive (traditional) 
cost_trend, Cost Trend,      0, -.1,  .1,  Cost trend over time - for continuous payment
contract_length, Periods,   20,   0,  100,  Total length of payment
initial_payment, Initial,   0,    0,  1,  Share of payment in initial period
refund, Refund,             0,    0,  1,  Share of payments refunded upon failure (progression)
start, Start,               1,    0,  ,  State where payment begins (ex: additional treatment in progression) 
end, End,                   2,    0,  ,  State where payment ends
aggregate_failure, Agg. Fail, 0,  0,  1,  Threshold share of population for aggregate failure
")

payment_description = read_csv(show_col_types = FALSE, "
name, title, value,               min,max, description
plan, Arm, ,                       ,  ,  Arm combining Treatment and Payment 
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

state_description = read_csv(show_col_types = FALSE, "
name, title, value,               min,max, description
arm, Arm,        ,                0,  1,   Arm specification when there are more than two treatments 
treatment, Treatment,    ,             ,  ,    Treatment name
payment, payment,  , ,  ,   Payment plans for treatment stage
p_prog, Pr.prog, 0,               0,  1,   Probability of progression starting
p_death, Pr.death,          0,      0,  1,   Probability of dying when progression free
QoL, QoL, ,         0,  ,   Quality of life
")

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
plan, name, p_HU, health_states
1, ATMP,              0.04, 4
0, Comparison,        1, 4
") 

contract_table = read_csv(show_col_types = FALSE, "
plan, name,tot_payment, cont_payment,     contract_length, end
1, For ATMP tr.,10,0,        10,2
0, For Comparator tr.,0,0.5,            0,4
") 

state_table = read_csv(show_col_types = FALSE, "
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

models = list(
  Globals = global_table ,
  Treatments = treatment_table , 
  Contracts = contract_table , 
  States = state_table,
  Treatment_fields = treatment_description, 
  Contract_fields = contract_description,
  State_fields = state_description,
  Payment_fields = payment_description,
  Global_fields = global_description
)

write_xlsx(models, "Simple.xlsx")

# -------------- EXAMPLE A ---------------------------

models$Treatments = read_csv(show_col_types = FALSE, "
plan, name, p_HU, p_HD, p_UD, health_states
1, ATMP,              0.04, 0.01, 0.02, 6
0, Comparison,        0.98,    0.02, 0.02, 6
") 

models$Contracts = read_csv(show_col_types = FALSE, "
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
1, For ATMP tr.,10,0,        10,0,             0, 1,2
1, For Comparator tr. on failure,0,0.5,            0,0,             0,2,6
0, For Comparator tr.,0,0.5,            0,0,             0,1,6
") %>% 
  select(-refund, -initial_payment)

models$States = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,        ,    0.02,   For comparator tr.
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,   For comparator tr.
Comparison,   6,        0.00,     0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      10,         0,        10
For comparator tr., 0,          0.5, 
") 

write_xlsx(models, "Example_A.xlsx")

#
# -------------- EXAMPLE B ---------------------------
# Cost saving ATMP

models$Treatments = read_csv(show_col_types = FALSE, "
plan, name, p_HU, p_HD, p_UD, health_states, QoL_start, QoL_end, random_state2
1, ATMP,              0.03, 0.02, 0.02, 4,     0.8, 0.7,    2
0, Comparison,        0.00, 0.02, 0.02,    4,    0.7, 0.7,  0
") # %>% select(-QoL_start, -QoL_end)

models$Contracts = read_csv(show_col_types = FALSE, "
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end, cost_trend
1, For ATMP tr.,            10,0,      10,0,             0, 1,2, 0
1, For failure tr. w trend , 0,0.5,      0,0,             0, 2,4, 0.05
0, For comparator tr. w trend,      0,0.5,      0,0,             0, 1,4, 0.05
") 

models$States = read_csv(show_col_types = FALSE, "
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

models$Treatments = read_csv(show_col_types = FALSE, "
plan, name, p_HU, p_HD, p_UD, health_states
1, ATMP,              0.04, 0.01, 0.02, 6
1, ATMP Certain,      0, 0.01, 0.02, 6
0, Comparison,        1,    0.01, 0.02, 6
") 

models$Contracts = read_csv(show_col_types = FALSE, "
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
1, For ATMP tr.,10,0,        10,0,             0, 1,2
1, For Hospital tr.,0.5,0,           1,0,           0,1,6
1, For failure tr.,0,0.5,      0,0,             0,2,6
0, For comparator tr.,0,0.5,            0,0,             0,1,6
")

# Same payments as in previous

write_xlsx(models, "Example_A2.xlsx")

#
# -------------- EXAMPLE C ---------------------------
# Waiting with treatment

models$Treatments = read_csv(show_col_types = FALSE, "
plan, name, p_HU, p_HD, p_UD, health_states, random_state
1, ATMP,              0.04, 0.01, 0.02, 16, 1
2, ATMP 2,              0.04, 0.01, 0.02, 16, 3
0, Comparison,        1,    0.01, 0.02, 16, 1
") 


models$Contracts = read_csv(show_col_types = FALSE, "
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
1, For ATMP tr.,10,0,        10,0,             0, 1,2
2, For ATMP tr., 8,0,        10,0,             0, 3,4
2, For tr. while waiting,0,0.5,            0,0,             0,1,6
2, For failure tr., 0,0.5,            0,0,             0,1,6
0, For comparator tr.,0,0.5,            0,0,             0,1,6
")

models$States = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        1.00,      1.0,    0.02,   Comparison
ATMP,         2,        0.05,         ,    0.01,   ATMP
ATMP,         3,        1.00,         ,    0.02,   Comparison
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,    Comparison
Comparison,   6,        0.00,     0.0,    0.00,   
")

models$Payments = read_csv(show_col_types = FALSE, "
payment,tot_payment, cont_payment,     contract_length
ATMP,10,0,        10
Comparison,0,0.5, 
") 

write_xlsx(models, "Example_C.xlsx")
