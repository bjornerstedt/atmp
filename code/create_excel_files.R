library(tidyverse)
library(writexl)


treatment_description = read_csv("
name, title, value,                      description
plan, Arm,        ,               Arm combining Treatment and Contract 
name, Treatment,    ,              Treatment name
p_HU, Pr. PF->P, 0,    Probability of progression starting
p_HD, Pr. PF->D,          0,     Probability of dying when progression free
p_UD, Pr. P->D, 0,     Probability of dying in progression state
health_states, Health states,  0,      Number of health states
QoL_column, QoL Column, ,          Name of column in QoL table (non-linear progression)
QoL_start, QoL Start,   1,    Initial QoL
QoL_end, QoL End,       0,    Final QoL
random_state, Random state, 1, Which state is the random state. To model progression before treatment
")

contract_description = read_csv("
name, title, value,               description
plan, Arm, ,                Arm combining Treatment and Contract 
name, Payment plan, ,            Payment name
tot_payment,  Tot. payment, 0,     Total payment or yearly paymentif payment is continuous
cont_payment, Cont. payment, 0, Payment each year that the patient is alive (traditional) 
cost_trend, Cost Trend,      0, Cost trend over time - for continuous payment
contract_length, Periods,   20, Total length of contract
initial_payment, Initial,   0,    Share of payment in initial period
refund, Refund,             0, Share of payments refunded upon failure (progression)
start, Start,               1, State where contract begins (ex: additional treatment in progression) 
end, End,                   2, State where contract ends
aggregate_failure, Agg. Fail, 0, Threshold share of population for aggregate failure
")

global_description = read_csv("
name,           title,        description
discount,       HA discount, 
firm_discount,  Firm discount,
time_horizon,   Time horizon, Time horizon of analysis
threshold,      ICER threshold,
active_plan,    Active arm,
control_plan,   Control arm,
"
) 

#
# -------------- EXAMPLE A ---------------------------

global_table = read_csv("
name, value
discount, 0.0
firm_discount, 0.0
time_horizon, 20
threshold, 1
active_plan, 1
control_plan, 0
"
) 

treatment_table = read_csv("
plan, name, p_HU, p_HD, p_UD, health_states
1, ATMP,              0.04, 0.01, 0.02, 6
0, Comparison,        1,    0.01, 0.02, 6
") 

contract_table = read_csv("
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
1, ATMP company,10,0,        10,0,             0, 1,2
0, Comparison,0,0.5,            0,0,             0,1,6
") %>% 
  select(-refund, -start, -initial_payment)

# state_table = read_csv("
# name,   state,    hazard,   QoL,  death_hazard
# ATMP,   1,        0.05,     1.0,    0.01
# ATMP,   2,        1.00,     0.8,    0.02
# ATMP,   3,        1.00,     0.6,    0.02
# ATMP,   4,        1.00,     0.4,    0.02
# ATMP,   5,        1.00,     0.2,    0.02
# ATMP,   6,        0.00,     0.0,    0.02
# ")

models = list(
  Globals = global_table ,
  Treatments = treatment_table , 
  Contracts = contract_table , 
  # QoL = tibble(QoL1 = QoL),
  # States = state_table,
  Treatment_fields = treatment_description, 
  Contract_fields = contract_description,
  Global_fields = global_description
)

write_xlsx(models, "Example_A.xlsx")

#
# -------------- EXAMPLE B ---------------------------
# Cost saving ATMP

models$Treatments = read_csv("
plan, name, p_HU, p_HD, p_UD, health_states, QoL_start, QoL_end, random_state2
1, ATMP,              0.03, 0.02, 0.02, 4,     0.8, 0.7,    2
0, Comparison,        0.00, 0.02, 0.02,    4,    0.7, 0.7,  0
") # %>% select(-QoL_start, -QoL_end)

models$Contracts = read_csv("
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end, cost_trend
1, ATMP company,            10,0,      10,0,             0, 1,2, 0
1, Failure treat. w trend , 0,0.5,      0,0,             0, 2,4, 0.05
0, Comparison w trend,      0,0.5,      0,0,             0, 1,4, 0.05
") 

write_xlsx(models, "Example_B.xlsx")

#
# -------------- EXAMPLE A2 ---------------------------
# More complex payment schemes

models$Globals = read_csv("
name, value
discount, 0.03
firm_discount, 0.03
threshold, 1
time_horizon, 20
active_plan, 1
control_plan, 0
"
) 


models$Treatments = read_csv("
plan, name, p_HU, p_HD, p_UD, health_states
1, ATMP,              0.04, 0.01, 0.02, 6
1, ATMP Certain,      0, 0.01, 0.02, 6
0, Comparison,        1,    0.01, 0.02, 6
") 

models$Contracts = read_csv("
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
1, ATMP company,10,0,        10,0,             0, 1,2
1, Hospital,0.5,0,           1,0,           0,1,6
1, Failure treatment,0,0.5,      0,0,             0,2,6
0, Comparison,0,0.5,            0,0,             0,1,6
")

write_xlsx(models, "Example_A2.xlsx")

#
# -------------- EXAMPLE C ---------------------------
# Waiting with treatment

models$Treatments = read_csv("
plan, name, p_HU, p_HD, p_UD, health_states, random_state
1, ATMP,              0.04, 0.01, 0.02, 16, 1
2, ATMP 2,              0.04, 0.01, 0.02, 16, 3
0, Comparison,        1,    0.01, 0.02, 16, 1
") 


models$Contracts = read_csv("
plan, name,tot_payment, cont_payment,     contract_length,initial_payment,refund, start, end
1, ATMP payment,10,0,        10,0,             0, 1,2
2, ATMP future payment, 8,0,        10,0,             0, 3,4
2, Initial waiting,0,0.5,            0,0,             0,1,6
2, Failure treatment, 0,0.5,            0,0,             0,1,6
0, Comparison,0,0.5,            0,0,             0,1,6
")

# write_xlsx(models, "Example_C.xlsx")
