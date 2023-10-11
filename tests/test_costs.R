library(tidyverse)
setwd("~/GitHub/atmp/code")

source('atmp.R')

indata = open_indata("example_A.xlsx")

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
ATMP,         1,        0.05,     1.0,    0.0,   For ATMP tr.
ATMP,         2,        1.00,        ,    0.0,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.0,
Comparison,   1,        1.00,     1.0,    0.0,   For comparator tr.
Comparison,   4,        0.00,     0.0,    0.0,   
")


indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length, aggregate_failure, refund, initial_payment
For ATMP tr. ,      10,         0,            10,               0.0,              1,      1
For comparator tr., 0,          0.5, 
") 

state_table_tr = indata$state_table %>% filter(treatment == "ATMP") %>%  create_state_table() 

globals = named_list(indata$global_table, "name", "value")

P = transition_matrix(state_table_tr) 

states = expected_markov( P, globals$time_horizon)

# Modification
# state_table_tr

# analyse_treatment(state_table_tr , indata) 


df = calculate_costs(state_table_tr , indata, states) 
df
df = analyse_treatments(indata, over_time = TRUE) %>% filter(costben != "QALY") %>% pivot_wider( names_from = "costben", values_from = "value")
# df$state = states[,1]
df
# sum(df$Refund)
# 1-states[9:12,1]
plot_QALY(indata)
plot_costs(indata)
