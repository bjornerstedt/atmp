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
ATMP,         1,        0.05,     1.0,    0.01,   For ATMP tr.
ATMP,         3,        1.00,        ,    0.02,   For comparator tr.
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,   For comparator tr.
Comparison,   6,        0.00,     0.0,    0.00,   
")


indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      10,         0,        10
For comparator tr., 0,          0.5, 
") 

state_table_tr = indata$state_table %>% filter(treatment == "ATMP") %>%  create_state_table() 

P = transition_matrix(state_table_tr) 

states = expected_markov( P, globals$time_horizon)

# Modification
costs =  create_payment_plans(state_table_tr %>% mutate(start = pstart), indata) * states

costs_df = costs %>% 
  as_tibble() %>% 
  mutate(time = row_number()) %>% 
  pivot_longer(-time, names_to = "start", values_to = "value", names_prefix = "S", 
               names_transform = as.integer,values_drop_na = FALSE) %>% 
  left_join(state_table_tr %>% select(start, costben = payment), by = join_by(start)) %>% 
  filter(!is.na(costben)) %>% 
  group_by(time, costben) %>% 
  summarise(value = sum(value))

create_payment_plans(state_table_tr, indata)
