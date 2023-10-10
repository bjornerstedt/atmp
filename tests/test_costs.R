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
ATMP,         2,        1.00,        ,    0.02,   For comparator tr.
ATMP,         6,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.02,   For comparator tr.
Comparison,   6,        0.00,     0.0,    0.00,   
")


indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length, aggregate_failure, refund
For ATMP tr. ,      10,         0,            10,               0.3,              .5
For comparator tr., 0,          0.5, 
") 

state_table_tr = indata$state_table %>% filter(treatment == "ATMP") %>%  create_state_table() 

P = transition_matrix(state_table_tr) 

states = expected_markov( P, globals$time_horizon)

# Modification
# state_table_tr

# calculate_costs(state_table_tr , indata, states) 
# analyse_treatment(state_table_tr , indata) 

# calculate_costs <- function(state_table_tr, indata, states) {
  globals = named_list(indata$global_table, "name", "value")
  con_def = named_list(indata$payment_description, "name", "value")
  cons = state_table_tr %>% 
    mutate(start = pstart) %>%  # TODO: remove start variable
    left_join(indata$payment_table, by = join_by(payment)) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    mutate(payment = replace_na(payment, "Death"))
  
  pay = c()
  for (i in 1:nrow(cons)) {
    con = cons %>% slice(i)  %>% as.list() %>% 
      modifyList(con_def, .)
    pay = c(pay, payment_plan(con, globals))
  }
  pay = matrix(pay, ncol = nrow(cons))
  # pay * states
# }

  # Add defaults to payments
paysched = cons %>% as.list() %>% 
    modifyList(con_def, .) %>% as_tibble() %>% 
   select(-QoL, -p_death, -p_prog) %>% distinct() %>% 
  filter(tot_payment > 0) 
# Check that only one row has tot payment > 0

if(paysched$aggregate_failure > 0) {
  initshare = states[paysched$pstart, paysched$pstart]

  vec = 1:ncol(states)
  if_else(vec >= paysched$pstart & vec <= paysched$pend, 1, 0)
  initshare - states %*% rs(vec)  
}

if(paysched$refund > 0) {

  }

sccost = paysched %>% as.list() %>% payment_plan(globals)

cumcosts = lag(cumsum(sccost))
if (paysched$contract_length < globals$time_horizon) {
  cumcosts[(paysched$contract_length + 1):length(cumcosts)] = 0
}
cumcosts[1] = 0

# Share leaving the last state of payment sched
failing = states[, paysched$pend] * P[paysched$pend,  paysched$pend + 1]

reimburse = failing * con$refund * cumcosts



 analyse_treatment2 <- function(state_table_tr, indata) {
  globals = named_list(indata$global_table, "name", "value")
  
  P = transition_matrix(state_table_tr) 
  
  states = expected_markov( P, globals$time_horizon)
  
  # CALCULATE COSTS AND QALY
  costs =  calculate_costs(state_table_tr , indata, states) 
  # costs =  create_payment_plans(state_table_tr, indata) * states
  
  QALY = as.vector( states %*% get_QoL(state_table_tr)) * 
    discounting(globals$discount, globals$time_horizon)
  
  ret = tibble()
  # Put into dataset and sum costs over states 
  costs_df = costs %>% 
    as_tibble() %>% 
    setNames( 1:ncol(costs)) %>% 
    mutate(time = row_number()) %>% 
    pivot_longer(-time, names_to = "start", values_to = "value", names_transform = as.integer) %>% 
    left_join(state_table_tr , by = join_by(start)) 
    # left_join(state_table_tr %>% select(start, costben = payment), by = join_by(start)) %>% 
    # filter(!is.na(costben)) %>% 
    # group_by(time, costben) %>% 
    # summarise(value = sum(value))
  
  costs_df
  
  ret = bind_rows(ret,
                  bind_rows(costs_df, tibble(time = 1:length(QALY), costben = "QALY", value = QALY) ) %>% 
                    add_column( treatment = state_table_tr$treatment[[1]], .before = 1)
  )
  ret
}


