library(tidyverse)
library(shiny)
library(DT)
library(writexl)
library(readxl)
library(rmarkdown)
library(markdown)
setwd("~/GitHub/atmp/code")

source('atmp.R')
options(dplyr.summarise.inform = FALSE)

# -------------- EXAMPLE A ---------------------------

indata = open_indata("example_A.xlsx")

global_table = read_csv(show_col_types = FALSE, "
name, value
discount, 0.0
firm_discount, 0.0
time_horizon, 20
control_count, 1
"
) 

indata$state_table = read_csv(show_col_types = FALSE, "
treatment,    state,    p_prog,   QoL,  p_death, payment
ATMP,         1,        0.05,     1.0,    0.01,   For ATMP tr.
ATMP,         2,        1.00,     0.67,    0.02,   For comparator tr.
ATMP,         3,        1.00,     0.33,    0.02,   For comparator tr.
ATMP,         4,        0.00,     0.0,    0.00,
Comparison,   1,        1.00,     1.0,    0.01,   For comparator tr.
Comparison,   2,        1.00,     0.67,    0.02,   For comparator tr.
Comparison,   3,        1.00,     0.33,    0.02,   For comparator tr.
Comparison,   4,        0.00,     0.0,    0.00,   
")

indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. ,      15,         0,            10
For comparator tr., 0,          0.1, 
") 

# Compare to result for ICER 2023-10-10:
# Compare to result for ICER 2023-10-12, with new probability def:
res = analyse_treatments(indata)
res
res %>% pull(ICER) %>% round(2) == 1.09


# -------------- EXAMPLE F ---------------------------

indata = open_indata("example_F.xlsx")

indata$state_table = read_csv(show_col_types = FALSE, "
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


indata$payment_table = read_csv(show_col_types = FALSE, "
payment,          tot_payment, cont_payment,  contract_length
For ATMP tr. 1,      10,         0,            1
For ATMP tr. 10,     10,         0,            10
For comparator tr., 0,          0.1, 
") 

indata$global_table

transition_matrix(create_state_table(indata$state_table) %>% filter(treatment == "ATMP"))

analyse_treatments(indata, show_details = TRUE)
analyse_treatments(indata)

plot_payment_plans(indata)
debug(plot_payment_plans)
undebug(plot_payment_plans)

# -------------- EXAMPLE D ---------------------------
indata = open_indata("example_D.xlsx")
analyse_treatments(indata, show_details = TRUE)
analyse_treatments(indata)

# -------------- EXAMPLE C ---------------------------
indata = open_indata("example_C.xlsx")
create_state_table(indata$state_table)
