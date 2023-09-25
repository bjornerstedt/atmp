library(tidyverse)
library(readxl)
source('atmp.R')

indata = open_indata("Example_A.xlsx")


check_indata(indata)

indata$treatment_table = read_csv("
plan, name, p_HU, health_states
1, ATMP,              0.04, 4
0, Comparison,        2, 4
") 

check_indata(indata)