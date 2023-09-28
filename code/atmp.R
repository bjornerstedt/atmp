library(tidyverse)

# Check that all values are within limits set in the fields tables
#   For columns in dataset, check that each value is between min and max.
#   If not, save message indicating table, column, row and which limit fails
check_min_max <- function(indata, table_name) {
  table = indata[[str_c(table_name, '_table')]] 
  left_join(by = join_by(colname),
            table %>% pivot_longer(-where(negate(is.numeric)) , names_to = 'colname') ,
            indata[[str_c(table_name, '_description')]] %>% select(colname = name, title, min, max)
  ) %>% 
    mutate(
      table = table_name,
      row = row_number(),
      min_ok = value >= min, 
      max_ok = value <= max, 
    ) %>% 
    replace_na(list(min_ok = TRUE, max_ok = TRUE)) %>% 
    filter(!min_ok | !max_ok) %>% 
    mutate(
      message = str_c(sprintf("Value %.2f is ", value), if_else(!min_ok, "below minimum value", "above maximum value") )
    ) %>% 
    select(table, column = title, row, message)
}

check_indata <- function(indata) {
  
  errors = bind_rows(
    check_min_max(indata, 'state'),
    check_min_max(indata, 'payment')
  )
  errors
}

flextable_output <- function(df) {
  df %>% 
    flextable() %>% 
    theme_zebra() %>% 
    colformat_double(digits = 2, na_str = " ") %>% 
    autofit()
}

with_titles <- function(df, df_desc) {
  namelist = df_desc %>% select(title, name) %>% deframe()
  rename(df, any_of(namelist))
}

get_titles <- function(df, df_desc) {
  tibble(name =  df %>% names()) %>% left_join(df_desc, by = join_by(name)) %>% pull(title)
}

plot_QoL <- function(indata) {
  indata$state_table %>% 
    create_state_table() %>% 
    select(treatment, state=start, QoL) %>% 
    ggplot() +
    aes(state, QoL) + 
    geom_col(fill = lightblue) + 
    # coord_flip() + 
    facet_grid(rows = vars(treatment)) +
    labs(title = "QoL of health states")
}

plot_treatment_paths <- function(indata, treatment_name, reps = 8, T = 20) {
  ex_treatment = indata$treatment_table %>% 
    filter(name == treatment_name) %>% 
    pmap(Treatment) %>% pluck(1)
  
  result = c()
  for (i in 1:reps) {
    s = run.mc.sim(transition(ex_treatment), T = T)
    result = cbind(result, s) 
  }
  matplot(matrix(create_QoL(ex_treatment)[result], ncol = reps), main="Sample health outcomes",
          type='l', lty=1, col=1:5, ylim=c(0,1), ylab='QoL', xlab='year')
  abline(h=0, lty=3)
  abline(h=1, lty=3)
}

print_tansitions <- function(indata, treatment_name) {
  ex_treatment = indata$treatment_table %>% 
    filter(name == treatment_name) %>% 
    pmap(Treatment) %>% 
    pluck(1)
  P = transition(ex_treatment)
  P %>% as_tibble() %>% 
    mutate(st = row.names(P)) %>% 
    select(st, everything()) %>% 
    flextable() %>% 
    theme_box() %>% 
    bold(j = "st") %>% 
    set_header_labels(st = "") %>% 
    colformat_double(digits = 2, na_str = " ") 
}

# Get payments for all payment plans. Used in displaying model inputs
plot_payment_plans <- function(indata) {
  globals = named_list(indata$global_table, "name", "value")
  con_def = named_list(indata$payment_description, "name", "value")
  
  indata$payment_table %>% 
    left_join(by = join_by(payment),
              indata$state_table %>% 
                select(payment, start = state)
    ) %>% 
    # Do not distinguish between continuous payments with different start states:
    mutate(start = if_else(tot_payment > 0, start, 1)) %>% 
    distinct() %>% 
    transpose() %>% 
    map( ~modifyList(con_def, .)) %>% 
    map(~payment_plan(., globals)) %>% 
    setNames(indata$payment_table %>% pull(payment) ) %>% 
    as_tibble() %>% 
    mutate(time = row_number()) %>% 
    pivot_longer(-time, names_to = "payment_plan", values_to = "payment") %>%
    
    ggplot() + aes(time, payment) + 
    geom_col(fill = lightblue) + 
    facet_grid(rows = vars(payment_plan)) + 
    labs(title = "Payment plans")
}

plot_costs <- function(indata) {
  analyse_treatments(indata, over_time = TRUE) %>% 
    filter(costben != "QALY") %>% 
    ggplot() + 
    aes(time, value, fill = costben) + 
    geom_col()  + facet_grid(rows = vars(treatment)) + 
    labs(fill= "Arm", title = "Costs over time")
}

plot_QALY <- function(indata) {
  analyse_treatments(indata, over_time = TRUE) %>% 
    filter(costben == "QALY") %>% 
    ggplot() + 
    aes(time, value, color = treatment) + 
    geom_line() + labs(color ="Arm", title = "QALY over time")
}

################## BASIC FUNCTIONS #########################

discounting = function(ir, max_year) {
  year = 1:max_year
  ret = 1/(1 + ir)^(year-1)
  ret
}

hazard_calc <- function(sh, t) {
  p = exp( 1/t * log(sh))
  1 - p
}

# Modify a single value in the input tables, based on table, row and column names
set_value <- function(indata, table_name, row_name, column_name, value) {
  if (is.null(indata[[table_name]])) {
    stop("Could not find table: ", table_name)
  }
  sel = which(indata[[table_name]][["name"]] == row_name)
  if (is.null(  indata[[table_name]][[sel, column_name]] )) {
    stop("Could not find column: ", column_name, " in table: ", table_name)
  }
  if (length(sel) == 0) {
    stop("Could not find row: ", row_name, " in table: ", table_name)
  }
  indata[[table_name]][[sel, column_name]] = value
  indata
}

# Simulering för en patient
run.mc.sim <- function( P, T = 50 ) {
  num.states <- nrow(P)
  states     <- rep(num.states, T) # Sätt alla till död dvs num.states
  states[1]    <- 1 # Börja med full hälsa
  
  for(t in 2:T) {
    states[t] <-  which(rmultinom(1, 1,  P[states[t-1], ]) == 1)
    if (states[t] == num.states) {
      break
    }
  }
  return(states)
}


expected_markov <- function(P, T ) {
  num.states <- nrow(P)
  initstate = rep(0, num.states)
  initstate[1] = 1
  
  states = matrix(0, T, num.states)
  states[1,] = initstate
  colnames(states) = rownames(P)
  
  for (t in 2:T) {
    states[t,] = states[t-1,] %*% P 
  }
  states
}

markov <- function(n, P, T = 50, end = 2 ) {
  # Kod för Markov val, ny strat som funktion av tidigare strat och uniform draw
  states = 1:ncol(P)
  S = P
  for (i in 1:nrow(S)) {
    S[i,] = cumsum(S[i,])
  }

  rdraw = matrix(runif(n * T), nrow = T)
  outcome = matrix(rep(0, n * T), nrow = T)
  outcome[1,] = 1
  for (t in 2:T) {
    for (i in 1:n) {
      outcome[t,i] = states[ S[outcome[t-1,i], ] >= rdraw[t,i]][1]
      
    }
  }
  outcome
}

group_markov <- function(n, P, T = 50, end = 2 ) {
  num.states <- nrow(P)
  states     <- matrix(rep(num.states, num.states * T), ncol = num.states) # Sätt alla till död
  failures = rep(0, T)
  states[1,] = 0
  states[1,1]  = n # Börja med full hälsa
  
  istates = P
  
  for(t in 2:T) {
    for (s in 1:num.states) {
      istates[s,] <-  rmultinom(1, states[t-1, s],  P[s, ])
    }
    states[t, ] = colSums(istates)
    # Inflöde till failure state
    istates[end,] = 0
    failures[t] = sum(istates[,end])
  }
  colnames(states) = rownames(P)
  return(list(states, failures))
}

create_QoL <- function(tr) {
  if (tr$QoL_end > 0) {
    QoL = c(seq(tr$QoL_start, tr$QoL_end, length.out = tr$health_states-1), 0)
  } else {
    QoL = seq(tr$QoL_start, tr$QoL_end, length.out = tr$health_states)
  }
  QoL
}

# Put global_table in a list globals
get_globals <- function(global_table) {
  globals = global_table %>% select(value) %>% transpose() %>% flatten()
  names(globals) = global_table  %>% select(name) %>% pull()
  globals
}


# payment_plan creates a vector of discounted per period costs over the time_horizon
# It takes a payment_plan as input and returns payments per period.
# For non-continuous payment plans it assumes that payments start in period 1
payment_plan <- function(con, globals) {
  T = globals$time_horizon
  costs = rep(0, T)
  if (con$tot_payment > 0) {
    if (con$initial_payment > 0) {
      payment = rep(con$tot_payment*(1- con$initial_payment)/(con$contract_length - 1), con$contract_length)
      payment[1] = con$tot_payment*(con$initial_payment)
    } else {
      payment = rep(con$tot_payment/con$contract_length, con$contract_length)
    }
    # Payment plan can start in a later state if previous plans states are deterministic
    costs[con$start:min(length(payment) + con$start - 1, T)] = payment 
    costs = costs / 
      discounting(globals$firm_discount, T)  *
      discounting(globals$discount, T) 
  } else {
    # Continuous payment, with possibly a trend in costs
    costs[1:T] = con$cont_payment *
      discounting(con$cost_trend, T) 
    costs = costs  *
      discounting(globals$discount, T) 
  }
  # Future payments increase due to firm_discount, decrease with HA discount
  costs
}


# New functions --------------
# 


create_state_table = function(indata) {
  indata  %>% group_by(treatment) %>% 
    mutate(QoLstate = pmax(state, QoL), QoLnext = replace_na(lead(QoL), 0)) %>% fill(QoLstate) %>%  # Get QoLstate
    mutate(periods = replace_na(lead(state) - state, 1)) %>% 
    fill(QoL) %>% 
    uncount(periods) %>% 
    mutate(item = state, start = row_number()) %>% 
    group_by(treatment, QoLstate) %>% 
    mutate(QoL = QoL - (QoL - QoLnext)*(row_number() - 1 )/n() ) %>% 
    ungroup() %>% 
    select(-QoLstate, -QoLnext)
}

transition_matrix <- function(state_table) {
  state_table = state_table
  health_states = max(state_table$start)
  P = suppressWarnings(matrix(0.0, health_states, health_states))
  for (i in 1:(health_states-1)) {
    P[i, i] = 1 - state_table$p_prog[i] 
    P[i, i + 1] = state_table$p_prog[i] - state_table$p_death[i]
    P[i, health_states] =  P[i, health_states] + state_table$p_death[i] 
  }
  P[health_states, health_states] = 1
  rownames(P) = colnames(P) = c(sprintf("S%d", 1:(health_states - 1)), "D")
  P
}

get_QoL <- function(state_table) {
  state_table %>% pull(QoL)
  # if (tr$QoL_end > 0) {
  #   QoL = c(seq(tr$QoL_start, tr$QoL_end, length.out = tr$health_states-1), 0)
  # } else {
  #   QoL = seq(tr$QoL_start, tr$QoL_end, length.out = tr$health_states)
  # }
  # QoL
}

named_list <- function(table, name, value) {
  ret = as.list(table[[value]])
  names(ret) = table[[name]]
  ret  
}

create_payment_plans <- function(state_table_tr, indata) {
  globals = named_list(indata$global_table, "name", "value")
  con_def = named_list(indata$payment_description, "name", "value")
  cons = state_table_tr %>% 
    left_join(indata$payment_table, by = join_by(payment)) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    mutate(payment = replace_na(payment, "Death"))
  
  ret = c()
  for (i in 1:nrow(cons)) {
    con = cons %>% slice(i)  %>% as.list() %>% 
      modifyList(con_def, .)
    ret = c(ret, payment_plan(con, globals))
  }
  ret = matrix(ret, ncol = nrow(cons))
  ret
}

analyse_treatment <- function(state_table_tr, indata) {
  globals = named_list(indata$global_table, "name", "value")
  
  P = transition_matrix(state_table_tr) 
  
  states = expected_markov( P, globals$time_horizon)
  
  # CALCULATE COSTS AND QALY
  costs =  create_payment_plans(state_table_tr, indata) * states
  
  QALY = as.vector( states %*% get_QoL(state_table_tr)) * 
    discounting(globals$discount, globals$time_horizon)
  
  # Put into dataset and sum costs over states 
  ret = tibble()
  costs_df = costs %>% 
    as_tibble() %>% 
    mutate(time = row_number()) %>% 
    pivot_longer(-time, names_to = "start", values_to = "value", names_prefix = "S", names_transform = as.integer) %>% 
    left_join(state_table_tr %>% select(start, costben = payment), by = join_by(start)) %>% 
    filter(!is.na(costben)) %>% 
    group_by(time, costben) %>% 
    summarise(value = sum(value))
  
  ret = bind_rows(ret,
                  bind_rows(costs_df, tibble(time = 1:length(QALY), costben = "QALY", value = QALY) ) %>% 
                    add_column( treatment = state_table_tr$treatment[[1]], .before = 1)
  )
  ret
}

analyse_treatments <- function(indata, over_time = FALSE, show_details = FALSE) {
  
  state_table_all = create_state_table(indata$state_table) 
  
  treats = state_table_all %>% distinct(treatment) %>% pull()
  ret = tibble()
  for (i in 1:length(treats)) {
    state_table_tr = state_table_all %>% filter(treatment == treats[i])
    # print(analyse_treatment(state_table_tr, indata))
    ret = bind_rows(ret, analyse_treatment(state_table_tr, indata))
  }
  if (!over_time) {
    ret = ret %>% group_by(treatment, costben) %>% 
      summarise(value = sum(value)) %>% 
      mutate(
        cost = if_else(costben != "QALY", value, NA) ,
        QALY = if_else(costben == "QALY", value, NA) ,
        costben = if_else(costben != "QALY", costben, NA) 
      ) %>% select(-value) %>% rename(payment = costben)
    if (!show_details) {
      global_ea_count = 1
      df = ret %>% group_by(treatment) %>% 
        summarise(
          Cost = sum(cost, na.rm = TRUE) , 
          QALY = sum(QALY, na.rm = TRUE)
        )  
      cross_join(
        df %>% slice(1:global_ea_count) ,
        df %>% slice(-(1:global_ea_count))
      ) %>% 
        transmute(
          Treatments = str_c(treatment.x, " - ", treatment.y) ,
          QALY = QALY.x - QALY.y ,
          Cost = Cost.x - Cost.y ,
          ICER = Cost / QALY
        )
    } else {
      ret
    }
  } else {
    ret
  }
}

open_indata <- function( filename, vals = list(), indata = list()) {
  tryCatch(
    {
      indata$state_description = read_excel(filename, sheet = "State_fields") 
      indata$payment_description = read_excel(filename, sheet = "Payment_fields") 
      indata$global_table = read_excel(filename, sheet = "Globals") 
      indata$state_table = read_excel(filename, sheet = "States")
      indata$payment_table = read_excel(filename, sheet = "Payments")
      indata$global_description = read_excel(filename, sheet = "Global_fields")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  indata$errors <- NA # check_indata(indata)
  
  # DT::coerceValue wants a data.frame
  vals$state_table <- as.data.frame(indata$state_table)
  vals$payment_table <- as.data.frame(indata$payment_table)
  vals$global_table <- as.data.frame(indata$global_table)
  vals$state_description <- as.data.frame(indata$state_description)
  vals$payment_description <- as.data.frame(indata$payment_description)
  vals$global_description <- as.data.frame(indata$global_description)
  vals$errors <- indata$errors
  
  indata
}

lightblue = grDevices::rgb(128, 198, 212, maxColorValue = 255)

