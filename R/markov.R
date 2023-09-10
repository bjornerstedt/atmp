library(tidyverse)

open_indata <- function(infile) {
  indata = list(
    treatment_table = read_excel(infile, sheet = "Treatments") ,
    contract_table = read_excel(infile, sheet = "Contracts") ,
    global_table = read_excel(infile, sheet = "Globals") ,
    treatment_description = read_excel(infile, sheet = "Treatment_fields") ,
    contract_description = read_excel(infile, sheet = "Contract_fields")
  )
  if ("States" %in% excel_sheets(infile)) {
      indata$state_table = read_excel(infile, sheet = "States")
  }
  indata
}

# qcol = "QoL"
# QoL = read_excel(infile, sheet = "QoL") %>% select(matches(qcol)) 
# if (ncol(QoL) > 0) {
#   QoL = QoL %>% pull()
# } else {
#   stop(str_c("Could not find QoL column: ", qcol))
# }

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


expected_markov <- function(tr, T ) {
  P = transition( tr)
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

# TODO: Unused function 
cost_function = function(s, cfunc = c(1), terminate = TRUE, reimbursetime = -1) {
  costs = rep(0, length(s))
  if (reimbursetime < 0 | reimbursetime <= min(which(s > 1)) ) {
    costs[1:length(cfunc)] = cfunc
  }
  if (terminate) {
    costs[ s > 1 ] = 0
  }
  costs
}

create_QoL <- function(tr) {
  if (tr$QoL_end > 0) {
    QoL = c(seq(tr$QoL_start, tr$QoL_end, length.out = tr$health_states-1), 0)
  } else {
    QoL = seq(tr$QoL_start, tr$QoL_end, length.out = tr$health_states)
  }
  QoL
}


Treatment <- function(name = "Treatment", p_HU = 1, p_HD = 0, p_UD = 0, QoL_col = NA, health_states = 0, end = 2, plan = 0, 
                      QoL_start = 1, QoL_end = 0, random_state = 1, random_state2 = 0, p_HU2 = 0) {
  # IF prog_time > 1 create linear QoL with that many states
  if(health_states < 2) { 
    # TODO: Check that states is not too big
    # If QoL_end is not zero allow for death state
    stop(sprintf("health_states must be at least 2, now: %d", health_states))
  }
  
  # P = cr_transition(QoL, p_HU, p_HD, p_UD)
  # names(QoL) = colnames(P)
  
  # Returns a Treatment class
  structure(list(
    plan = plan,
    name = name,
    p_HU = p_HU, 
    p_HD = p_HD, 
    p_UD = p_UD,
    health_states = health_states,
    end = end ,
    QoL_start = QoL_start, 
    QoL_end = QoL_end, 
    random_state = random_state,
    p_HU2 = p_HU2, 
    random_state2 = random_state2
  ), class = "Treatment")  
}

transition <- function(tr) {
  P = matrix(0, tr$health_states, tr$health_states)
  
  if (tr$health_states <= tr$random_state + 1) {
    stop("The random_state is too big compared with the number of health_states")
  }
  if (tr$health_states > 2) {
    for (i in 1:(tr$health_states-1)) {
      P[i, i + 1] = 1 - tr$p_UD
      P[i, tr$health_states] =  tr$p_UD
    }
    P[tr$health_states-1, tr$health_states] =  1
  } else {
    P[1, 2] = tr$p_HU + tr$p_HD
  }
  P[tr$health_states, tr$health_states] = 1
  
  P[tr$random_state, tr$random_state] = 1 - tr$p_HU - tr$p_HD
  P[tr$random_state, tr$random_state + 1] = tr$p_HU
  P[tr$random_state, tr$health_states] = tr$p_HD
  if (tr$random_state2 > 0) {
    P[tr$random_state2, tr$random_state2] = 1 - tr$p_HU2 - tr$p_HD
    P[tr$random_state2, tr$random_state2 + 1] = tr$p_HU2
    P[tr$random_state2, tr$health_states] = tr$p_HD
  }
  rownames(P) = colnames(P) = c("PF", sprintf("P%d", 1:(tr$health_states - 2)), "D")
  P
}


# TODO: refund, aggregate fail level and initial payment are the three continuous params. 
#       Report all payment schemes given level for these.
# TODO: Continuous payment until condition is satisfied (a better alternative exists)

# Check contract input and set default values
Contract <- function(
    name = "",
    tot_payment = 0, # nuvärde av kostnader
    contract_length = 0, # kontraktslängd
    initial_payment = 0, # andel av totalkostnad som betalas före behandling, övrigt betalas över hela kontraktslängden
    cont_payment = 0, # kontinuerlig betalning, kan vara vektor, om > 0 går före initial payment o tot_payment
    cost_trend = 0, # kontinuerlig betalning, kan vara vektor, om > 0 går före initial payment o tot_payment
    irr = 0 , # företagets internränta
    refund = 0 , # andel återbetalning om behandling inte verkar
    start = 1 , # Starttillstånd
    end = 2 , # Tillstånd där behandling bedöms slutat verka, kontrakt upphör
    aggregate_failure = 0, # behandling upphör om andelen som börjat progredera överstiger denna nivå
    payment_scheme = NA , # alternativt kan betalningsprofil anges manuellt
    other_costs = NA , # kostnader utöver behandlingskostnaden
    plan = NA  # Används för att skilja olika typer av kontrakt
) {
  parameters <<- c(as.list(environment()))
  structure(parameters, class = "Contract")
}

# Create payment vector for contract
contract_payments <- function(c) {
  if (c$initial_payment > 0) {
    payment = rep(c$tot_payment*(1- c$initial_payment)/(c$contract_length - 1), c$contract_length)
    payment[1] = c$tot_payment*(c$initial_payment)
  } else {
    payment = rep(c$tot_payment/c$contract_length, c$contract_length)
  }
  payment
}

# TODO: Find general way to transform for plots over time
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
    costs[1:length(payment)] = payment 
  } else {
    # Continuous payment, with possibly a trend in costs
    costs[1:T] = con$cont_payment *
      discounting(con$cost_trend, T) 
  }
  costs / # Future payments increase due to firm_discount
    discounting(globals$firm_discount, T) 
}

payment_plans <- function(indata) {
  globals = get_globals(indata$global_table)
  
  x = pmap(indata$contract_table, Contract)
  y = map(x, ~ payment_plan(.x, globals) )
  names(y) = x %>% map( "name") 
  res = y %>% 
    as_tibble() %>% 
    mutate(time = row_number()) %>% 
    pivot_longer(-time, names_to = "payment_plan", values_to = "payment")
  res
}

analyse_contract <- function(con, tr, globals) {
  
  states = expected_markov(tr, globals$time_horizon)
  
  # failing indicates share that enter failing state. Note that death without 
  # entering state is not included. Refunds are reasonable if the treatment 
  # does not work, but not if death is unrelated to illness.
  # if (con$end <= 2) {
  #   failing = states[,1:(con$end-1)]  *  tr$P[1:(con$end-1), con$end]
  # } else {
  #   failing = states[,1:(con$end-1)] %*%  tr$P[1:(con$end-1), con$end]
  # }
  # failing = cumsum(c(0, failing[-1]) ) # end is in the next period
 
  failing = states[, con$end] # end is in the next period
  
  # Costs are only defined for contract length
  costs = payment_plan(con, globals)
  costs = costs * discounting(globals$discount, globals$time_horizon)

  cumcosts = lag(cumsum(costs))
  if (con$contract_length < globals$time_horizon) {
    cumcosts[(con$contract_length + 1):length(cumcosts)] = 0
  }
  cumcosts[1] = 0
  
  reimburse = failing * con$refund * cumcosts
  
  # Health
  QALY = as.vector( states %*% create_QoL(tr)) * 
    discounting(globals$discount, globals$time_horizon)
  
  # Pay for progression free, not the dead
  if (con$end == 2) {
    payment_states = states[,1]
  } else {
    payment_states = rowSums(states[,con$start:(con$end-1)])
  }
  payment = payment_states * costs - reimburse
  
  # Pay for all survive (Can use the payment calcs, with end = 6)
  # survive = (1 - states[,ncol(states)])
  
  # Pay for everyone until failing share exceeds level
  # Should be pf
  cumfail = cumsum(failing)
  aggcond = ifelse(cumfail < con$aggregate_failure, 1.0, 0.0) 
  if (con$aggregate_failure > 0) {
    payment = aggcond * costs 
    # TODO: Repayment trivial in this case. All pay in full or 0.
  }
  
  tibble(time = 1:globals$time_horizon, QALY, failing, payment, costs, cumcosts, reimburse, cumfail, aggcond) 

}

# Summarise in separate function, to be able to study yearly paymnets 
summarise_contract <- function(con, tr, globals, over_time = FALSE) {
  if (over_time) {
    df = analyse_contract(con, tr, globals) %>% 
      select(
        time ,
        QALY , 
        Cost = payment
      ) 
  }
  else {
    df = analyse_contract(con, tr, globals) %>% 
      summarise(
        QALY = sum(QALY) , 
        Cost = sum(payment) ,
      )  
  }
  
  df %>% add_column(
      # name = sprintf("Hazard %.1f%%", round(tr$p_HU *100)) , 
      name = tr$name , 
      plan = con$plan ,
      contract = con$name ,
      .before = 1)
}

get_globals <- function(global_table) {
  globals = global_table %>% select(value) %>% transpose() %>% flatten()
  names(globals) = global_table  %>% select(name) %>% pull()
  globals
}

contract_analysis <- function(indata, active_plan = NA, control_plan = 0, 
                              over_time = FALSE, show_details = FALSE) 
{
  globals = get_globals(indata$global_table)
  if (is.na(active_plan)) {
    plans = indata$contract_table %>% select(plan) %>% distinct() %>% pull()
  } else {
    plans = c(active_plan, control_plan)
  }
  df = tibble()
  for (pl in plans) {
    treats = indata$treatment_table %>% filter(plan == pl) %>%  pmap( Treatment)
    cons = indata$contract_table %>% filter(plan == pl) %>% pmap( Contract)
    for (tr in treats) {
      is_first = TRUE
      for (con in cons) {
        dftr = summarise_contract(con, tr, globals, over_time)
        if(!is_first) {
          dftr$QALY = NA
        }
        is_first = FALSE
        df = bind_rows(df, dftr)
      }
    }
  }
  if (over_time) {
    df
  } else {
    # Only include first QALY estimate
    # TODO: Calculate QALY separately
    # df = df %>% group_by(name) %>% 
    #   mutate(QALY = ifelse( row_number() == 1, QALY, NA))
    if(show_details) {
      df
    } else {
      compare_results(df, 
                active_plan = plans[plans != control_plan], 
                control_plan = control_plan
      )
    }
  }
}

compare_results <- function(df, active_plans = 1, control_plan = 0) {
  res = tibble()
  for ( active_plan in active_plans) {
    
  df2 = df %>% 
    group_by(name, plan) %>% select(-contract) %>% 
    summarise_all(~sum(.x, na.rm = TRUE)) 
  
  cpl = df2 %>% filter(plan == control_plan) %>% select( -plan)  
  
  dfn  = cross_join(
    df2 %>% filter(plan == active_plan) %>% select( -plan) ,
    cpl
  ) %>% 
    transmute(
      Treatments = str_c(name.x, " - ", name.y) ,
      QALY = QALY.x - QALY.y ,
      Cost = Cost.x - Cost.y ,
      ICER = Cost / QALY
    )
  res = bind_rows(res, dfn)
  }
  res
} 

# Row numbers of all combinations
get_all_combinations <- function(table) {
  m = table %>% mutate(r = row_number()) %>% group_by(table_name, row_name, column_name) %>% summarise(from = min(r), to = max(r))
  selection = m %>% ungroup() %>% arrange(-from) %>% select(from, to) %>% pmap(seq) %>% expand.grid
  selection
}

compare_with_variations <- function(indata, mod_table , add_row_names = FALSE) {
  sel = get_all_combinations(mod_table)
  
  res = tibble()
  for (i in 1:nrow(sel)) {
    for (j in 1:ncol(sel)) {
      s = sel[[i,j]]
      indata = set_value(
        indata , 
        table_name = mod_table[[s, "table_name"]],
        row_name = mod_table[[s, "row_name"]],
        column_name = mod_table[[s, "column_name"]],
        value = mod_table[[s, "value"]]
      ) 
    }
    resi = contract_analysis(indata) 
    res = bind_rows(res, resi)
  }
  
  column_names = c()
  for (j in 1:ncol(sel)) {
    if (add_row_names) {
      column_name = paste(
        mod_table[[sel[[1,j]], "row_name"]] ,
        mod_table[[sel[[1,j]], "column_name"]] 
      )
    } else {
      column_name = mod_table[[sel[[1,j]], "column_name"]]
    }
    column_names = c(column_names, column_name)
    res = add_column(res, 
                     "{column_name}" := 0,
                     .before = 1)
  }
  # row_name = mod_table[[s, "row_name"]]
  for (i in 1:nrow(sel)) {
    for (j in 1:ncol(sel)) {
      res[[i, column_names[j] ]] = mod_table[[sel[[i,j]], "value"]]
      # res[[i, mod_table[[sel[[1,j]], "column_name"]] ]] = mod_table[[sel[[i,j]], "value"]]
    }
  }
  res
}

lightblue = grDevices::rgb(128, 198, 212, maxColorValue = 255)
