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

# Open all tables in Excelblad as list of tibbles, with list of errors 
# as a tibble
open_indata <- function(filename) {
  indata = list(
    treatment_table = read_excel(filename, sheet = "Treatments") ,
    contract_table = read_excel(filename, sheet = "Contracts") ,
    global_table = read_excel(filename, sheet = "Globals") ,
    treatment_description = read_excel(filename, sheet = "Treatment_fields") ,
    contract_description = read_excel(filename, sheet = "Contract_fields") ,
    state_table = read_excel(filename, sheet = "States") ,
    payment_table = read_excel(filename, sheet = "Payments") ,
    state_description = read_excel(filename, sheet = "State_fields") ,
    payment_description = read_excel(filename, sheet = "Payment_fields") 
  )
  # Data tables that do not exist in all example excel sheets
  if ("Global_fields" %in% excel_sheets(filename)) {
    indata$global_description = read_excel(filename, sheet = "Global_fields")
    # indata$state_table = read_excel(infile, sheet = "States")
  }
  indata$errors = check_indata(indata)
  indata
}

# Load data into reactiveValues object
load_data <- function(vals, indata, filename) {
    file_indata = open_indata(filename)
      # indata = open_indata(filename) does not work if indata is to be reactive
      indata$treatment_table = read_excel(filename, sheet = "Treatments") 
      indata$contract_table = read_excel(filename, sheet = "Contracts") 
      indata$global_table = read_excel(filename, sheet = "Globals") 
      indata$treatment_description = read_excel(filename, sheet = "Treatment_fields") 
      indata$payment_description = read_excel(filename, sheet = "Payment_fields") 
      indata$contract_description = read_excel(filename, sheet = "Contract_fields")
      indata$global_description = read_excel(filename, sheet = "Global_fields")
  
  indata$errors <- check_indata(indata)
    
  # DT::coerceValue wants a data.frame
  #       indata <- open_indata(input$upload$datapath)
  vals$treatment_table <- as.data.frame(file_indata$treatment_table)
  vals$contract_table <- as.data.frame(file_indata$contract_table)
  vals$global_table <- as.data.frame(file_indata$global_table)
  vals$treatment_description <- as.data.frame(file_indata$treatment_description)
  vals$contract_description <- as.data.frame(file_indata$contract_description)
  vals$errors <- file_indata$errors
  
  # vals$treatment_table <- indata$treatment_table
  # vals$contract_table <- indata$contract_table
  indata
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

plot_QoL <- function(health_analysis) {
  
  QoL_table <- function(tr) {
    tibble(name = tr$name, state = 1:tr$health_states, QoL = create_QoL(tr))
  }
  
  ex_treatment = health_analysis$treatment_table %>% 
    pmap(Treatment) %>% map(QoL_table) %>% bind_rows()
  ex_treatment %>% ggplot() +
    aes(state, QoL) + 
    geom_col(fill = lightblue) + 
    # coord_flip() + 
    facet_grid(rows = vars(name)) +
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
    P[tr$random_state2, tr$random_state2] = 1 - tr$p_HU2 - tr$p_UD
    P[tr$random_state2, tr$random_state2 + 1] = tr$p_HU2
    P[tr$random_state2, tr$health_states] = tr$p_UD
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

# Put global_table in a list globals
get_globals <- function(global_table) {
  globals = global_table %>% select(value) %>% transpose() %>% flatten()
  names(globals) = global_table  %>% select(name) %>% pull()
  globals
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

# Get payments for all payment plans. Used in displaying model inputs
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
  
  states = expected_markov( transition( tr), globals$time_horizon)
  
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
  
  # Costs are only defined for contract length, discounted for both firm and HA
  costs = payment_plan(con, globals)

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
