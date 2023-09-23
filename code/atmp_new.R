# New functions
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
  P = matrix(0, health_states, health_states)
  for (i in 1:(health_states-1)) {
    P[i, i] = 1 - state_table$pr.prog[i] 
    P[i, i + 1] = state_table$pr.prog[i] - state_table$p_death[i]
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

create_payment_plans <- function(state_table, payment_table, globals) {
  cons = state_table %>% 
    left_join(payment_table) %>% 
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

analyse_treatment <- function(state_table, payment_table, globals) {
  
  P = transition_matrix(state_table) 
  
  states = expected_markov( P, globals$time_horizon)
  
  # CALCULATE COSTS AND QALY
  costs =  create_payment_plans(state_table, payment_table, globals) * states
  
  QALY = as.vector( states %*% get_QoL(state_table)) * 
    discounting(globals$discount, globals$time_horizon)
  
  # Put into dataset and sum costs over states 
  ret = tibble()
  costs_df = costs %>% 
    as_tibble() %>% 
    mutate(time = row_number()) %>% 
    pivot_longer(-time, names_to = "start", values_to = "value", names_prefix = "S", names_transform = as.integer) %>% 
    left_join(state_table %>% select(start, costben = payment)) %>% 
    filter(!is.na(costben)) %>% 
    group_by(time, costben) %>% summarise(value = sum(value))
  
  ret = bind_rows(ret,
                  bind_rows(costs_df, tibble(time = 1:length(QALY), costben = "QALY", value = QALY) ) %>% 
                    add_column( treatment = state_table$treatment[[1]], .before = 1)
  )
  ret
}

analyse_treatments <- function(indata, over_time = FALSE) {
  globals = named_list(indata$global_table, "name", "value")
  
  state_table_all = create_state_table(indata$state_table) 
  
  treats = state_table_all %>% distinct(treatment) %>% pull()
  ret = tibble()
  for (i in 1:length(treats)) {
    state_table_i = state_table_all %>% filter(treatment == treats[i])
    ret = bind_rows(ret, analyse_treatment(state_table_i, indata$payment_table, globals))
  }
  if (!over_time) {
    ret %>% group_by(treatment, costben) %>% 
      summarise(value = sum(value))
  } else {
    ret
  }
}


