# New functions
# 


plot_QoL2 <- function(indata) {
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

load_data <- function(vals = list(), indata = list(), filename) {
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

