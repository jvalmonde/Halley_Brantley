
DX_levels <- 
  c("A_Fib",
    "Alcohol_Abuse", 
    "Alzheimers_Dementia", 
    "Anxiety",
    "Arthritis",
    "Asthma",
    "Cancer_Breast",
    "Cancer_CRC",
    "Cancer_Lung",
    "Cancer_Prostate",
    "CKD", 
    "ESRD",
    "COPD", 
    "Depression", 
    "Diabetes",
    "Drug_Abuse", 
    "Heart_Failure", 
    "HTN",
    "Hyperlipidemia", 
    "Ischemic_Heart_Disease", 
    "Osteoporosis")

DX_labels <- 
  c("A. Fib.",
    "Alcohol Abuse", 
    "Alzheimer's or Dementia", 
    "Anxiety",
    "Arthritis \n (RA or \n Osteoarthritis)",
    "Asthma" ,
    "Cancer (Breast)",
    "Cancer (CRC)",
    "Cancer (Lung)",
    "Cancer (Prostate)",
    "CKD" ,
    "ESRD",
    "COPD" , 
    "Depression" , 
    "Diabetes" ,
    "Drug Abuse" , 
    "Heart Failure" , 
    "Hypertension" , 
    "Hyperlipidemia" , 
    "Ischemic Heart Disease" , 
    "Osteoporosis" )

DX_labels_short <- 
  c("A. Fib.",
    "Alcohol Abuse", 
    "Alzheimer's", 
    "Anxiety",
    "Arthritis",
    "Asthma" ,     
    "Breast Cancer",
    "CRC",
    "Lung Cancer",
    "Prostate Cancer",
    "CKD" ,
    "ESRD",
    "COPD" , 
    "Depression" , 
    "Diabetes" ,
    "Drug Abuse" , 
    "Heart Failure" , 
    "Hypertension" , 
    "Hyperlipidemia" , 
    "IHD", 
    "Osteoporosis")

# Transforms count matrices from wide to long format for plotting
wide_to_long <- function(data_mat, col_label){
  col_label <- enquo(col_label)
  data_mat %>%
    as.data.frame() %>%
    mutate(Condition1 = row.names(.)) %>%
    gather("Condition2",  !!col_label, -Condition1) %>%
    mutate(Condition1 = factor(str_remove(Condition1, "DX_"), 
                               levels = DX_levels, labels = DX_labels_short),
           Condition2 = factor(str_remove(Condition2, "DX_"), 
                               levels = DX_levels, labels = DX_labels_short))
}

# Calculates PMPM for cost variables
get_costs <- function(diag_mat, dyad_ct, n_months, cost_var){
  costs <-  as.matrix(crossprod(diag_mat, 
                                Diagonal(x=cost_var)
                                         %*% diag_mat))
  pmpm <- (costs/dyad_ct)/n_months
}


# Creates cost plots
make_plot <- function(input, plot_df, cost_var, title_text){
  cost_var <- enquo(cost_var)
  cost_plot <- plot_df %>%
    filter(Condition1 %in% input$condition, Condition2 %in% input$condition) %>%
    ggplot(aes(x=Condition1, y=Condition2)) + 
    geom_tile(aes(fill = !!cost_var)) +
    scale_fill_distiller(palette="Spectral") + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
    labs(x = NULL, y = NULL, fill = "$ pmpm",
         title = title_text)
  
  if (input$show_labels){
    cost_plot + geom_label(aes(label = comma(!!cost_var))) 
  } else {
    cost_plot
  }
}


get_dyads <- function(mod_df, n_months){
  
  diags <-  
    mod_df %>% 
    select(paste0("DX_", DX_levels)) %>%
    as.matrix(.) 
  
  dyad_ct <- crossprod(diags)
  
  cases <-  
    mod_df %>% 
    filter(CASE_INT == 1) %>%
    select(paste0("DX_", DX_levels)) %>%
    as.matrix(.) %>% 
    crossprod(.)
  
  dyad_freq <- wide_to_long(dyad_ct, Counts) 
  case_freq <- wide_to_long(cases, Case_Counts)
  
  # Cost types
  pmpm <- 
    get_costs(diags, dyad_ct, n_months, mod_df$TOTAL_COST2) %>%
    wide_to_long(PMPM)
  ip <- 
    get_costs(diags, dyad_ct, n_months, mod_df$Inpatient) %>%
    wide_to_long(IP)
  op <- 
    get_costs(diags, dyad_ct, n_months, mod_df$Outpatient) %>%
    wide_to_long(OP)
  dr <- 
    get_costs(diags, dyad_ct, n_months, mod_df$DR) %>%
    wide_to_long(DR)
  er <- 
    get_costs(diags, dyad_ct, n_months, mod_df$ER) %>%
    wide_to_long(ER)
  

  # Join dataframes  
  dyads <- 
    dyad_freq %>% 
    inner_join(case_freq) %>% 
    inner_join(pmpm) %>%
    inner_join(ip) %>%
    inner_join(op) %>%
    inner_join(dr) %>%
    inner_join(er)
  
  fct_levels <- dyads %>% group_by(Condition1) %>%
    summarise(ct = max(Counts)) %>% 
    arrange(ct) %>%
    mutate(Condition1 = as.character(Condition1)) %>%
    .[[1]]
    
  dyads %<>% mutate(Condition1 = fct_relevel(Condition1, fct_levels), 
                    Condition2 = fct_relevel(Condition2, fct_levels),
                    Frequency = Counts/nrow(mod_df),
                    Case_Frequency = Case_Counts/Counts)
  
  return(dyads)
}



