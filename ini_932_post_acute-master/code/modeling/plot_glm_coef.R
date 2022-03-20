
# PLOT FOR GLM AND NB MODELS ----------------------------------------------

# defaults to plotting only significant variables

plot_glm_coef <- function(mod_fit, signif.only = T, values = 'all', cutoff = NULL,
                          top_filter = FALSE, top_n = 15, top_order = NULL, ...){
  
  if(is.na(signif.only))
    signif.only <-  T
  
  if(is.na(values))
    values <- 'all'
  
  coef_smry <- summary(mod_fit)$coefficient
  
  # keep significant variables, does not include intercept value
  vars <- if(signif.only == T){
    rownames(coef_smry)[which(coef_smry[,4] < 0.05)][-1]
  } else {
    rownames(coef_smry)[-1]}
  
  top_vars <- 
    if (top_filter == TRUE){
      if(top_order == 'desc'){
        cbind(vars,coef_smry[which(rownames(coef_smry) %in% vars)]) %>% as.data.frame() %>% 
          mutate(abs_effect = abs(as.numeric(as.character(V2)))) %>% 
          arrange(desc(abs_effect)) %>% head(top_n) 
      } else {
        cbind(vars,coef_smry[which(rownames(coef_smry) %in% vars)]) %>% as.data.frame()  %>% 
          mutate(abs_effect = abs(as.numeric(as.character(V2)))) %>% 
          arrange(desc(abs_effect)) %>% tail(top_n) 
      }
      } else {
        cbind(vars,coef_smry[which(rownames(coef_smry) %in% vars)]) %>% as.data.frame()
    }
  
  # add rownames of categorical variables that are not all significant

  # coefficient table
  coef_tab <- if(is.null(cutoff)){
    
    tibble(var = vars,
           chg = (exp(coef_smry[which(rownames(coef_smry) %in% vars),1]) - 1)*100,
           low_ci = (exp(coef_smry[vars, 1] - (1.96*coef_smry[vars, 2])) - 1)*100,
           upper_ci = (exp(coef_smry[vars, 1] + (1.96*coef_smry[vars, 2])) - 1)*100) 
    
    } else {
      
      tibble(var = vars,
             chg = (exp(coef_smry[which(rownames(coef_smry) %in% vars),1]) - 1)*100,
             low_ci = (exp(coef_smry[vars, 1] - (1.96*coef_smry[vars, 2])) - 1)*100,
             upper_ci = (exp(coef_smry[vars, 1] + (1.96*coef_smry[vars, 2])) - 1)*100) %>% 
        filter(chg >= cutoff)
        
    }
  
  # plot estimates with CI
  plot_df <- switch(values,
                    positive = coef_tab[coef_tab$chg > 1,],
                    negative = coef_tab[coef_tab$chg < 1,],
                    all = coef_tab)
  
  plot_df %>% filter(var %in% top_vars$vars) %>% 
    ggplot(aes(x = reorder(var, chg), y = chg)) +
    geom_errorbar(aes(ymin = low_ci, ymax = upper_ci), col = '#4285f4')+
    geom_point(col = 'grey35') +
    geom_hline(yintercept = 0, col = '#fcaf03') +
    coord_flip() + 
    labs(x = '', y = "Percentage Change in Expected Admission Counts") + 
    theme_bw()
}

  

# with labels -------------------------------------------------------------

plot_glm_coef_labs <- function(mod_fit, signif.only = T, cutoff = 'all', ...){
  
  if(is.na(signif.only))
    signif.only <-  T
  
  if(is.na(cutoff))
    cutoff <- 'all'
  
  coef_smry <- summary(mod_fit)$coefficient
  
  # keep significant variables, does not include intercept value
  vars <- if(signif.only == T){
    rownames(coef_smry)[which(coef_smry[,4] < 0.05)][-1]
  } else {
    rownames(coef_smry)[-1]}
  
  # creating labels for plotting
  labels <- full_join(data.frame(drg_col_name = vars),
                      filter(drg_mapping, drg_col_name %in% vars)) %>% 
    mutate(drg_desc2 = ifelse(is.na(drg_desc), drg_col_name, drg_desc))
  
  
  # coefficient table
  coef_tab <- tibble(labs = labels$drg_desc2,
                     chg = exp(coef_smry[which(rownames(coef_smry) %in% vars),1]),
                     low_ci = exp(coef_smry[vars, 1] - (1.96*coef_smry[vars, 2])),
                     upper_ci = exp(coef_smry[vars, 1] + (1.96*coef_smry[vars, 2])))
  
  # plot estimates with CI
  plot_df <- switch(cutoff,
                    positive = coef_tab[coef_tab$chg > 1,],
                    negative = coef_tab[coef_tab$chg < 1,],
                    all = coef_tab)
  
  plot_df %>%
    ggplot(aes(x = reorder(labs, chg), y = chg)) +
    geom_errorbar(aes(ymin = low_ci, ymax = upper_ci), col = '#4285f4')+
    geom_point(col = 'grey35') +
    geom_hline(yintercept = 1, col = '#fcaf03') +
    coord_flip() + 
    labs(x = 'Model Coefficient', y = "") + 
    theme_bw()
}

# PLOT FOR ZINB MODELS ----------------------------------------------------

plot_zinb_coef <- function(mod_fit, signif.only = T, cutoff = 'all', ...){
  
  if(is.na(signif.only))
    signif.only <-  T
  
  if(is.na(cutoff))
    cutoff <- 'all'
  
  ## count component
  count_coef <- summary(mod_fit)$coefficient$count
  
  ## zero component
  zero_coef <- summary(mod_fit)$coefficient$zero
  
  
  # keep significant variables, does not include intercept value
  signif_vars <- if(signif.only == T){
    count_vars = rownames(count_coef)[which(count_coef[,4] < 0.05 & 
                                              !rownames(count_coef) %in% 'Log(theta)')][-1] 
    zero_vars <- rownames(zero_coef)[which(zero_coef[,4] < 0.05 & 
                                             !rownames(zero_coef) %in% 'Log(theta)')][-1]
    list(count_vars, zero_vars)
  } else {
    count_vars <- rownames(count_coef)[which(!rownames(count_coef) %in% 'Log(theta)')][-1]
    zero_vars <- rownames(zero_coef)[which(!rownames(zero_coef) %in% 'Log(theta)')][-1]
    list(count_vars, zero_vars)
  }
  
  # add rownames of categorical variables that are not all significant
  
  # Count model component ---------------------------------------------------
  # coefficient table
  count_coef_tab <- tibble(vars = signif_vars[[1]],
                           chg = exp(count_coef[which(rownames(count_coef) %in% signif_vars[[1]]),1]),
                           low_ci = exp(count_coef[signif_vars[[1]], 1] - (1.96*count_coef[signif_vars[[1]], 2])),
                           upper_ci = exp(count_coef[signif_vars[[1]], 1] + (1.96*count_coef[signif_vars[[1]], 2])))
  
  # plot estimates with CI
  plot_df <- switch(cutoff,
                    positive = count_coef_tab[count_coef_tab$chg > 1,],
                    negative = count_coef_tab[count_coef_tab$chg < 1,],
                    all = count_coef_tab)
  
  count_plot <- plot_df %>%
    ggplot(aes(x = reorder(vars, chg), y = chg)) +
    geom_errorbar(aes(ymin = low_ci, ymax = upper_ci), col = '#4285f4')+
    geom_point(col = 'grey35') +
    geom_hline(yintercept = 1, col = '#fcaf03') +
    coord_flip() + 
    labs(x = 'Model Coefficient', y = "",
         title = 'Count Model') + 
    theme_bw()
  
  
  # zero inflation component ------------------------------------------------
  
  # coefficient table
  zero_coef_tab <- tibble(vars = signif_vars[[2]],
                          chg = exp(zero_coef[which(rownames(zero_coef) %in% signif_vars[[2]]),1]),
                          low_ci = exp(zero_coef[signif_vars[[2]], 1] - (1.96*zero_coef[signif_vars[[2]], 2])),
                          upper_ci = exp(zero_coef[signif_vars[[2]], 1] + (1.96*zero_coef[signif_vars[[2]], 2])))
  
  # plot estimates with CI
  zero_plot_df <- switch(cutoff,
                         positive = zero_coef_tab[zero_coef_tab$chg > 1,],
                         negative = zro_coef_tab[zero_coef_tab$chg < 1,],
                         all = zero_coef_tab)
  
  zero_plot <- zero_plot_df %>%
    ggplot(aes(x = reorder(vars, chg), y = chg)) +
    geom_errorbar(aes(ymin = low_ci, ymax = upper_ci), col = '#4285f4')+
    geom_point(col = 'grey35') +
    geom_hline(yintercept = 1, col = '#fcaf03') +
    coord_flip() + 
    labs(x = 'Model Coefficient', y = "",
         title = 'Zero Inflation Model') + 
    theme_bw()

  
  return(list(count_plot, zero_plot))
}

