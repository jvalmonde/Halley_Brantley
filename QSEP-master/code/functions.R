################################################################################
# Functions for QSEP analysis
# Halley Brantley
################################################################################

# Subset data for a given diagnosis
get_diag_data <- function(flag, diag, Month_diag, after_diag, adult_model_data){
  
  # Variable names
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  after_diag <- enquo(after_diag)
  
  flagged_data <- adult_model_data %>%
    filter(!!flag == 1) 
  
  # Determine minimum and maximum number of months before and after diagnosis
  min_diag <- flagged_data %>% 
    group_by(Indv_Sys_Id) %>%
    filter(between(!!Month_diag, -6, 6)) %>%
    summarise(min_month = min(!!Month_diag), 
              max_month = max(!!Month_diag)) 
  
  min_diag_keep <- min_diag %>% filter(min_month < -2 & max_month >  1)
  
  diag_data <- 
    inner_join(flagged_data, min_diag_keep) %>% 
    filter(between(!!Month_diag, -9, 8))
  
  return(diag_data)
}


# Fit logistic and log-normal models
fit_models <- function(diag_data, diag){
  
  predictors <- c("RAF_cat",
                  "Income_cat",
                  "ns(Age, 3)",
                  "ns(sick_child_age,3)",
                  "youngest_child", 
                  "multiple_sick",
                  "num_children", "single_adult", 
                  "MM", "ns(time,3)", "plan_sub")
  
  formula_gam <- formula(paste("positive_spend ~", 
                               paste(c(sprintf("After_%s", diag), 
                                       predictors), collapse = "+")))
  
  formula_logNorm <- formula(paste("logSpend ~", 
                                   paste(c(sprintf("After_%s", diag), 
                                           predictors), collapse = "+")))
  
  
  logistic_model <- glm(formula_gam, family = binomial, data = diag_data)  
  lognormal_model <- lm(formula_logNorm, 
                        data = diag_data[diag_data$positive_spend == 1, ])
  
  return(list(logistic_model = logistic_model, 
              lognormal_model = lognormal_model))
}

# Fit models for a given diagnosis
get_diag_models <- function(diag, flag, Month_diag, after_diag, adult_model_data){
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  after_diag <- enquo(after_diag)
  
  diag_data <- get_diag_data(!!flag, diag, !!Month_diag, !!after_diag, 
                             adult_model_data)
  
  male_models <- fit_models(subset(diag_data, male == 1), diag)
  female_models <- fit_models(subset(diag_data, male == 0), diag)
  
  return(list(male = male_models, female = female_models))
}

# Calculate predicted spend (for use inside boot function)
getPreds <- function(IDs, i, diag, diag_data, pred_data) {
  if(nrow(IDs) > 500){
    IDs <- IDs[1:500,, drop=FALSE]
  }
  
  suppressMessages(dat <- left_join(IDs[i,,drop=FALSE], diag_data))
  models <- fit_models(diag_data, diag)
  
  phat <- predict(models$logistic_model, newdata = pred_data, type = "response")
  yhat <- exp(predict(models$lognormal_model, newdata = pred_data))
  
  preds <- yhat*phat

  return(preds)
}



bootstrap_results <- function(diag_data, flag, diag, Month_diag, after_diag){
  
  IDs <- data.frame(Indv_Sys_Id = unique(diag_data$Indv_Sys_Id))
  
  models <- fit_models(diag_data, diag)
  
  pred_data <- diag_data %>%
    data_grid(., 
              !!after_diag, youngest_child,
              Age = seq(25, 55, 10), 
              RAF_cat, Income_cat,  single_adult, 
              .model = models$lognormal_model) %>%
    filter(Age >= 30 | youngest_child != "Teen")
  
  # Get bootstrap predictions
  b <- boot(IDs, getPreds, 100, diag_data = diag_data, pred_data = pred_data, 
            diag = diag)
  
  # Add covariates to predictions 
  pred_agg <- bind_cols(pred_data, as.data.frame(t(b$t))) 
  
  after_samples <- pred_agg %>% filter(!!after_diag == 1)
  before_samples <- pred_agg %>% filter(!!after_diag == 0) %>%
    select(starts_with("V"))
  
  diff_samples <- after_samples
  samp_cols <- grep("V", names(diff_samples))
  diff_samples[, samp_cols] <- after_samples[,samp_cols] - before_samples
  
  diff_long <- diff_samples %>% 
    gather(key = "b", value = "pred_diff", starts_with("V"))
  
  CI_table <- diff_long %>% 
    group_by(RAF_cat) %>%
    summarise(
      mean_b = mean(pred_diff), 
      lb_b = quantile(pred_diff, 0.025), 
      ub_b = quantile(pred_diff, 0.975), 
      ct = n()
    )
  
  diff_summ <- 
    diff_long %>% 
    group_by(RAF_cat, Age) %>%
    summarise(
      mean_b = mean(pred_diff), 
      lb_b = quantile(pred_diff, 0.025), 
      ub_b = quantile(pred_diff, 0.975), 
      ct = n()
    ) 
  
  p <- diff_summ %>%  
    ggplot(aes(x=factor(Age), y = mean_b)) + 
    geom_point() +
    geom_errorbar(aes(ymin = lb_b, ymax = ub_b)) +
    facet_grid(RAF_cat~., scales = "free") + 
    geom_abline(slope = 0, intercept = 0, col = "red") + 
    theme_savvy(base_size = 16) + 
    geom_label(aes(label = round(mean_b)), 
               hjust = "middle", 
               vjust = "center", 
               nudge_x = .1) +
    labs(y = "Difference in Average Monthly Spend ($)", 
         x = "Parent Age",
         title = "Predicted Difference in Average Monthly Spend After Child's Diagnosis",
         subtitle = "95% CI by RAF category and Age") 
  
  return(list(plot = p,
              CI_table = CI_table,
              difference_summary = diff_summ,
              boot_samples = pred_agg
  ))
}


boot_pred <- function(flag, diag, Month_diag, after_diag, adult_model_data){
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  after_diag <- enquo(after_diag)
  
  diag_data <- get_diag_data(!!flag, diag, !!Month_diag, !!after_diag, 
                             adult_model_data)
  male_data <- diag_data %>% filter(male == 1)
  female_data <- diag_data %>% filter(male == 0)
  
  male_fit <- bootstrap_results(male_data, flag, diag, Month_diag, after_diag)
  female_fit <- bootstrap_results(female_data, flag, diag, Month_diag, after_diag)
  
  return(list(male = male_fit, 
              female = female_fit))
}

# Get confidence interval for predictions
getCI_table <- function(fit, diag){
  male_CI <- fit$male$CI_table
  male_CI$Gender <- "male" 
  female_CI <- fit$female$CI_table
  female_CI$Gender <- "female"
  
  CIs <- bind_rows(male_CI, female_CI)
  CIs$condition <- diag
  return(CIs)
}

# Logistic models for depression (fill a prescription for depression medication)
get_depress_model <- function(diag, flag, Month_diag, after_diag,
                              adult_model_data){
  
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  after_diag <- enquo(after_diag)
  
  diag_data <- get_diag_data(!!flag, diag, !!Month_diag, !!after_diag, 
                             adult_model_data)
  
  male_data <- diag_data %>% filter(male == 1)
  female_data <- diag_data %>% filter(male == 0)
  
  predictors <- c("RAF_cat",
                  "Income_cat",
                  "ns(Age, 3)",
                  "ns(sick_child_age,3)",
                  "youngest_child", 
                  "multiple_sick",
                  "num_children", "single_adult", 
                  "MM", "ns(time,3)", "plan_sub")
  
  formula_full <- formula(paste("Depression_Flag ~", 
                                paste(c(sprintf("After_%s", diag), 
                                        predictors), collapse = "+")))
  
  male_gam <- glm(formula_full, family = binomial, data = male_data)  
  female_gam <- glm(formula_full, family = binomial, data = female_data) 
  
  male_effect <-  
    tidy(male_gam) %>% 
    mutate(LB = estimate - 1.96*std.error, 
           UB = estimate + 1.96*std.error, 
           Condition = diag, 
           Gender = "male") %>%
    filter(term == paste0("After_", diag)) %>%
    select(Condition, Gender, LB, estimate, UB) %>% 
    mutate_at(vars(LB:UB), exp)
  
  female_effect <-  
    tidy(female_gam) %>% 
    mutate(LB = estimate - 1.96*std.error, 
           UB = estimate + 1.96*std.error, 
           Condition = diag, 
           Gender = "female") %>%
    filter(term == paste0("After_", diag)) %>%
    select(Condition, Gender, LB, estimate, UB) %>% 
    mutate_at(vars(LB:UB), exp)
  
  return(bind_rows(male_effect, female_effect))
}
