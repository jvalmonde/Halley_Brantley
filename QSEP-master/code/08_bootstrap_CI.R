################################################################################
# QSEP Bootstrap Predictions
# Halley Brantley
################################################################################
library(savvy)
library(tidyverse)
library(modelr)
library(gam)
library(boot)

rm(list = ls())
load("../data/adult_model_data.Rdata")
set.seed(98765)
################################################################################
adult_model_data$MM <- factor(adult_model_data$MM, 
                              levels = c(12, 4, 5, 6, 7, 8, 9, 10, 11))

getPreds <- function(IDs, i, diag_data, pred_data, formula_gam, 
                     formula_logNorm) {
  if(nrow(IDs) > 500){
    IDs <- IDs[1:500,, drop=FALSE]
  }
  
  ## join the sampled firms to their firm-year data
  suppressMessages(dat <- left_join(IDs[i,,drop=FALSE], diag_data))
  logistic_model <- gam(formula_gam, family = binomial, data = dat)  
  lognormal_model <- lm(formula_logNorm, data = dat[dat$positive_spend == 1, ])
  
  phat <- predict(logistic_model, newdata = pred_data, type = "response")
  yhat <- exp(predict(lognormal_model, newdata = pred_data))
  
  preds <- yhat*phat
  ## Run regression with and without firm fixed effects
  return(preds)
}

fit_models <- function(diag_data, flag, diag, Month_diag, after_diag){
  predictors <- c("RAF_cat",
                  "IncomeCat",
                  "ns(Age, 3)",
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
  
  numMonths <- diag_data %>% group_by(Indv_Sys_Id) %>% summarise(ct = n()) 
  table(numMonths$ct)
  n_distinct(diag_data$Indv_Sys_Id)
  
  IDs <- data.frame(Indv_Sys_Id = unique(diag_data$Indv_Sys_Id))
  
  keep_RAF_cat <- diag_data %>%
    select(Indv_Sys_Id, RAF_cat) %>%
    group_by(RAF_cat) %>%
    summarise(
      ct = n_distinct(Indv_Sys_Id)
    ) %>%
    filter(ct >= 20) %>%
    select(RAF_cat)
  
  keep_IncomeCat <- diag_data %>%
    select(Indv_Sys_Id, IncomeCat) %>%
    group_by(IncomeCat) %>%
    summarise(
      ct = n_distinct(Indv_Sys_Id)
    ) %>%
    filter(ct >= 20) %>%
    select(IncomeCat) 
  
  dat <- diag_data
  logistic_model <- gam(formula_gam, family = binomial, data = dat)  
  lognormal_model <- lm(formula_logNorm, data = dat[dat$positive_spend == 1, ])
  
  pred_data <- diag_data %>%
    data_grid(., 
              !!after_diag, youngest_child,
              Age = seq(20, 55, 5), 
              RAF_cat, IncomeCat,  single_adult, 
              .model = lognormal_model) %>%
    filter(Age >= 30 | youngest_child != "Teen") %>%
    inner_join(keep_RAF_cat) %>%
    inner_join(keep_IncomeCat)
  
  # Get bootstrap predictions
  b <- boot(IDs, getPreds, 100, diag_data = diag_data, pred_data = pred_data, 
            formula_gam = formula_gam, formula_logNorm = formula_logNorm)
  
  # Add predictions to predictor combinations
  pred_agg <- bind_cols(pred_data, as.data.frame(t(b$t))) 
  
  after_samples <- pred_agg %>% filter(!!after_diag == 1)
  before_samples <- pred_agg %>% filter(!!after_diag == 0) %>% 
    select(starts_with("V"))
  
  diff_samples <- after_samples
  samp_cols <- grep("V", names(diff_samples))
  diff_samples[, samp_cols] <- after_samples[,samp_cols] - before_samples
  
  diff_long <- diff_samples %>% 
    gather(key = "b", value = "pred_diff", V1:V100)
  
  CI_table <- diff_long %>% 
    summarise(
      mean_b = mean(pred_diff), 
      lb_b = quantile(pred_diff, 0.05), 
      ub_b = quantile(pred_diff, 0.95), 
      ct = n()
    )
  
  diff_summ <- 
    diff_long %>% 
    group_by(RAF_cat, Age) %>%
    summarise(
      mean_b = mean(pred_diff), 
      lb_b = quantile(pred_diff, 0.05), 
      ub_b = quantile(pred_diff, 0.95), 
      ct = n()
    ) 
  
  p <- diff_summ %>% filter(Age < 50) %>%  
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
    labs(col = "Youngest Child",
         y = "Difference in Average Monthly Spend ($)", 
         x = "Parent Age",
         title = "Predicted Difference in Average Monthly Spend After Child's Diagnosis",
         subtitle = "90% CI by RAF category and Age") 
  
  return(list(plot = p,
              CI_table = CI_table,
              difference_summary = diff_summ,
              boot_samples = pred_agg,
              lognormal_model = lognormal_model, 
              logistic_model = logistic_model
  ))
}

boot_pred <- function(flag, diag, Month_diag, after_diag, adult_model_data){
  
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  after_diag <- enquo(after_diag)
  
  # Clean Data
  mod_data <- adult_model_data
  
  flagged_data <- mod_data %>%
    filter(!!flag == 1) 
  
  n_distinct(flagged_data$Indv_Sys_Id)
  
  # Determine minimum and maximum number of months before and after diagnosis
  min_diag <- flagged_data %>% 
    group_by(Indv_Sys_Id) %>%
    filter(between(!!Month_diag, -6, 6)) %>%
    summarise(min_month = min(!!Month_diag), 
              max_month = max(!!Month_diag)) 
  
  min_diag_keep <- min_diag %>% filter(min_month < -2 & max_month >  1)
  min_diag_keep %>%  select(Indv_Sys_Id) %>% n_distinct()
  
  diag_data <- inner_join(flagged_data, min_diag_keep) %>% 
    filter(between(!!Month_diag, -9, 8))
  
  male_data <- diag_data %>% filter(male == 1)
  female_data <- diag_data %>% filter(male == 0)
  
  male_fit <- fit_models(male_data, flag, diag, Month_diag, after_diag)
  female_fit <- fit_models(female_data, flag, diag, Month_diag, after_diag)
  
  return(list(male = male_fit, 
              female = female_fit))
}

getCI_table <- function(fit, diag){
  male_CI <- fit$male$CI_table
  male_CI$Gender <- "male" 
  female_CI <- fit$female$CI_table
  female_CI$Gender <- "female"
  
  CIs <- bind_rows(male_CI, female_CI)
  CIs$condition <- diag
  return(CIs)
}

t1d_fit <- boot_pred(T1D_Flag, "T1D", Month_IND_T1D, After_T1D, 
                     adult_model_data)


cerebral_fit <- boot_pred(Cerebral_Flag, "Cerebral", 
          Month_IND_Cerebral, After_Cerebral, adult_model_data)

asd_fit <- boot_pred(ASD_Flag, "ASD", Month_IND_ASD, After_ASD, 
                     adult_model_data)
save(asd_fit, file = "../data/asd_fit.Rdata")

cancer_fit <- boot_pred(CA_Flag, "CA", Month_IND_CA, After_CA, 
                     adult_model_data)
save(cancer_fit, file = "../data/cancer_fit.Rdata")

trauma_fit <- boot_pred(Trauma_Flag, "Trauma", Month_IND_Trauma, 
                        After_Trauma, 
                     adult_model_data)
save(trauma_fit, file = "../data/trauma_fit.Rdata")

asthma_fit <- boot_pred(Asthma_Flag, "Asthma", Month_IND_Asthma, 
                        After_Asthma, 
                        adult_model_data)
save(asthma_fit, file = "../data/asthma_fit.Rdata")

CIs <- bind_rows(
  getCI_table(cerebral_fit, "Cerebral"), 
  getCI_table(t1d_fit, "T1D"), 
  getCI_table(cancer_fit, "Cancer"),
  getCI_table(trauma_fit, "Trauma"),
  getCI_table(asd_fit, "ASD"))

# getCI_table(asthma_fit, "Asthma"),

CIs <- CIs %>%
  mutate_at(vars(mean_b:ub_b), round, digits = 2) %>%
  select(condition, Gender, lb_b, mean_b, ub_b) %>%
  arrange(lb_b)


t1d_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "T1D"),
       width = 10, height = 10)
t1d_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "T1D"),
       width = 10, height = 10)

asd_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "ASD"), 
       width = 10, height = 10)
asd_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "ASD"), 
       width = 10, height = 10)

cerebral_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "Cerebral"), 
       width = 10, height = 10)

cerebral_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "Cerebral"), 
       width = 10, height = 10)

cancer_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "CA"), 
       width = 10, height = 10)
cancer_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "CA"), 
       width = 10, height = 10)

trauma_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "Trauma"), 
       width = 10, height = 10)
trauma_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "Trauma"), 
       width = 10, height = 10)

asthma_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "Asthma"), 
       width = 10, height = 10)
asthma_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "Asthma"), 
       width = 10, height = 10)

CIs 
latex(CIs, 
      colheads = c("", "Gender", "LB", "Mean", "UB"),
      title = '', 
      file = "../tables/pred_CIs.tex", 
      na.blank = FALSE)  
