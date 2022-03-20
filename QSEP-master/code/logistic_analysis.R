################################################################################
# QSEP Exploratory Analysis - Asthma
# Halley Brantley
################################################################################
library(savvy)
library(rlang)
library(tidyverse)
library(modelr)
library(broom)
library(lmtest)
library(gam)
library(pROC)
rm(list=ls())
load("../data/QSEP.RData")
rm(distToPCP)
################################################################################
set.seed(23456)
diag <- "T1D"
illness_flag <- quo(T1D_Flag)
month_diag <- quo(Month_IND_T1D)

none_before_diag <-  spend %>% 
  filter(!!illness_flag == 1, SickChild_Flag == 1, Age < 16) %>%
  group_by(Indv_Sys_Id) %>%
  summarise(minDiag = min(!!month_diag)) %>% 
  filter(minDiag > -2)

family_sample <- members %>%
  anti_join(none_before_diag) %>%
  filter(!!illness_flag == 1, SickChild_Flag == 1, Age < 16) %>%
  dplyr::select(FamilyID) %>%
  unique() 

members <-
  members %>% 
  filter(FamilyID %in% family_sample[,1]) %>%
  mutate_at(vars(Same_HHwSC, RAF_2014), funs(as.numeric))

spend <- spend %>% filter(FamilyID %in% family_sample[,1])

sick_child_spend <- 
  spend %>% 
  filter(SickChild_Flag == 1,
         Age < 16, 
         !!illness_flag == 1) 

sick_child_spend %>% 
  group_by(FamilyID) %>%
  summarise(minDiag = min(!!month_diag)) %>% 
  filter(minDiag > -2)
  
sick_child_age <- 
  sick_child_spend %>%
  group_by(FamilyID) %>%
  summarise(
    sick_child_age = min(Age)
  )

adult_family_age <- 
  spend %>%
  filter(SickChild_Flag == 0, Age  > 16)%>%
  group_by(FamilyID) %>%
  summarise(
    max_adult_age = max(Age)
  )

adult_family_spend <- 
  members %>%
  dplyr::select(Indv_Sys_Id, Year, MM, Same_HHwSC, RAF_2014, Gdr_Cd, 
         MedianHouseholdIncome, IncomeCat, Zip) %>%
  right_join(spend) %>%
  filter(SickChild_Flag == 0, 
         Age >= 16, 
         Same_HHwSC == 1) %>%
  left_join(sick_child_age) %>%
  left_join(adult_family_age) %>%
  mutate(Age_diff_adult = abs(Age - max_adult_age), 
         Age_diff_child = Age - sick_child_age) %>%
  filter(Age_diff_adult < 14 & Age_diff_child > 14 & Age_diff_child < 60) 

numHouse <- adult_family_spend %>%
  dplyr::select(Indv_Sys_Id, FamilyID) %>%
  distinct() %>%
  group_by(FamilyID) %>%
  summarise(
    adult_ct = n()
  )

table(numHouse$adult_ct)

numChild <- members %>%
  filter(Age < 16, Same_HHwSC == 1) %>%
  dplyr::select(Indv_Sys_Id, FamilyID) %>%
  distinct() %>%
  group_by(FamilyID) %>%
  summarise(child_ct = n())
         
adult_model_data <- 
  family %>%
  dplyr::select(FamilyID, Year, plan_type, FamilySize, ChildrenCnt) %>%
  right_join(adult_family_spend) %>%
  filter(!!month_diag > -18 & !!month_diag < 18) %>%
  left_join(numHouse) %>%
  left_join(numChild) %>%
  mutate(After_diag = ifelse(!!month_diag > -1, 1, 0), 
         male = case_when(Gdr_Cd == "M" ~ 1, 
                          Gdr_Cd == "F" ~ 0, 
                          TRUE ~ NA_real_),
         positive_spend = as.numeric(Total_Allow > 1), 
         single_adult = as.numeric(adult_ct == 1),
         other_child = as.numeric(child_ct > 1),
         time = as.numeric(as.factor(Year_Mo)), 
         plan_sub = fct_relevel(fct_lump(plan_type, 4), "Other"))


  
predictors <- c("After_diag", "After_diag:male", "male", 
                "s(RAF_2014)", "male:s(RAF_2014)", "s(MedianHouseholdIncome)", 
                 "single_adult", 
                  
                "single_adult:other_child:male",
                "single_adult:male",
                 "plan_sub")
                

adult_logistic <- adult_model_data %>%
  dplyr::select(positive_spend, After_diag, male, RAF_2014, Age,
         MedianHouseholdIncome, sick_child_age, 
         single_adult, other_child, time, plan_sub) %>%
  na.omit() 

adult_lognormal <- adult_model_data %>% 
  filter(positive_spend == 1) %>%
  dplyr::select(Total_Allow, After_diag, male, RAF_2014, Age,
                MedianHouseholdIncome, sick_child_age, 
                single_adult, other_child, time, plan_sub) %>%
  mutate(logSpend = log(Total_Allow)) %>%
  na.omit()

rm(spend, family, sick_child_spend, members)

formula_full <- paste("positive_spend ~", paste(predictors, collapse = "+"))
formula_sub <- paste("positive_spend ~", 
                     paste(predictors[3:length(predictors)], collapse = "+"))
###############################################################################
model_full <- function(df, formula_text){
  gam(formula(formula_text), data = df, family = binomial)
}

fn_misclass <- function(pred, resp){
  # pred is given as log odds
  # resp is given as T/F 
  is_misclass <- (sign(pred) == 1) != resp
  
}

get_auc <- function(pred, resp){
  pHat <- exp(pred)/(1+exp(pred))
  roc_obj <- roc(resp, pHat)
  return(as.numeric(roc_obj$auc))
}

sample_prediction <- function(model, sample){
  df <- as.data.frame(sample)
  pred <- stats::predict(model, df)
  pred
}


sample_response <- function(model, sample){
  df <- as.data.frame(sample)
  var_response <- all.vars(formula(model))[[1]]
  df[[var_response]]
}
###############################################################################

# adult_split_logit <- 
#   adult_logistic %>%
#   crossv_mc(n=10, test = .3) 
# 
# adult_model_logit <- 
#   adult_split_logit %>%
#   mutate(model = map(train, model_full, formula_text = formula_full))
# 
# adult_logit_resid <- 
#   adult_model_logit %>%
#   gather(key = split, value = data, train, test) %>%
#   mutate(
#     pred = map2(model, data, sample_prediction),
#     resp = map2(model, data, sample_response),
#     is_misclass = map2(pred, resp, fn_misclass)
#   ) %>%
#   dplyr::select(.id, split, pred, resp, is_misclass) %>%
#   unnest() 
# 
# cv_stats <- 
#   adult_logit_resid %>%
#   group_by(.id, split) %>%
#   summarise(
#     misclass_rate = sum(is_misclass)/n()
#     #auc = get_auc(pred, resp)
#   ) %>%
#   ungroup()
# 
# 
# cv_stats %>%
#   ggplot(aes(x ="one_one", y=auc)) + 
#   geom_point(
#     aes(color = split), 
#     position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
#     alpha = 0.75
#   )
# 
# cv_stats %>%
# ggplot(aes(x ="one_one", y=misclass_rate)) + 
#   geom_point(
#     aes(color = split), 
#     position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
#     alpha = 0.75
#   )
# 
# 
# adult_logit_resid %>%
#   mutate(pred_group = 
#            as.numeric(as.character(
#              cut(exp(pred)/(1+exp(pred)),
#                  seq(0, 1, .1),
#                  label = seq(0.05, .95, .1),
#                  include.lowest = TRUE)))) %>% 
#   group_by(.id, split, pred_group) %>%
#   summarise(
#     ct = n(),
#     misclass_rate = sum(is_misclass), 
#     resp_prob = mean(resp)
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = pred_group, y=resp_prob)) + 
#   geom_point(
#     aes(color = split, size = ct),
#     position = position_jitterdodge(jitter.width = 0.03, dodge.width = 0.05, 
#                                     jitter.height = 0.03),
#     alpha = 0.3
#   ) +
#   geom_abline(slope = 1, intercept = 0) +
#   labs(x = "Predicted Probablity (0.1 bins)", 
#        y = "Observed Frequency", 
#        col = NULL) +
#   theme_savvy()
# ggsave(sprintf("../figures/%s_predicted_prob.png", diag), width = 6, height = 5)

full_gam <- model_full(adult_logistic, formula_full)
sub_gam <- model_full(adult_logistic, formula_sub)

lrtest(full_gam, sub_gam)
exp(coef(full_gam))
summary(full_gam)

save(adult_logistic, full_gam, sub_gam, 
     file = sprintf("../data/%s_logistic_results.RData", diag))

adult_logistic %>%
  group_by(male, After_diag) %>%
  summarise(pos = sum(positive))
###################################################################################################

predictors <- c("After_diag", "After_diag:male", "male", 
                "s(RAF_2014)", "s(MedianHouseholdIncome)", 
                "s(Age_diff_child)", "s(sick_child_age)", "single_adult", 
                "other_child", "single_adult:other_child", "single_adult:male",
                "time", "plan_sub")

formula_full <- formula(paste("logSpend ~", paste(predictors, collapse = "+")))
formula_sub <- formula(paste("logSpend ~", 
                     paste(predictors[3:length(predictors)], collapse = "+")))

lognormal_full <-  adult_lognormal %>%
  gam(formula_full, data = ., family = gaussian)
lognormal_sub <- gam(formula_sub, data = adult_lognormal, family = gaussian)

lrtest(lognormal_full, lognormal_sub)

adult_lognormal <- adult_lognormal %>%
  add_predictions(lognormal_full) %>%
  add_residuals(lognormal_full)

save(lognormal_full, lognormal_sub, adult_lognormal, 
     file = sprintf("../data/%s_lognormal_model.Rdata", diag))

# adult_lognormal %>%
#   ggplot(aes(x = pred, y = logSpend)) + 
#   geom_point(alpha = .3) + 
#   geom_abline(intercept = 0, slope = 1, col = "red")

# summary(lognormal_full)
# exp(coef(lognormal_full))
# rsquare(lognormal_full, adult_lognormal)

# lognormal_model <- 
#   adult_lognormal %>%
#   crossv_kfold(k = 10) %>%
#   mutate(model_full = map(train, ~gam(formula_full, data=.)),
#          model_sub = map(train, ~gam(formula_sub, data=.))) %>%
#   gather(key = "model_name", value = "model", starts_with("model")) %>%
#   gather(key = "split", value = "data", train, test) %>%
#   mutate(rmse = map2_dbl(model, data, rmse)) 
# 
# lognormal_model %>%
#   ggplot(aes(x=model_name, y = rmse, col=split)) + 
#   geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
#              alpha = .3) +
#   stat_summary(fun.y = "mean", geom="point", size = 4,
#                position = position_dodge(width = 0.5)) 
# 
# sd(adult_lognormal$logSpend)
