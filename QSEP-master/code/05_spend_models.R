################################################################################
# QSEP Exploratory Analysis - Asthma
# Halley Brantley
################################################################################
library(savvy)
library(tidyverse)
library(modelr)
library(gam)
rm(list = ls())
load("../data/adult_model_data.Rdata")

################################################################################

predictors <- c("RAF_cat",
                "IncomeCat",
                "bs(Age, 4)",
                "youngest_child", 
                "num_children", "single_adult", 
                "time", "plan_sub")

female_data <- subset(adult_model_data, male == 0)
male_data <- subset(adult_model_data, male == 1)

flag <- quo(ASD_Flag)
diag <- "ASD"
Month_diag <- quo(Month_IND_ASD)

diag_data <- female_data %>%
  filter(!!flag == 1) 

min_diag <- diag_data %>% 
  group_by(Indv_Sys_Id) %>%
  summarise(min_month = min(!!Month_diag)) %>%
  filter(min_month > -1)

diag_data <- anti_join(diag_data, min_diag) %>% 
  filter(!!Month_diag > -13 & !!Month_diag < 13, 
         positive_spend == 1)

formula_full <- formula(paste("logSpend ~", 
                              paste(c(sprintf("After_%s", diag), 
                                      predictors), collapse = "+")))


ASD_female1 <- lm(formula_full, diag_data)
summary(ASD_female1)

diag_data <- 
  diag_data %>%
  add_residuals(ASD_female1)  

ggplot(diag_data, aes(x = MedianHouseholdIncome, y = resid)) + 
  geom_point(alpha=.2) +
  #stat_summary(fun.y = "mean", geom = "point", col = "red") +
  geom_smooth()

get_diag_model <- function(diag, flag, Month_diag, adult_model_data){
  
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  
  diag_data <- adult_model_data %>%
    filter(!!flag == 1) 
  
  min_diag <- diag_data %>% 
    group_by(Indv_Sys_Id) %>%
    summarise(min_month = min(!!Month_diag)) %>%
    filter(min_month > -1)
  
  diag_data <- anti_join(diag_data, min_diag) %>% 
    filter(!!Month_diag > -12 & !!Month_diag < 12)
  
  formula_full <- formula(paste("positive_spend ~", 
                              paste(c(sprintf("After_%s", diag), 
                                      predictors), collapse = "+")))
  

  full_gam <- gam(formula_full, family = binomial, data = diag_data)  

  return(full_gam)
}

get_lognorm_model <- function(diag, flag, Month_diag, adult_model_data){
  
  flag <- enquo(flag)
  Month_diag <- enquo(Month_diag)
  
  diag_data <- adult_model_data %>%
    filter(!!flag == 1) 
  
  min_diag <- diag_data %>% 
    group_by(Indv_Sys_Id) %>%
    summarise(min_month = min(!!Month_diag)) %>%
    filter(min_month > -1)
  
  diag_data <- anti_join(diag_data, min_diag) %>% 
    filter(!!Month_diag > -12 & !!Month_diag < 12, 
           positive_spend == 1)
  
  formula_full <- formula(paste("logSpend ~", 
                                paste(c(sprintf("After_%s", diag), 
                                        predictors), collapse = "+")))
  
  full_gam <- lm(formula_full,  data = diag_data)  
  
  return(full_gam)
}


ASD_female_logit <-  get_diag_model("ASD", ASD_Flag, Month_IND_ASD, 
                              female_data) 
ASD_male_logit <-  get_diag_model("ASD", ASD_Flag, Month_IND_ASD, 
                              male_data) 
ASD_female_lognorm <- get_lognorm_model("ASD", ASD_Flag, Month_IND_ASD, 
                                     female_data) 
ASD_male_lognorm <- get_lognorm_model("ASD", ASD_Flag, Month_IND_ASD, 
                                        male_data) 

Asthma_female_logit <-  get_diag_model("Asthma", Asthma_Flag, Month_IND_Asthma, 
                                    female_data) 
Asthma_male_logit <-  get_diag_model("Asthma", Asthma_Flag, Month_IND_Asthma, 
                                  male_data) 
Asthma_female_lognorm <- get_lognorm_model("Asthma", Asthma_Flag, Month_IND_Asthma, 
                                        female_data) 
Asthma_male_lognorm <- get_lognorm_model("Asthma", Asthma_Flag, Month_IND_Asthma, 
                                      male_data) 

Cerebral_female_logit <-  get_diag_model("Cerebral", Cerebral_Flag, Month_IND_Cerebral, 
                                       female_data) 
Cerebral_male_logit <-  get_diag_model("Cerebral", Cerebral_Flag, Month_IND_Cerebral, 
                                     male_data) 
Cerebral_female_lognorm <- get_lognorm_model("Cerebral", Cerebral_Flag, Month_IND_Cerebral, 
                                           female_data) 
Cerebral_male_lognorm <- get_lognorm_model("Cerebral", Cerebral_Flag, Month_IND_Cerebral, 
                                         male_data) 

CA_female_logit <-  get_diag_model("CA", CA_Flag, Month_IND_CA, 
                                       female_data) 
CA_male_logit <-  get_diag_model("CA", CA_Flag, Month_IND_CA, 
                                     male_data) 
CA_female_lognorm <- get_lognorm_model("CA", CA_Flag, Month_IND_CA, 
                                           female_data) 
CA_male_lognorm <- get_lognorm_model("CA", CA_Flag, Month_IND_CA, 
                                         male_data)

T1D_female_logit <-  get_diag_model("T1D", T1D_Flag, Month_IND_T1D, 
                                       female_data) 
T1D_male_logit <-  get_diag_model("T1D", T1D_Flag, Month_IND_T1D, 
                                     male_data) 
T1D_female_lognorm <- get_lognorm_model("T1D", T1D_Flag, Month_IND_T1D, 
                                           female_data) 
T1D_male_lognorm <- get_lognorm_model("T1D", T1D_Flag, Month_IND_T1D, 
                                         male_data)

Trauma_female_logit <-  get_diag_model("Trauma", Trauma_Flag, Month_IND_Trauma, 
                                       female_data) 
Trauma_male_logit <-  get_diag_model("Trauma", Trauma_Flag, Month_IND_Trauma, 
                                     male_data) 
Trauma_female_lognorm <- get_lognorm_model("Trauma", Trauma_Flag, Month_IND_Trauma, 
                                           female_data) 
Trauma_male_lognorm <- get_lognorm_model("Trauma", Trauma_Flag, Month_IND_Trauma, 
                                         male_data)
################################################################################
pred_data <- bind_rows(female_data[1, ], female_data[1, ],
                       female_data[1, ], female_data[1, ])
pred_data[c(2,4), c("After_ASD", "After_Asthma", "After_T1D", 
                    "After_CA")] <- 1
pred_data[3:4, "RAF_cat"] <- "(1,2]"

predict(ASD_female_logit, newdata = pred_data, type="response")*
exp(predict(ASD_female_lognorm, newdata = pred_data))

predict(ASD_male_logit, newdata = pred_data, type="response")*
  exp(predict(ASD_male_lognorm, newdata = pred_data))

predict(Asthma_female_logit, newdata = pred_data, type="response")*
  exp(predict(Asthma_female_lognorm, newdata = pred_data))

predict(Asthma_male_logit, newdata = pred_data, type="response")*
  exp(predict(Asthma_male_lognorm, newdata = pred_data))

predict(CA_female_logit, newdata = pred_data, type="response")*
  exp(predict(CA_female_lognorm, newdata = pred_data))

predict(CA_male_logit, newdata = pred_data, type="response")*
  exp(predict(CA_male_lognorm, newdata = pred_data))

predict(T1D_female_logit, newdata = pred_data, type="response")*
  exp(predict(T1D_female_lognorm, newdata = pred_data))

predict(T1D_male_logit, newdata = pred_data, type="response")*
  exp(predict(T1D_male_lognorm, newdata = pred_data))
###############################################################################
oddsFactors_female <- 
  data.frame(pred = names(coef(ASD_female_logit)),
             ASD = exp(coef(ASD_female_logit)),
             Asthma = exp(coef(Asthma_female_logit)), 
             Cancer = exp(coef(CA_female_logit)), 
             Cerebral = exp(coef(Cerebral_female_logit)), 
             T1D = exp(coef(T1D_female_logit)), 
             Trauma = exp(coef(Trauma_female_logit)))

pvalues <- 
  data.frame(pred = rownames(summary(ASD_full_gam)$parametric.anova),
             ASD = summary(ASD_full_gam)$parametric.anova[,5],
             Asthma = summary(Asthma_full_gam)$parametric.anova[,5], 
             Cancer = summary(CA_full_gam)$parametric.anova[,5], 
             Cerebral = summary(Cerebral_full_gam)$parametric.anova[,5], 
             T1D = summary(T1D_full_gam)$parametric.anova[,5], 
             Trauma = summary(Trauma_full_gam)$parametric.anova[,5])

pvalues <- pvalues[-nrow(pvalues),]

pvalues <- rbind(rep(0, 6), pvalues[1:3,], 
                 pvalues[3, ], pvalues[3, ], 
                 pvalues[3:10,], pvalues[10, ],
                 pvalues[10, ], pvalues[10:12, ], 
                 pvalues[12, ], pvalues[12, ], 
                 pvalues[12:nrow(pvalues), ])

cbind(as.character(pvalues[,1]), as.character(oddsFactors[,1]))

ind <- which(pvalues > 0.05, arr.ind = TRUE)

oddsFactors[,2:7] <- round(oddsFactors[,2:7], 3)
oddsFactors[ind] <- "-"

oddsFactors_out <- oddsFactors[c(1,2,18,3,4:17,19:22, 23:nrow(oddsFactors)),]

latex(oddsFactors_out[,2:7], 
      title = '', 
      file = "../tables/oddsFactors.tex", 
      rowname = c("Intercept",   "After child's diagnosis", 
                  "After child's diagnosis x Male", "Male", 
                  "RAF (0.25, 1]",
                  "RAF (1, 5]",
                  "RAF (5, 10]",
                  "RAF (10, 100]",
                  "s(Median Household Income)", 
                  "s(Age adult)",
                  "s(Sick child age)",
                  "Single Adult",
                  "Other Children", 
                  "Time",
                  "Choice Plus Plan", "Select Plus", "Choice", "Gated HMO", 
                  "Male x RAF (0.25, 1]",
                  "Male x RAF (1, 5]",
                  "Male x RAF (5, 10]",
                  "Male x RAF (10, 100]",
                  "Other Children x Single Adult", 
                  "Male x Single Adult"),
      na.blank = FALSE)

oddsFactors[ind] <- 1
oddsFactors_out <- oddsFactors[c(1,2,18,3,4:17,19:22, 23:nrow(oddsFactors)),]
odds_AfterDiag <- oddsFactors_out[2:3,2:7]
odds_AfterDiag[2,] <- round(as.numeric(odds_AfterDiag[2, ])*
  as.numeric(odds_AfterDiag[1, ]),3)
odds_AfterDiag <- t(odds_AfterDiag)
colnames(odds_AfterDiag) <- c("Female", "Male")
latex(odds_AfterDiag, 
      title = '', 
      file = "../tables/Logistic_afterdiag.tex")
################################################################################
oddsFactors <- 
  data.frame(pred = names(coef(ASD_full)),
             ASD = exp(coef(ASD_full)),
             Asthma = exp(coef(Asthma_full)), 
             Cancer = exp(coef(CA_full)), 
             Cerebral = exp(coef(Cerebral_full)), 
             T1D = exp(coef(T1D_full)), 
             Trauma = exp(coef(Trauma_full)))

pvalues <- 
  data.frame(pred = rownames(summary(ASD_full)$parametric.anova),
             ASD = summary(ASD_full)$parametric.anova[,5],
             Asthma = summary(Asthma_full)$parametric.anova[,5], 
             Cancer = summary(CA_full)$parametric.anova[,5], 
             Cerebral = summary(Cerebral_full)$parametric.anova[,5], 
             T1D = summary(T1D_full)$parametric.anova[,5], 
             Trauma = summary(Trauma_full)$parametric.anova[,5])

pvalues <- pvalues[-nrow(pvalues),]

pvalues <- rbind(rep(0, 6), pvalues[1:3,], 
                 pvalues[3, ], pvalues[3, ], 
                 pvalues[3:10,], pvalues[10, ],
                 pvalues[10, ], pvalues[10:12, ], 
                 pvalues[12, ], pvalues[12, ], 
                 pvalues[12:nrow(pvalues), ])

cbind(as.character(pvalues[,1]), as.character(oddsFactors[,1]))

ind <- which(pvalues > 0.05, arr.ind = TRUE)

oddsFactors[,2:7] <- round(oddsFactors[,2:7], 3)
oddsFactors[ind] <- "-"

oddsFactors_out <- oddsFactors[c(1,2,18,3,4:17,19:22, 23:nrow(oddsFactors)),]

latex(oddsFactors_out[,2:7], 
      title = '', 
      file = "../tables/spendFactors.tex", 
      rowname = c("Intercept",   "After child's diagnosis", 
                  "After child's diagnosis x Male", "Male", 
                  "RAF (0.25, 1]",
                  "RAF (1, 5]",
                  "RAF (5, 10]",
                  "RAF (10, 100]",
                  "s(Median Household Income)", 
                  "s(Age adult)",
                  "s(Sick child age)",
                  "Single Adult",
                  "Other Children", 
                  "Time",
                  "Choice Plus Plan", "Select Plus", "Choice", "Gated HMO", 
                  "Male x RAF (0.25, 1]",
                  "Male x RAF (1, 5]",
                  "Male x RAF (5, 10]",
                  "Male x RAF (10, 100]",
                  "Other Children x Single Adult", 
                  "Male x Single Adult"),
      na.blank = FALSE)



oddsFactors[ind] <- 1
oddsFactors_out <- oddsFactors[c(1,2,18,3,4:17,19:22, 23:nrow(oddsFactors)),]
odds_AfterDiag <- oddsFactors_out[2:3,2:7]
odds_AfterDiag[2,] <- round(as.numeric(odds_AfterDiag[2, ])*
  as.numeric(odds_AfterDiag[1, ]), 3)

odds_AfterDiag <- t(odds_AfterDiag)
colnames(odds_AfterDiag) <- c("Female", "Male")
latex(odds_AfterDiag, 
      title = '', 
      file = "../tables/Spend_afterdiag.tex")

################################################################################
adult_logistic <- adult_model_data %>%
  dplyr::select(positive_spend, After_ASD, After_Asthma, After_CA, 
                After_Cerebral, After_T1D, After_Trauma, male, RAF_cat,
                MedianHouseholdIncome, Age, sick_child_age, 
                single_adult, other_child, time, plan_sub) %>%
  na.omit() 

adult_lognormal <- adult_model_data %>% 
  filter(positive_spend == 1) %>%
  dplyr::select(Total_Allow, After_ASD, After_Asthma, After_CA, 
                After_Cerebral, After_T1D, After_Trauma, male, RAF_cat,
                MedianHouseholdIncome, Age, sick_child_age, 
                single_adult, other_child, time, plan_sub) %>%
  mutate(logSpend = log(Total_Allow)) %>%
  na.omit()


################################################################################
formula_full <- formula(paste("positive_spend ~", paste(predictors, collapse = "+")))
full_gam <- gam(formula_full, data = adult_model_data, family = binomial)

exp(coef(full_gam))
summary(full_gam)

################################################################################

formula_full <- formula(paste("logSpend ~", paste(predictors, collapse = "+")))
lognormal_full <-  adult_lognormal %>%
  gam(formula_full, data = ., family = gaussian)

################################################################################
oddsFactors <- exp(coef(full_gam))
spendFactors <- exp(coef(lognormal_full))

outFactors <- round(cbind(oddsFactors, spendFactors)[c(1:7, 23:28, 8:22, 29:length(oddsFactors)), ],3)
colnames(outFactors) <- c("Logistic", "Log-normal")
outNames <- c( "(Intercept)" , "After ASD", "After Asthma", "After Cancer", 
               "After Cerebral", "After T1D", "After Trauma" , "After ASD x male",               
               "After Asthma x male", "After Cancer x male", "After Cerebral  x male",
               "After T1D x male",  "After Trauma x male",  "male",  
               "RAF (0.25, 1]",
               "RAF (1, 5]",
               "RAF (5, 10]",
               "RAF (10, 100]",
               "s(MedianHouseholdIncome)", "s(Age Adult)",  "s(sick child age)",  
               "single adult", "other child" , "time",
               "Choice Plus Plan", "Select Plus", "Choice", "Gated HMO", 
               "Male x RAF (0.25, 1]",
               "Male x RAF (1, 5]",
               "Male x RAF (5, 10]",
               "Male x RAF (10, 100]",
                "single adult x other child",
               "male x single adult")
latex(outFactors[1:13,], 
      rowname = outNames[1:13], 
      title = '', 
      file = "../tables/combined_model_1.tex")


latex(outFactors[14:34,], 
      rowname = outNames[14:34], 
      title = '', 
      file = "../tables/combined_model_2.tex")
