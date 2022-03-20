################################################################################
# QSEP Exploratory Analysis - Asthma
# Halley Brantley
################################################################################
library(savvy)
library(tidyverse)
library(modelr)
library(splines)
rm(list = ls())
load("../data/adult_model_data.Rdata")
source("functions.R")
################################################################################

flag <- quo(ASD_Flag)
diag <- "ASD"
Month_diag <- quo(Month_IND_ASD)
after_diag <- quo(After_ASD)


ASD_models <-  get_diag_models("ASD", ASD_Flag, Month_IND_ASD, After_ASD,
                              adult_model_data) 

Asthma_models <- get_diag_models("Asthma", Asthma_Flag, Month_IND_Asthma,
                                 After_Asthma, adult_model_data)

Cerebral_models <-  get_diag_models("Cerebral", Cerebral_Flag, Month_IND_Cerebral, 
                                    After_Cerebral, adult_model_data) 

CA_models <-  get_diag_models("CA", CA_Flag, Month_IND_CA, After_CA, 
                             adult_model_data)

T1D_models <-  get_diag_models("T1D", T1D_Flag, Month_IND_T1D, After_T1D, 
                               adult_model_data) 

Trauma_models <-  get_diag_models("Trauma", Trauma_Flag, Month_IND_Trauma, 
                                  After_trauma, adult_model_data) 
################################################################################


logodds_female <- 
  rbind(tidy(ASD_models$female$logistic_model)[2,1:3],
        tidy(Asthma_models$female$logistic_model)[2,1:3],
        tidy(CA_models$female$logistic_model)[2,1:3],
        tidy(Cerebral_models$female$logistic_model)[2,1:3],
        tidy(T1D_models$female$logistic_model)[2,1:3],
        tidy(Trauma_models$female$logistic_model)[2,1:3])

logodds_male <- 
  rbind(tidy(ASD_models$male$logistic_model)[2,1:3],
        tidy(Asthma_models$male$logistic_model)[2,1:3],
        tidy(CA_models$male$logistic_model)[2,1:3],
        tidy(Cerebral_models$male$logistic_model)[2,1:3],
        tidy(T1D_models$male$logistic_model)[2,1:3],
        tidy(Trauma_models$male$logistic_model)[2,1:3])

logspend_female <- 
  rbind(tidy(ASD_models$female$lognormal_model)[2,1:3],
        tidy(Asthma_models$female$lognormal_model)[2,1:3],
        tidy(CA_models$female$lognormal_model)[2,1:3],
        tidy(Cerebral_models$female$lognormal_model)[2,1:3],
        tidy(T1D_models$female$lognormal_model)[2,1:3],
        tidy(Trauma_models$female$lognormal_model)[2,1:3])

logspend_male <- 
  rbind(tidy(ASD_models$male$lognormal_model)[2,1:3],
        tidy(Asthma_models$male$lognormal_model)[2,1:3],
        tidy(CA_models$male$lognormal_model)[2,1:3],
        tidy(Cerebral_models$male$lognormal_model)[2,1:3],
        tidy(T1D_models$male$lognormal_model)[2,1:3],
        tidy(Trauma_models$male$lognormal_model)[2,1:3])

parameter_est <- rbind(cbind(logodds_female, logspend_female), 
               cbind(logodds_male, logspend_male))

parameter_est <- parameter_est[,-4]
parameter_est <- parameter_est %>%
  mutate_at(2:5, round, digits = 3)

latex(parameter_est[,2:5], 
      title = '', 
      file = "../tables/after_diagnosis_coefficients.tex", 
      n.cgroup = c(2, 2), 
      cgroup = c("Logistic Model", "Log-normal model"),
      colheads = c("Estimate", "Std. Error", "Estimate", "Std. Error"),
      rowname = c("ASD", "Asthma", "Cancer", "Cerebral Palsy", "T1D", "Trauma",
                  "ASD", "Asthma", "Cancer", "Cerebral Palsy", "T1D", "Trauma"),
      n.rgroup = c(6, 6),
      rgroup = c("Women", "Men"),
      na.blank = FALSE)

