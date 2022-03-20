################################################################################
# QSEP Exploratory Analysis - Asthma
# Halley Brantley
################################################################################
library(savvy)
library(tidyverse)
library(modelr)
library(mgcv)
library(splines)
rm(list = ls())
load("../data/adult_model_data.Rdata")

################################################################################


ASD_logit <-  get_depress_model("ASD", ASD_Flag, Month_IND_ASD, After_ASD, 
                              adult_model_data)
Asthma_logit <-  get_depress_model("Asthma", Asthma_Flag, Month_IND_Asthma, 
                             After_Asthma, adult_model_data)

Cerebral_logit <-  get_depress_model("Cerebral", Cerebral_Flag, Month_IND_Cerebral, 
                             After_Cerebral, adult_model_data)

CA_logit <-  get_depress_model("CA", CA_Flag, Month_IND_CA, 
                            After_CA, adult_model_data)

Trauma_logit <-  get_depress_model("Trauma", Trauma_Flag, Month_IND_Trauma, 
                            After_Trauma, adult_model_data)

T1D_logit <-  get_depress_model("T1D", T1D_Flag, Month_IND_T1D, 
                            After_T1D, adult_model_data)

combined_depress <- bind_rows(ASD_logit, Asthma_logit, Cerebral_logit, 
                              CA_logit, Trauma_logit, T1D_logit) 

combined_depress <- combined_depress %>% 
  mutate_at(vars(LB:UB), round, digits = 2)

latex(combined_depress, 
      title = '', 
      rowname = NULL,
      file = "../tables/odds_depression.tex", 
      na.blank = FALSE)


