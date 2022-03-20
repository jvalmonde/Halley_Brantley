################################################################################
# Compare model coefficents
# Halley Brantley

################################################################################
library(tidyverse)
library(lmtest)
rm(list=ls())

# Autism
load("ASD_logistic_results.RData")
ASD_full_gam <- full_gam
ASD_sub_gam <- sub_gam
rm(adult_logistic, full_gam, sub_gam)
lrtest(ASD_full_gam, ASD_sub_gam)

# Asthma
load("Asthma_logistic_results.RData")
Asthma_full_gam <- full_gam
Asthma_sub_gam <- sub_gam
rm(adult_logistic, full_gam, sub_gam)
lrtest(Asthma_full_gam, Asthma_sub_gam)

# Cancer
load("CA_logistic_results.RData")
CA_full_gam <- full_gam
CA_sub_gam <- sub_gam
rm(adult_logistic, full_gam, sub_gam)
lrtest(CA_full_gam, CA_sub_gam)

# Cerebral Palsy
load("Cerebral_logistic_results.RData")
Cerebral_full_gam <- full_gam
Cerebral_sub_gam <- sub_gam
rm(adult_logistic, full_gam, sub_gam)
lrtest(Cerebral_full_gam, Cerebral_sub_gam)

# Trauma
load("Trauma_logistic_results.RData")
Trauma_full_gam <- full_gam
Trauma_sub_gam <- sub_gam
rm(adult_logistic, full_gam, sub_gam)
lrtest(Trauma_full_gam, Trauma_sub_gam)

# Type 1 Diabetes
load("T1D_logistic_results.RData")
T1D_full_gam <- full_gam
T1D_sub_gam <- sub_gam
rm(adult_logistic, full_gam, sub_gam)
lrtest(T1D_full_gam, T1D_sub_gam)


oddsFactors <- 
  data.frame(pred = names(coef(ASD_full_gam)),
       ASD = exp(coef(ASD_full_gam)),
       Asthma = exp(coef(Asthma_full_gam)), 
       Cancer = exp(coef(CA_full_gam)), 
       Cerebral = exp(coef(Cerebral_full_gam)), 
       T1D = exp(coef(T1D_full_gam)), 
       Trauma = exp(coef(Trauma_full_gam)))

pvalues <- 
  data.frame(pred = rownames(summary(ASD_full_gam)$parametric.anova),
       ASD = summary(ASD_full_gam)$parametric.anova[,5],
       Asthma = summary(Asthma_full_gam)$parametric.anova[,5], 
       Cancer = summary(CA_full_gam)$parametric.anova[,5], 
       Cerebral = summary(Cerebral_full_gam)$parametric.anova[,5], 
       T1D = summary(T1D_full_gam)$parametric.anova[,5], 
       Trauma = summary(Trauma_full_gam)$parametric.anova[,5])

pvalues <- pvalues[-nrow(pvalues),]

pvalues <- rbind(rep(0, 6), pvalues[1:10,], 
                 pvalues[10, ], pvalues[10, ], 
                 pvalues[10:14,])

cbind(as.character(pvalues[,1]), as.character(oddsFactors[,1]))

ind <- which(pvalues > 0.05, arr.ind = TRUE)

oddsFactors[ind] <- NA

oddsFactors[,2:7] <- round(oddsFactors[,2:7], 3)
oddsFactors <- oddsFactors[c(1,2,15,16,3:14,17:18),]
