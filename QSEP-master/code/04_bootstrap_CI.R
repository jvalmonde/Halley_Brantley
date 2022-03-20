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
source("functions.R")
set.seed(98765)
################################################################################


t1d_fit <- boot_pred(T1D_Flag, "T1D", Month_IND_T1D, After_T1D, 
                     adult_model_data)
save(t1d_fit, file = "../data/t1d_fit.Rdata")

cerebral_fit <- boot_pred(Cerebral_Flag, "Cerebral", 
                          Month_IND_Cerebral, After_Cerebral, adult_model_data)
save(cerebral_fit, file = "../data/cerebral_fit.Rdata")

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
################################################################################

load("../data/asthma_fit.Rdata")
load("../data/cancer_fit.Rdata")
load("../data/trauma_fit.Rdata")
load("../data/t1d_fit.Rdata")
load("../data/cerebral_fit.Rdata")
load("../data/asd_fit.Rdata")

CIs <- bind_rows(
  getCI_table(cerebral_fit, "Cerebral"), 
  getCI_table(t1d_fit, "T1D"), 
  getCI_table(cancer_fit, "Cancer"),
  getCI_table(trauma_fit, "Trauma"),
  getCI_table(asthma_fit, "Asthma"),
  getCI_table(asd_fit, "ASD"))

CIs$Gender <- factor(CIs$Gender)

write.csv(CIs, file = "../tables/CIs.csv")

CIs %>% 
  filter(condition %in% c("Cerebral", "ASD", "Asthma"), 
         RAF_cat != "(10,200]") %>%
  ggplot(aes(x = RAF_cat, y = mean_b, col = Gender)) + 
    geom_errorbar(aes(ymin = lb_b, ymax = ub_b), 
                  position = position_dodge(width = 1)) +
    geom_point(position = position_dodge(width = 1)) +
    geom_label(aes(label = round(mean_b)), 
             hjust = "middle", 
             vjust = -.4, 
             position = position_dodge(width = 1)) +
    facet_grid(.~condition) +
    theme_savvy(base_size = 18)+
    theme(axis.text.x = element_text(angle=-45))+
    labs(y = "Difference in Average Monthly Spend ($)", 
         x = "RAF category",
         title = "Predicted Difference in Average Monthly Spend After Child's Diagnosis",
         subtitle = "95% CI by RAF category and Gender") 
ggsave("../figures/spend_diff_ASD_Cerebral.png", width = 12, height = 7)


CIs %>% 
  filter(condition %in% c("Cancer", "Trauma", "T1D"),
         RAF_cat != "(10,200]") %>%
  ggplot(aes(x = RAF_cat, y = mean_b, col = Gender)) + 
  geom_errorbar(aes(ymin = lb_b, ymax = ub_b), 
                position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_label(aes(label = round(mean_b)), 
             hjust = "middle", 
             vjust = -.4, 
             position = position_dodge(width = 1)) +
  facet_grid(.~condition) +
  theme_savvy(base_size = 18) +
  theme(axis.text.x = element_text(angle=-45))+
  labs(y = "Difference in Average Monthly Spend ($)", 
       x = "RAF category",
       title = "Predicted Difference in Average Monthly Spend After Child's Diagnosis",
       subtitle = "95% CI by RAF category and Gender") 
ggsave("../figures/spend_diff_Cancer_T1D.png", width = 12, height = 7)

CIs <- CIs %>%
  mutate_at(vars(mean_b:ub_b), round, digits = 2) %>%
  select(condition, Gender, lb_b, mean_b, ub_b) %>%
  arrange(lb_b)

CIs 
latex(CIs, 
      colheads = c("", "Gender", "LB", "Mean", "UB"),
      title = '', 
      file = "../tables/pred_CIs.tex", 
      na.blank = FALSE)  

t1d_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "T1D"),
       width = 12, height = 10)
t1d_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "T1D"),
       width = 12, height = 10)

asd_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "ASD"), 
       width = 12, height = 10)
asd_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "ASD"), 
       width = 12, height = 10)

cerebral_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "Cerebral"), 
       width = 12, height = 10)

cerebral_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "Cerebral"), 
       width = 12, height = 10)

cancer_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "CA"), 
       width = 12, height = 10)
cancer_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "CA"), 
       width = 12, height = 10)

trauma_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "Trauma"), 
       width = 12, height = 10)
trauma_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "Trauma"), 
       width = 12, height = 10)

asthma_fit$female$plot
ggsave(filename =  sprintf("../figures/predicted_spend_female_%s.png", "Asthma"), 
       width = 12, height = 10)
asthma_fit$male$plot
ggsave(filename =  sprintf("../figures/predicted_spend_male_%s.png", "Asthma"), 
       width = 12, height = 10)


