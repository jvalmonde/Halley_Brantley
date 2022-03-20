
#******************************************************************************#
# Author : Karen Bioy
# Editor : Halley Brantley
# Purpose: 
#   Plotting significant variables from glm model
#******************************************************************************##**********************************************************************#
library(tidyverse)
load('../../data/glm_fit.RData')
fig_folder <- '../../figures/readmission_model_30days'

# get coefficient summary from model fitted in simple_drg_pred.R
coef_smry <- summary(glm_fit)$coefficient

# keep significant variables
signif_vars <- rownames(coef_smry)[which(coef_smry[,4] < 0.05)][-1]

# creating labels for plotting
labels <- full_join(data.frame(drg_col_name = signif_vars),
                    filter(drg_mapping, drg_col_name %in% signif_vars)) %>% 
  mutate(drg_desc2 = ifelse(is.na(drg_desc), drg_col_name, drg_desc))

# odds ratio
coef_tab <- 
  tibble(
    vars = labels$drg_desc2,
    odds = exp(coef_smry[which(rownames(coef_smry) %in% signif_vars),1]),
    low_ci = exp(coef_smry[signif_vars, 1] - (1.96*coef_smry[signif_vars, 2])),
    upper_ci = exp(coef_smry[signif_vars, 1] + (1.96*coef_smry[signif_vars, 2]))
  )

# plot estimates with CI
coef_tab %>%
  filter(odds > 1) %>%
ggplot(aes(x = reorder(vars, odds), y = odds)) +
  geom_errorbar(aes(ymin = low_ci, ymax = upper_ci), col = '#4285f4')+
  geom_point(col = 'grey35') +
  geom_hline(yintercept = 1, col = '#fcaf03') +
  coord_flip() + 
  labs(x = 'Diagnosis Related Group', y = "Odds Ratio") + 
  theme_rnd(13) +
  labs(subtitle = "Figure 5. Diagnoses predictive of readmission")

ggsave(filename = file.path(fig_folder, "positive_features.png"), 
       device = "png", width = 13, height = 7) 


coef_tab %>%
  filter(odds < 1) %>%
  arrange(odds) %>% top_n(25, -odds) %>%
  ggplot(aes(x = reorder(vars, odds), y = odds)) +
  geom_errorbar(aes(ymin = low_ci, ymax = upper_ci), col = '#4285f4')+
  geom_point(col = 'grey35') +
  geom_hline(yintercept = 1, col = '#fcaf03') +
  coord_flip() + 
  labs(x = 'Diagnosis Related Group', y = "Odds Ratio") + 
  labs(subtitle = "Figure 6: Diagnoses that reduce risk of readmission") +
  theme_rnd(13)

ggsave(filename = file.path(fig_folder, "negative_features.png"), 
       device = "png", width = 13, height = 7) 
