#******************************************************************************#
# Author : Suchit Mehrotra
# Editor : Halley Brantley
# Purpose: 
#   Evaluate the 30-day readmission model predictions
#******************************************************************************#

rm(list = ls())
library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
library(scales)
load_all("../../savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)

# Load predictions from GBQ table
ids_w_preds <- 
  glue("
    SELECT
      * 
    FROM 
      {DB}_basic_drg_preds
  ") %>%
  load_bq(db = pid, query = .)

# Data for calibration plot
calib_info <- 
  ids_w_preds %>% 
  group_by(pred_quant) %>%
  summarise(
    true_probs = mean(readmit_30_flag), 
    estim_probs = mean(pred),
  ) %>%
  ungroup() %>%
  rename(ntile = pred_quant)

# Scatter Plot
calib_info %>%
ggplot(aes(x = estim_probs, y = true_probs)) + 
  geom_point(size = 3) + 
  labs(
    x = "Estimated Readmission Rate", 
    y = "True Readmission Rate", 
    title = "Model Performace (each point represents 5% of population)"
  ) + 
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = mean(ids_w_preds$readmit_30_flag), 
             linetype = 2, 
             col = 'red', 
             size = 2) +
  geom_label(aes(x = 0.05, y = 0.125, label = "Overall Readmission Rate")) +
  theme_rnd()
ggsave(filename = "../../figures/readmission_model_30days/model_calibration.png", 
       width = 10, height = 6)

# Bar Plot
calib_info %>%
mutate(ntile = ifelse(ntile <= 18, '<19', ntile)) %>% 
mutate(
  ntile = factor(
    ntile, 
    levels = c("<19", "19", "20"), 
    labels = c("Bottom 90%", "90-95%", "Top 5%")
  )
) %>%
group_by(ntile) %>%
summarise(
  mean_true = mean(true_probs), 
  mean_estim = mean(estim_probs)
) %>%
pivot_longer(cols = c("mean_true", "mean_estim")) %>%
mutate(
  name = factor(
    name, 
    levels = c("mean_estim", "mean_true"), 
    labels = c("Estimted", "Actual")
  )
) %>%
ggplot(aes(x = ntile, y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_rnd() + 
  scale_fill_rnd() + 
  labs(
    fill = "Readmit \n Proportion", 
    y = "Proportion", 
    x = "Risk Group"
  ) +
  geom_hline(yintercept = ids_w_preds$readmit_30_flag %>% mean())
ggsave(filename = "../../figures/readmission_model_30days/model_calibration_bar.png",
       width = 10, height = 6)



