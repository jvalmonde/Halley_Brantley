#******************************************************************************#
# Author : Suchit Mehrotra
# Purpose: 
#   this script creates some plots for helda tuberty at Optum, calculating
#   retrospective readmission rates, days to readmission, and costs for the
#   respective 30 day spend deciles
#******************************************************************************#

rm(list = ls())

library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
load_all("~/savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)
days <- 30

q_30_day_readmits <- 
  glue("
    SELECT 
      savvy_pid
    FROM 
      {DB}_readmissions
    WHERE
      days_to_readmit <= 30
  ")

q_readmits_w_spend_deciles <- 
  glue("
    SELECT
      savvy_pid, 
      spend_30_decile, 
      CASE 
        WHEN savvy_pid IN ({q_30_day_readmits}) THEN 1
        ELSE 0
      END AS readmit_flag
    FROM 
      {DB}_cost_decile AS a
    WHERE 
      spend_30_decile >= 8
  ")


readmit_by_decile_tab <- 
  glue("
    SELECT
      spend_30_decile,
      AVG(readmit_flag) AS readmission_rate
    FROM 
      ({q_readmits_w_spend_deciles}) 
    GROUP BY
      spend_30_decile
  ") %>%
  load_bq(db = pid, query = .)

p1 <- 
  readmit_by_decile_tab %>%
  ggplot(aes(x = spend_30_decile, y = readmission_rate)) + 
  geom_bar(stat = "identity") + 
  theme_rnd() + 
  scale_fill_rnd() + 
  labs(
    y = "Retrospective 30 Day \n Readmission Rate", 
    x = "Post Discharge 30 Day Spend Decile", 
    title = "Readmission Rates by Spend Decile"
  ) + 
  geom_label(
    data = readmit_by_decile_tab, 
    aes(label = round(readmission_rate, 2))
  )

ggsave(p1, filename = "../../figures/readmisson_by_decile.png")

readmit_costs_decile <- 
  glue("
    SELECT
      a.savvy_pid, 
      a.spend_30_decile,
      b.days_to_readmit, 
      b.allw_amt
    FROM 
      {DB}_cost_decile AS a
    LEFT JOIN
      {DB}_readmission_costs AS b
    ON
      a.savvy_pid = b.savvy_pid
    WHERE
      a.savvy_pid IN ({q_30_day_readmits}) 
      AND a.spend_30_decile >= 8
  ") %>%
  load_bq(db = pid, query = .)

p2 <- 
  readmit_costs_decile %>%
  group_by(spend_30_decile) %>%
  summarise(
    mean_days = mean(days_to_readmit)
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = spend_30_decile, y = mean_days)) + 
  geom_bar(stat = "identity") +
  theme_rnd() + 
  scale_fill_rnd() + 
  labs(
    y = "Average days to readmission", 
    x = "Post Discharge 30 Day Spend Decile", 
    title = "Days to Readmission by Spend Decile"
  ) + 
  geom_label(
    aes(label = round(mean_days, 2))
  )

ggsave(p2, filename = "../../figures/days_to_readmisson_by_decile.png")

p3 <- 
  readmit_costs_decile %>%
  group_by(spend_30_decile) %>%
  summarise(
    sum_costs = sum(allw_amt), 
    mean_costs = mean(allw_amt) / 1000,
    count = n()
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = spend_30_decile, y = mean_costs)) + 
  geom_bar(stat = "identity") +
  theme_rnd() + 
  scale_fill_rnd() + 
  labs(
    y = "Avg. cost of first readmit \n ($ Thousands)", 
    x = "Post Discharge 30 Day Spend Decile", 
    title = "Average cost of first readmission", 
    subtitle = "By spend decile"
  ) + 
  geom_label(
    aes(label = round(mean_costs, 2))
  )
  
ggsave(p3, filename = "../../figures/cost_of_readmisson_by_decile.png")