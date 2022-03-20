#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION: 
#   makes a plot for the cost of readmissions and the counts for each day after
#   the discharge date. you can change the days parameter to get a different
#   range of days
# OUTPUTS: Plots
#**********************************************************************#

rm(list = ls())

library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
load_all("../../savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)
days <- 90

readmit_costs_tab <- 
  load_bq(
    db = pid, 
    query = glue("Select * from {DB}_readmission_costs")
  )

p1 <- 
  readmit_costs_tab %>%
  filter(
    age_group == '65+', 
    days_to_readmit <= days
  ) %>%
  mutate(
    total_member_count = n(), 
    total_costs = sum(allw_amt)
  ) %>%
  group_by(days_to_readmit, total_member_count, total_costs) %>%
  summarise(
    member_count = n(),
    sum_allw_amt = sum(allw_amt)
  ) %>%  
  ungroup() %>%
  mutate(
    member_count = member_count / total_member_count, 
    sum_allw_amt = sum_allw_amt / total_costs
  ) %>%
  pivot_longer(cols = c('member_count', 'sum_allw_amt')) %>% 
  mutate(
    name = factor(
      name, 
      levels = c("member_count", "sum_allw_amt"), 
      labels = c("Count", "Costs")
    )
  ) %>%
  ggplot(aes(x = days_to_readmit, y = value, fill = name)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  labs(
    x = "Days to Readmission", 
    y = glue("Proportion of {days} Day Total"), 
    fill = "Readmit:", 
    title = "Readmission Counts and Costs", 
    subtitle = "Readmission costs are proportional to counts"
  ) + 
  theme_rnd() + 
  scale_fill_rnd()

fname = glue("../../figures/costs_and_counts_{days}_days.png")
ggsave(p1, filename = fname)
