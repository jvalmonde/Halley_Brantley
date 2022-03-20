#**********************************************************************#
# Author: Suchit Mehrotra, Alden Cabajar
# Description: 
# 1) Makes plots for readmission counts by time to readmission, 
# 2) answers additional queries about readmission counts
#**********************************************************************#

rm(list = ls())

library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
library(data.table)
library(ggforce)
load_all("../../savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)

readmit_tab <- 
  load_bq(
    db = pid, 
    query = glue("SELECT * FROM {DB}_readmissions")
  )

# how many hospitalizations for people over the age of 65?
full_member_count <- 
  load_bq(
    db = pid, 
    query = glue("
      SELECT COUNT(DISTINCT savvy_pid) AS total_hosp, age_group
      FROM {DB}_pop
      GROUP BY age_group
    ")
  ) 

print(full_member_count)

member_count <- 
  full_member_count %>% 
  select(total_hosp) %>%
  unlist() %>%
  sum() %>%
  as.numeric()

# 30-day readmission rate
readmit_tab %>%
  filter(days_to_readmit <= 30) %>%
  summarise(count = n()/member_count) 
  
# Proportion of 30-day readmissions within 10 days
readmit_tab %>%
  filter(days_to_readmit <= 30) %>%
  mutate(under10 = ifelse(days_to_readmit < 11, 1, 0)) %>%
  group_by(under10) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count))

# filter readmit table to only people above age 65
readmit_tab <- 
  readmit_tab %>%
  select(days_to_readmit, savvy_pid)



p1 <- 
  readmit_tab %>%
  filter(days_to_readmit <= 90) %>% 
  group_by(days_to_readmit) %>%
  summarise(counts = n()) %>% 
  ggplot(aes(x = days_to_readmit, y = counts)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(
    subtitle = "Figure 3: Days to Readmission",
    x = "Days to Readmission",
    y = "Count"
  ) +
  geom_hline(aes(yintercept = 1000), col = 'darkblue', size = 2) +
  geom_ellipse(aes(x0 = 5, y0 = 2500, a = 6, b = 1500, angle = 0), col = 'darkblue') +
  theme_rnd()
  
print(p1)

ggsave(p1, filename = "../../figures/readmission_barplot.png", 
       device = "png", 
       width = 8, 
       height = 5) 

p2 <- readmit_tab %>%
  group_by(days_to_readmit) %>%
  summarise(
    daily_prop = n() / member_count
  ) %>% 
  ungroup() %>% 
  filter(days_to_readmit <= 30) %>%
  mutate(cum_sum = cumsum(daily_prop)) %>% 
  ggplot(aes(x = days_to_readmit, y = cum_sum)) + 
  geom_line() + 
  scale_x_continuous(
    minor_breaks = seq(0, 30, 5), 
    breaks = seq(0, 30, 10)
  ) + 
  labs(
    x = "Days from discharge", 
    y = "Cumulative proportion", 
    title = "Time to first readmission", 
    subtitle = "Relative to all hospitalized patients"
  ) + 
  theme_rnd() + 
  scale_color_rnd() + 
  geom_label(
    data = . %>% 
      filter(days_to_readmit %in% c(10, 20, 30, 40, 60, 90)) %>% 
      select(days_to_readmit, cum_sum), 
    aes(label = round(cum_sum, 3))
  )
print(p2)

ggsave(p2, 
       filename = "../../figures/cum_prop_30_day.png", 
       device = "png",
       width = 9, 
       height = 4) 

p3 <- 
  readmit_tab %>%
  group_by(days_to_readmit) %>%
  summarise(
    daily_prop = n() / member_count
  ) %>% 
  ungroup() %>% 
  filter(days_to_readmit <= 90) %>%
  mutate(
    cum_sum = cumsum(daily_prop)
  ) %>%  
  ggplot(aes(x = days_to_readmit, y = cum_sum)) + 
  geom_line() + 
  scale_x_continuous(
    minor_breaks = seq(0, 90, 5), 
    breaks = seq(0, 90, 10)
  ) + 
  labs(
    x = "Days from discharge", 
    y = "Cumulative Readmission Rate", 
    title = "Readmission Rates Over Time"
  ) + 
  theme_rnd() + 
  scale_color_rnd() + 
  geom_label(
    data = . %>% 
      filter(days_to_readmit %in% c(10, 20, 30, 40, 50, 60, 70, 80, 90)) %>% 
      select(days_to_readmit, cum_sum), 
    aes(label = round(cum_sum, 2))
  )
print(p3)

ggsave(p3, filename = "../../figures/cum_prop_90_day.png", 
       device = "png", width = 9, height = 4) 


#### ADDITIONAL QUERIES ####
## What is the readmission count and % within the first 10 days
setDT(readmit_tab)
days_readmit_10 <- readmit_tab[days_to_readmit <= 10] 
print(days_readmit_10[,.N]) # total count for readmissions within 10 days = 26,115
print(days_readmit_10[,.N] / member_count) #pct of readmission within 10 days 24.73%

# readmission count and % within 90 days
days_readmit_90 <- readmit_tab[days_to_readmit <= 90] 
print(days_readmit_90[,.N]) # total count for readmissions within 90 days = 105,609
print( days_readmit_90[,.N] / member_count) #pct of readmission within 90 days 100%



