#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION:
#   Makes plots for the proportion of readmissions that had a particular DRG
# INPUTS: 
# OUTPUTS: 
#**********************************************************************#
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

# join medical claim with readmission dates to find drgs
readmit_drgs <- 
  glue("
    SELECT
      a.savvy_pid,
      a.age_group,
      a.readmit_dt,
      SUM(b.allw_amt) AS allw_amt,
      a.days_to_readmit, 
      b.drg_desc,
    FROM
      `foc_interoperability.ini_932_readmissions` AS a
    LEFT JOIN
      `df_uhgrd.miniov_medical_claim` AS b
    ON
      a.savvy_pid = b.savvy_pid
      AND a.readmit_dt = b.full_dt
    WHERE
      b.hce_srvc_typ_desc = 'med/surg/icu'
    GROUP BY
      a.savvy_pid,
      a.age_group,
      a.readmit_dt,
      a.days_to_readmit, 
      b.drg_desc
    ORDER BY
      a.days_to_readmit    
  ") %>%
  load_bq(db = pid, query = .)

# get admit drgs so that we can plot frequency of index admissions
admit_drgs <- 
  glue("
    SELECT 
      a.savvy_pid, 
      a.admit_dt,
      a.drg_desc
    FROM 
      {DB}_2018_hosp_visits a
    INNER JOIN
      `{DB}_readmissions` AS b
    ON
      a.savvy_pid = b.savvy_pid
      AND a.admit_dt = b.admit_dt
    WHERE 
      b.age_group = '65+'
  ") %>%
  load_bq(db = pid, query = .)

# take the drgs from above and figure out how often they happened and when
readmit_drg_counts <- 
  readmit_drgs %>%
  filter(age_group == '65+') %>%
  mutate(
    readmit_groups = case_when(
      days_to_readmit <= 7 ~ 'first', 
      days_to_readmit <= 14 ~ 'second', 
      days_to_readmit <= 21 ~ 'third', 
      days_to_readmit <= 30 ~ 'fourth', 
      TRUE ~ 'fifth'
    )
  ) %>% 
  select(readmit_groups, drg_desc) %>%
  bind_rows(
    admit_drgs %>% 
      mutate(readmit_groups = 'index') %>%
      select(readmit_groups, drg_desc)
  ) %>% 
  group_by(readmit_groups, drg_desc) %>%
  summarise(counts = n()) %>%
  ungroup()

# there are too many drgs. want to subset to a reasonable number to make the
# plot more informative
drg_count_filter <- 
  readmit_drg_counts %>% 
  group_by(readmit_groups) %>% 
  slice_max(order_by = counts, n = 35) %>%
  ungroup() %>%
  select(drg_desc) %>%
  unlist() %>%
  unique()
length(drg_count_filter)

# for the subset drgs, what are the proportions relative to the total
# readmissions in a time period
readmit_props <- 
  readmit_drg_counts %>%
  mutate(
    drg_desc = ifelse(drg_desc %in% drg_count_filter, drg_desc, "Other")
  ) %>% 
  group_by(readmit_groups, drg_desc) %>% 
  summarise(counts = sum(counts)) %>% 
  ungroup() %>% 
  group_by(readmit_groups) %>%
  mutate(props = counts / sum(counts)) %>%
  ungroup() %>%
  select(readmit_groups, drg_desc, props) %>%
  mutate(
    readmit_groups = 
      factor(
        readmit_groups, 
        levels = c("index", "first", "second", "third", "fourth", "fifth"),
        labels = c("index", "1-7", "8-14", "15-21", "22-30", "31-90")
      )
  ) 

# plot the drg proportions 
p1 <- 
  readmit_props %>%
  filter(drg_desc != "Other", drg_desc != "unknown drg code") %>%
  ggplot(aes(x = readmit_groups, y = fct_reorder(drg_desc, props))) + 
  geom_raster(aes(fill = props)) +
  labs(
    y = "Diagnosis-related Group",
    x = "Days to Readmission", 
    title = "Major Diagnoses Vary by Time to Readmission",
    fill = "Proportion \n of admissions"
  ) +
  scale_fill_distiller(palette = 'Spectral')
p1

fname = glue("../../figures/drg_variability_drop_other.png")
ggsave(p1, filename = fname, height = 10, width = 8)

# replot but drop sepsis and heart failure since they dominate the plot
p2 <- 
  readmit_props %>%
  filter(
    !(drg_desc %in% c(
      "Other", "unknown drg code", 
      "septic or sev sepsis wo mv >96 hrs w mcc", 
      "heart failure & shock w mcc"
    ))
  ) %>%
  ggplot(aes(x = readmit_groups, y = drg_desc)) + 
  geom_raster(aes(fill = props)) + 
  labs(
    y = "Diagnosis-related Group",
    x = "Days to Readmission", 
    title = "Major Diagnoses Vary by Time to Readmission"
  ) +
  scale_fill_distiller(palette = 'Spectral')

fname = glue("../../figures/drg_variability_drop_top3.png")
ggsave(p2, filename = fname)
  

# table of readmission counts by reason
readmit_drg_counts %>%
  group_by(readmit_groups, drg_desc) %>% 
  summarise(counts = sum(counts)) %>% 
  ungroup() %>%
  select(readmit_groups, drg_desc, counts) %>%
  pivot_wider(names_from = "readmit_groups", values_from = "counts") %>%
  select(drg_desc, first, second, third, fourth, fifth) %>%
  arrange(desc(first)) %>%
  write_csv(path = "~/Desktop/counts_of_readmission_drgs.csv")

