#******************************************************************************#
# Author : Suchit Mehrotra
# Purpose: 
#   this script creates a CSV file that maps admission DRGs with readmission
#   drgs depending on the time period in which the re-admission happened. One of
#   out to-dos needs to be to group these drgs into more meaningful subgroups
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

q_readmission_reasons <- 
  glue("
    SELECT
      a.savvy_pid,
      a.age_group,
      a.readmit_dt,
      SUM(b.allw_amt) AS allw_amt,
      a.days_to_readmit, 
      b.drg_desc
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
  ") 

q_admission_reasons <- 
  glue("
    SELECT
      a.savvy_pid,
      a.admit_dt,
      SUM(b.allw_amt) AS allw_amt,
      b.drg_desc
    FROM
      `foc_interoperability.ini_932_readmissions` AS a
    LEFT JOIN
      `df_uhgrd.miniov_medical_claim` AS b
    ON
      a.savvy_pid = b.savvy_pid
      AND a.admit_dt = b.full_dt
    WHERE
      b.hce_srvc_typ_desc = 'med/surg/icu'
    GROUP BY
      a.savvy_pid,
      a.admit_dt,
      b.drg_desc
    ORDER BY
      a.savvy_pid
  ") 

admit_readmit_reasons <- 
  glue("
    SELECT   
      a.savvy_pid, 
      a.days_to_readmit, 
      a.age_group,
      b.drg_desc as admission_drg,
      a.drg_desc as readmission_drg
    FROM 
      ({q_readmission_reasons}) AS a
    FULL JOIN 
      ({q_admission_reasons}) AS b
    ON 
      a.savvy_pid = b.savvy_pid
  ") %>%
  load_bq(db = pid, query = .)

counts <- 
  admit_readmit_reasons %>%
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
  group_by(readmit_groups, admission_drg, readmission_drg) %>%
  summarise(counts = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "readmit_groups", values_from = "counts") %>%
  select(
    admission_drg, readmission_drg, first, second, third, fourth, fifth
  ) %>%
  arrange(desc(first))

counts %>%
  write_csv(path = "~/Desktop/admission_readmission_drgs.csv")



