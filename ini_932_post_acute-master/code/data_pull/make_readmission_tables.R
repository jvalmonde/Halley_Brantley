#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION: 
#   Table for readmissions and their respective costs. readmissions are only
#   relative to the first admission.
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

# determine the differences between admit date and hosp date and hosp date and
# discharge date we want to anchor on.
glue("
  CREATE OR REPLACE TABLE {DB}_readmissions AS (
    SELECT 
      a.savvy_pid,
      a.combined_transfers,
      a.admit_dt, 
      a.discharge_dt AS index_discharge_dt, 
      b.admit_dt AS readmit_dt,
      DATE_ADD(b.admit_dt, INTERVAL b.ip_days DAY) AS readmit_discharge_dt,
      a.age_group,
      a.age_at_admit,
      DATE_DIFF(b.admit_dt, a.admit_dt, DAY) AS admit_date_diff,
      DATE_DIFF(b.admit_dt, a.discharge_dt, DAY) AS readmit_days
    FROM
      {DB}_pop AS a
    LEFT JOIN
      {DB}_2018_hosp_visits AS b
    ON 
      a.savvy_pid = b.savvy_pid
    WHERE
      DATE_DIFF(b.admit_dt, a.admit_dt, DAY) >= 0
    ORDER BY 
      a.savvy_pid, 
      b.admit_dt
  )
") %>%
  bq_project_query(x = pid, query = .)

# how many hospitalizations for people over the age of 65?
full_member_count <- 
  load_bq(
    db = pid, 
    query = glue("
      SELECT COUNT(DISTINCT savvy_pid) AS total_hosp, age_group
      FROM {DB}_readmissions
      GROUP BY age_group
    ")) 



# minimum readmit day gets the first hospitalization date from the anchored
# discharge date. readmit days have to be between 1 and 30 for it to be
# a true readmit
glue("
  CREATE OR REPLACE TABLE {DB}_readmissions AS (
    SELECT
      savvy_pid, 
      admit_dt, 
      index_discharge_dt, 
      combined_transfers,
      MIN(readmit_dt) AS readmit_dt, 
      MIN(readmit_discharge_dt) AS readmit_discharge_dt,
      MIN(readmit_days) AS days_to_readmit,
      age_group
    FROM
      {DB}_readmissions
    WHERE
      readmit_days between 1 and 90
    GROUP BY
      savvy_pid, 
      index_discharge_dt, 
      admit_dt,
      age_group,
      combined_transfers
  )
") %>%
  bq_project_query(x = pid, query = .)



glue("
  CREATE OR REPLACE TABLE {DB}_readmission_costs AS (
    SELECT
      a.savvy_pid,
      a.readmit_dt,
      a.readmit_discharge_dt,
      SUM(b.allw_amt) AS allw_amt,
      a.age_group,
      a.days_to_readmit
    FROM
      {DB}_readmissions AS a
    LEFT JOIN
      `df_uhgrd.miniov_medical_claim` AS b
    ON
      a.savvy_pid = b.savvy_pid
      AND a.readmit_dt = b.full_dt
    WHERE
      hce_srvc_typ_desc = 'med/surg/icu'
      AND (b.full_dt BETWEEN a.readmit_dt AND a.readmit_discharge_dt)
    GROUP BY
      a.savvy_pid,
      a.readmit_dt,
      a.readmit_discharge_dt,
      a.age_group,
      a.days_to_readmit
    ORDER BY
      days_to_readmit    
  )
") %>%
bq_project_query(x = pid, query =  .)

