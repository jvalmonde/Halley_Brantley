#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION: 
#   Table for snf admissions and their respective costs.
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


# Find all SNF visits
# find all hospital visits in 2018 and 2019
q_snf_visits <-
  hospital_visits(
    medical_claims = "df_uhgrd.miniov_medical_claim",
    srvc_type = "srvc_typ_desc",
    hce_srvc = "hce_srvc_typ_desc",
    day_ct = "day_cnt",
    full_dt = "full_dt",
    allw_amt = "deriv_amt",
    start_date = "2018-01-01",
    end_date = "2019-12-31",
    ip_type = "skilled nursing"
  )

# Table with all SNF visits
glue("
  CREATE OR REPLACE TABLE {DB}_all_snf_visits AS (
    SELECT 
      savvy_pid,
      full_dt AS admit_dt, 
      SUM(ip_days) as ip_days,
      SUM(allw_amt) as deriv_amt
    FROM 
      ({q_snf_visits})
    GROUP BY 
      savvy_pid, 
      admit_dt
  )
") %>%
  bq_project_query(x = pid, query = .)


glue("
 SELECT 
  SUM(deriv_amt)
 FROM 
  {DB}_all_snf_visits a
 JOIN
     {DB}_pop b
  ON
  a.savvy_pid = b.savvy_pid
  WHERE 
    a.admit_dt BETWEEN DATE_ADD(b.discharge_dt, INTERVAL 0 day) 
          AND DATE_ADD(b.discharge_dt, INTERVAL 30 day)
     ") %>% load_bq(pid, .)

# determine the differences between admit date and hosp date and hosp date and
# discharge date we want to anchor on.
glue("
  CREATE OR REPLACE TABLE {DB}_snf AS (
    SELECT 
      a.savvy_pid,
      a.admit_dt, 
      a.discharge_dt AS index_discharge_dt, 
      b.admit_dt AS snf_admit_dt,
      b.deriv_amt AS snf_cost,
      b.ip_days AS snf_days,
      DATE_ADD(b.admit_dt, INTERVAL b.ip_days DAY) AS snf_admit_discharge_dt,
      a.age_group,
      a.age_at_admit,
      DATE_DIFF(b.admit_dt, a.admit_dt, DAY) AS admit_date_diff,
      DATE_DIFF(b.admit_dt, a.discharge_dt, DAY) AS snf_days_from_dis,
      c.readmit_dt, 
      c.readmit_discharge_dt,
      CASE WHEN b.admit_dt >= c.readmit_discharge_dt 
           THEN 1 ELSE 0 
           END AS after_readmit
    FROM
      {DB}_all_snf_visits AS b
    JOIN 
     {DB}_pop AS a
    ON 
      a.savvy_pid = b.savvy_pid
    LEFT JOIN
     {DB}_readmissions AS c
    ON
      a.savvy_pid = c.savvy_pid
    WHERE
      b.admit_dt BETWEEN DATE_ADD(a.discharge_dt, INTERVAL 0 day) 
          AND DATE_ADD(a.discharge_dt, INTERVAL 30 day)
    ORDER BY 
      a.savvy_pid, 
      b.admit_dt
  )
") %>%
  bq_project_query(x = pid, query = .)


