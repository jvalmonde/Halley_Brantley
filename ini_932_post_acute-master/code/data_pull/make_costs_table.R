#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION: Get 30 day costs for individuals who were discharged from
#   an inpatient visit to home
# INPUTS: 
# OUTPUTS: 
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

q_all_costs <- 
  glue("
    SELECT
      a.savvy_pid,
      a.deriv_amt, 
      a.full_dt, 
      a.year_mo,
      a.srvc_typ_cd,
      a.hce_srvc_typ_desc 
    FROM
      `df_uhgrd.miniov_medical_claim` AS a
    INNER JOIN
      {DB}_pop AS b
    ON 
      a.savvy_pid = b.savvy_pid
    WHERE
      (a.full_dt BETWEEN DATE_ADD(b.discharge_dt, interval 0 day) 
        AND DATE_ADD(b.discharge_dt, interval 90 day))
      AND NOT 
        (a.hce_srvc_typ_desc = 'med/surg/icu' AND b.discharge_dt = a.full_dt)
  ") 

# Costs table; hospital transfer fix can't account for situations where a
# transfer has length of zero ip days. We shouldn't include those med/surg/icu
# costs into our cost analysis; hence the where clause at the end. note, the
# hospital fix also won't account for people who get discharged on the same day.
glue("
  CREATE OR REPLACE TABLE {DB}_costs AS (
    SELECT
      a.savvy_pid,
      b.srvc_typ_cd,
      b.hce_srvc_typ_desc,
      b.full_dt,
      a.discharge_dt,
      a.admit_dt,
      a.age_at_admit,
      CASE 
        WHEN a.age_at_admit < 65 THEN '<65'
        WHEN a.age_at_admit >= 65 THEN '65+'
        ELSE NULL 
      END AS age_group,
      CASE 
        WHEN b.full_dt BETWEEN DATE_ADD(a.discharge_dt, INTERVAL 0 day) 
          AND DATE_ADD(a.discharge_dt, INTERVAL 30 day) THEN deriv_amt
        ELSE 0
      END AS spend_30,
      CASE 
        WHEN b.full_dt BETWEEN DATE_ADD(a.discharge_dt, INTERVAL 0 day) 
          AND DATE_ADD(a.discharge_dt, INTERVAL 60 day) THEN deriv_amt
        ELSE 0
      END AS spend_60,
      CASE 
        WHEN b.full_dt BETWEEN DATE_ADD(a.discharge_dt, INTERVAL 0 day) 
          AND DATE_ADD(a.discharge_dt, INTERVAL 90 day) THEN deriv_amt
        ELSE 0
      END AS spend_90
    FROM 
      {DB}_pop AS a
    LEFT JOIN
      ({q_all_costs}) AS b
    ON
      a.savvy_pid = b.savvy_pid
  )
  ") %>%
bq_project_query(x = pid, query = .)

# Cost deciles table
glue("
CREATE OR REPLACE TABLE {DB}_cost_decile AS (
  SELECT
    savvy_pid, 
    age_at_admit,
    age_group,
    SUM(spend_30) AS spend_30, 
    SUM(spend_60) AS spend_60, 
    SUM(spend_90) AS spend_90,
    NTILE(10) OVER 
      (PARTITION BY age_group ORDER BY SUM(spend_30)) AS spend_30_decile
  FROM 
    {DB}_costs
  GROUP BY
    savvy_pid,
    age_at_admit,
    age_group
)
") %>%
bq_project_query(x = pid, query = .)

# Cost categories
q_member_counts <- glue("
  SELECT
    spend_30_decile, 
    age_group,
    count(distinct savvy_pid) AS n_members
  FROM 
    {DB}_cost_decile
  GROUP BY
    spend_30_decile,
    age_group
")

q_cost_categories <- glue("
  SELECT
    spend_30_decile, 
    age_group,
    srvc_typ_cd,
    hce_srvc_typ_desc,
    sum(costs.spend_30) as total_cost
  FROM 
    {DB}_costs costs
  JOIN
    {DB}_cost_decile
  USING
    (savvy_pid, age_group)
  GROUP BY 
    spend_30_decile, 
    age_group,
    srvc_typ_cd,
    hce_srvc_typ_desc
")

glue("
CREATE OR REPLACE TABLE {DB}_cost_categories AS (
  SELECT 
    categories.*,
    n_members
  FROM 
    ({q_cost_categories}) categories
  JOIN
    ({q_member_counts})
  USING
    (spend_30_decile, age_group)
)
")  %>%
  bq_project_query(x = pid, query = .)  

  
