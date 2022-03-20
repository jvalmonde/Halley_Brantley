#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION: Make the population table; savvy_pid, date of first
#   hospitalization in 2018, the discharge date, gender, and total ip days
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

# Get members on an FFS plan
q_ffs_members <- ffs_members(start_yearmo = '201801', end_yearmo = '201912')

# Get first admisssion date and discharge date; also drop people with hopsital
# stays of over a year.
q_pop <- 
  glue("
    SELECT 
      a.savvy_pid,
      a.admit_dt,
      a.year_mo,
      a.combined_transfers,
      a.ip_days,
      DATE_ADD(a.admit_dt, INTERVAL a.ip_days DAY) AS discharge_dt
    FROM 
      {DB}_2018_hosp_visits AS a
    INNER JOIN (
      SELECT 
        savvy_pid, 
        MIN(admit_dt) AS admit_dt, 
        MIN(year_mo) AS year_mo
      FROM 
        {DB}_2018_hosp_visits
      GROUP BY
        savvy_pid
    ) AS b
    ON 
      a.savvy_pid = b.savvy_pid 
      AND a.admit_dt = b.admit_dt
    INNER JOIN 
      ({q_ffs_members}) ffs
    ON
      a.savvy_pid = ffs.savvy_pid
      AND a.year_mo = ffs.year_mo
    WHERE
      a.admit_dt <= '2018-12-31'
      AND ip_days < 365
  ")


load_bq(pid, glue("SELECT COUNT(DISTINCT savvy_pid) FROM ({q_pop})"))


# 588578 people as of 08/18/2020 (this will probably change)

glue("
  CREATE OR REPLACE TABLE {DB}_pop AS (
    SELECT
      a.savvy_pid, 
      a.admit_dt,
      a.discharge_dt,
      a.combined_transfers,
      a.ip_days,
      (EXTRACT(YEAR FROM a.admit_dt) - MAX(b.birth_year)) AS age_at_admit, 
      b.gender
    FROM 
      ({q_pop}) AS a
    LEFT JOIN
      df_uhgrd.miniov_member_detail AS b
    ON 
      a.savvy_pid = b.savvy_pid
      AND a.year_mo = b.year_mo
    GROUP BY
      a.savvy_pid, 
      a.discharge_dt,
      a.admit_dt, 
      a.ip_days,
      b.gender,
      a.year_mo,
      a.combined_transfers
  )
") %>%
  bq_project_query(x = pid, query = .)

# find and drop people who died the day they were discharged
q_death_dates <- death_dates(glue("{DB}_2018_hosp_visits"))

q_expired_patients <- 
  glue("
    SELECT
      a.*, 
      b.date_of_death, 
    FROM 
      {DB}_pop AS a
    LEFT JOIN
      ({q_death_dates}) AS b
    ON 
      a.savvy_pid = b.savvy_pid
    WHERE
      a.discharge_dt >= b.date_of_death
  ")

glue("
  CREATE OR REPLACE TABLE {DB}_pop AS (
    SELECT
      *,
    CASE 
      WHEN age_at_admit < 65 THEN '<65'
      WHEN age_at_admit >= 65 THEN '65+'
      ELSE NULL 
    END AS age_group,
    FROM 
      {DB}_pop
    WHERE 
      savvy_pid NOT IN (SELECT DISTINCT savvy_pid FROM ({q_expired_patients}))
  )
") %>%
  bq_project_query(x = pid, query = .)

# load_bq(
#   db = pid, 
#   query = glue("SELECT COUNT(DISTINCT savvy_pid) FROM {DB}_pop")
# )
# # 572322 AS OF 8/18/2020 (WILL CHANGE)
# 
# load_bq(
#   db = pid, 
#   query = 
#     glue("
#       SELECT COUNT(DISTINCT savvy_pid) 
#       FROM {DB}_pop 
#       WHERE age_at_admit >= 65
#     ")
# )
# 499967 as of 8/18/2020 (will change)


