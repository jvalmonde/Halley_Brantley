#**********************************************************************#
# AUTHOR: Suchit Mehrotra and Halley Brantley
# EDITORS:
# DESCRIPTION: Get a fixed hospital visits table; adjusted for transfers
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
source("./fix_hosp_visits.R")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)

# find all hospital visits in 2018 and 2019
q_hospital_visits <-
  hospital_visits(
    medical_claims = "df_uhgrd.miniov_medical_claim",
    srvc_type = "srvc_typ_desc",
    hce_srvc = "hce_srvc_typ_desc",
    day_ct = "day_cnt",
    full_dt = "full_dt",
    allw_amt = "allw_amt",
    start_date = "2018-01-01",
    end_date = "2019-12-31",
    drg = "drg_desc"
  )

# add admit_dt column
glue("
  CREATE OR REPLACE TABLE {DB}_all_hosp_visits AS (
    SELECT 
      savvy_pid,
      full_dt AS admit_dt, 
      SUM(ip_days) as ip_days,
      0 AS combined_transfers 
    FROM 
      ({q_hospital_visits})
    GROUP BY 
      savvy_pid, 
      admit_dt
  )
") %>%
  bq_project_query(x = pid, query = .)

# 2.093,754 admissions as of 08/18/2020 (this will probably change)
# Now includes both 2018 and 2019 hospitalizations
load_bq(
  pid, 
  glue("SELECT COUNT(*) FROM {DB}_all_hosp_visits")
) %>%
  print()

# fix the hospital visits by correctly accounting for transfers 
add_transfer_flag(hosp_visit_tab = glue("{DB}_all_hosp_visits"))

dual_count <- 
  glue("
      SELECT count(*)
      FROM {DB}_all_hosp_visits
      WHERE is_transfer = 1 AND incomplete_admit = 1
         
    ") %>%
  load_bq(db = pid, query = .)

print(dual_count)

# Store combined visits in {DB}_2018_hosp_visits
fix_hosp_visits(
  hosp_visit_tab = glue("{DB}_all_hosp_visits"),
  combined_visit_tab = glue("{DB}_2018_hosp_visits")
)

while(TRUE) 
{
  add_transfer_flag(glue("{DB}_2018_hosp_visits"))
  
  # Count of visits that need to be comined
  dual_count <- 
    glue("
      SELECT count(*)
      FROM {DB}_2018_hosp_visits
      WHERE is_transfer = 1 AND incomplete_admit = 1
         
    ") %>%
    load_bq(db = pid, query = .)
  
  print(dual_count)
  if(dual_count == 0) {break;}
  
  fix_hosp_visits(
    hosp_visit_tab = glue("{DB}_2018_hosp_visits"),
    combined_visit_tab = glue("{DB}_2018_hosp_visits")
  )
  
}

# add the right year months; relative to admit date, drop transfer flags
glue("
  CREATE OR REPLACE TABLE {DB}_2018_hosp_visits AS (
    SELECT
      savvy_pid, 
      combined_transfers,
      ip_days, 
      admit_dt, 
      (EXTRACT(YEAR FROM admit_dt) * 100) + 
        (EXTRACT(MONTH FROM admit_dt)) AS year_mo,
    FROM
     {DB}_2018_hosp_visits
  )    
") %>%
  bq_project_query(x = pid, query = .)

# 852,743 total hospital visits with admit date in 2018
load_bq(
  pid, 
  glue("SELECT count(*) FROM {DB}_2018_hosp_visits")
) %>%
  print()

