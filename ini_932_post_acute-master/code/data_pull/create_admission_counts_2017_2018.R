#**********************************************************************#
# AUTHOR: Alden Cabajar
# EDITORS: Karen Chrisele Bioy
# DESCRIPTION: 
#   Table for readmissions and their respective costs. readmissions are only
#   relative to the first admission.
#**********************************************************************#


library(bigrquery)
library(data.table)
library(glue)
library(devtools)
library(lubridate)
library(tidyverse)
setwd("~/ini_932_post_acute")
source("code/data_pull/fix_hosp_visits.R")
load_all("savvyR")

DB <- "foc_interoperability.ini_1085"
pid <- 'research-01-217611'
tbl_name <-  "admission_counts_features_2017_2018"

# get hospital visits for 2018
q_hospital_visits_2018 <-
  hospital_visits(
    medical_claims = "df_uhgrd.miniov_medical_claim",
    srvc_type = "srvc_typ_desc",
    hce_srvc = "hce_srvc_typ_desc",
    day_ct = "day_cnt",
    full_dt = "full_dt",
    allw_amt = "allw_amt",
    start_date = "2017-01-01",
    end_date = "2018-12-31",
    drg = "drg_desc"
  )

# get snf visits in 2017
q_snf_visits_2017 <- 
  hospital_visits(
    medical_claims = "df_uhgrd.miniov_medical_claim",
    srvc_type = "srvc_typ_desc",
    hce_srvc = "hce_srvc_typ_desc",
    day_ct = "day_cnt",
    full_dt = "full_dt",
    allw_amt ="allw_amt",
    start_date = "2017-01-01",
    end_date = "2017-12-31",
    ip_type = "skilled nursing"
  )

admit_cnts_pop <- 
  glue('SELECT savvy_pid, year_nbr, year_mo, full_dt, clm_aud_nbr, dx1_diag_cd, dx2_diag_cd, dx3_diag_cd
                 FROM `df_uhgrd.miniov_medical_claim`
                 WHERE year_nbr = 2017')

# get members chronic condition in 2017
cc_labels <- cms_ccw %>% 
  mutate(Label = paste0("cc_",Label))

q_conditions_2017 <- chronic_conditions(claim_table = admit_cnts_pop,
                                        icd_table = cc_labels,
                                        start_yearmo = 201701,
                                        end_yearmo = 201712)


# add admit_dt column
glue("
  CREATE OR REPLACE TABLE {DB}_{tbl_name} AS (
    SELECT 
      savvy_pid,
      full_dt AS admit_dt, 
      SUM(ip_days) as ip_days,
      0 AS combined_transfers 
    FROM 
      ({q_hospital_visits_2018})
    GROUP BY 
      savvy_pid, 
      admit_dt
  )
") %>%
  bq_project_query(x = pid, query = .)

load_bq(
  pid, 
  glue("SELECT COUNT(*) FROM {DB}_{tbl_name}")
) %>%
  print() 
# fix the hospital visits by correctly accounting for transfers 
add_transfer_flag(hosp_visit_tab = glue("{DB}_{tbl_name}"))

dual_count <- 
  glue("
      SELECT count(*)
      FROM {DB}_{tbl_name}
      WHERE is_transfer = 1 AND incomplete_admit = 1
         
    ") %>%
  load_bq(db = pid, query = .)

print(dual_count)

# Store combined visits in {DB}_2018_hosp_visits
fix_hosp_visits(
  hosp_visit_tab = glue("{DB}_{tbl_name}"),
  combined_visit_tab = glue("{DB}_{tbl_name}")
)

while(TRUE) 
{
  add_transfer_flag(glue("{DB}_{tbl_name}"))
  
  # Count of visits that need to be comined
  dual_count <- 
    glue("
      SELECT count(*)
      FROM {DB}_{tbl_name}
      WHERE is_transfer = 1 AND incomplete_admit = 1
         
    ") %>%
    load_bq(db = pid, query = .)
  
  print(dual_count)
  if(dual_count == 0) {break;}
  
  fix_hosp_visits(
    hosp_visit_tab = glue("{DB}_{tbl_name}"),
    combined_visit_tab = glue("{DB}_{tbl_name}")
  )
  
}

# add the right year months; relative to admit date, drop transfer flags
glue("
  CREATE OR REPLACE TABLE {DB}_{tbl_name} AS (
    SELECT
      savvy_pid, 
      combined_transfers,
      ip_days, 
      admit_dt, 
      (EXTRACT(YEAR FROM admit_dt) * 100) + 
        (EXTRACT(MONTH FROM admit_dt)) AS year_mo,
    FROM
     {DB}_{tbl_name}
  )    
") %>%
  bq_project_query(x = pid, query = .)

# 852,743 total hospital visits with admit date in 2018
load_bq(
  pid, 
  glue("SELECT count(*) FROM {DB}_{tbl_name}")
) %>%
  print()


# get total costs, separate hospital admit spend, snf spend, and overall spend
q_costs_2018 <- "
    SELECT
      DISTINCT savvy_pid,
      SUM(
        IF
        (srvc_typ_cd = 'ip'
          AND hce_srvc_typ_desc = 'med/surg/icu',
          allw_amt,
          0)) AS hosp_spend,
      SUM(
        IF
        (srvc_typ_cd = 'ip'
          AND hce_srvc_typ_desc = 'skilled nursing',
          allw_amt,
          0)) AS snf_spend,
      SUM(allw_amt) AS total_spend,
      COUNT(DISTINCT
            IF
            (srvc_typ_cd = 'ip'
              AND hce_srvc_typ_desc = 'skilled nursing',
              full_dt,
              NULL)) AS snf_admits
      FROM df_uhgrd.miniov_medical_claim
      WHERE year_nbr = 2018
      GROUP BY savvy_pid
    "

# query for death dates
q_deaths_2018 <- death_dates(glue('{DB}_pop_ce_2017'))


## joining other features to main cohort table
# get fraily feature column names
ff_col_names <- load_bq(
  pid,
  "SELECT * FROM research-01-217611.foc_interoperability.INFORMATION_SCHEMA.COLUMNS
  WHERE table_name = 'ini_1085_frailty_gilbert_2017'" 
) %>% 
  filter(grepl("frail_", column_name)) %>% 
  pull(column_name)

# convert nulls in frailty features to zero
ff_null_qr <- sapply(ff_col_names, function(x){
  glue("IFNULL(e.{x}, 0) as {x}")
}) %>% 
  glue_collapse(sep = ",")


# convert nulls in comorbidity labels to zero
cc_null_qr <- sapply(unique(cc_labels$Label), function(x){
  glue("IFNULL(f.{x}, 0) as {x}")
}) %>% 
  glue_collapse(sep = ",")


# main query for admission counts table: population continuously enrolled 2017 to 2018
q_admit_cnts_features <- 
glue(
 "
 CREATE OR REPLACE TABLE {DB}_{tbl_name} as 
 (
    WITH admit_cnts as
    (
      SELECT savvy_pid, COUNTIF(EXTRACT(YEAR from admit_dt) = 2018) as admit_counts,
      SUM(IF(EXTRACT(YEAR from admit_dt) = 2017, ip_days, 0)) as hosp_days
       FROM {DB}_{tbl_name}
       GROUP BY savvy_pid
    ),
    
    snf_days as 
    (
      SELECT savvy_pid, SUM(ip_days) as snf_days
      FROM
      (
        {q_snf_visits_2017}
      )
      GROUP BY savvy_pid
    ),
    
    condition_flags as 
    ({q_conditions_2017}),
    
    costs as 
    ({q_costs_2018}),
    
    death_dates as
    ({q_deaths_2018})
    
    SELECT a.savvy_pid
    , a.gender
    , CASE WHEN b.admit_counts is NULL then 0 else admit_counts end as admit_counts
    , IFNULL(b.hosp_days,0) as hosp_days
    , IFNULL(c.snf_days,0) as snf_days
    , {ff_null_qr}
    , {cc_null_qr}
    , (2017 - d.birth_year) as age_2017
    , IFNULL(g.hosp_spend, 0) as hosp_spend
    , IFNULL(g.snf_spend, 0) as snf_spend
    , IFNULL(g.total_spend, 0) as total_spend
    , IFNULL(g.snf_admits, 0) as snf_admits
    , h.date_of_death
    FROM {DB}_pop_ce_2017_2018 as a
    
    LEFT JOIN admit_cnts as b
    ON a.savvy_pid = b.savvy_pid
    
    LEFT JOIN snf_days as c
    ON a.savvy_pid = c.savvy_pid
    
    LEFT JOIN 
    (
      SELECT savvy_pid, birth_year
      FROM df_uhgrd.miniov_member_detail
      GROUP BY savvy_pid, birth_year
    ) d
    ON a.savvy_pid = d.savvy_pid
    
    LEFT JOIN {DB}_frailty_gilbert_2017 as e
    ON a.savvy_pid = e.savvy_pid
    
    LEFT JOIN condition_flags as f
    ON a.savvy_pid = f. savvy_pid
    
    LEFT JOIN costs as g
    ON a.savvy_pid = g.savvy_pid
    
    LEFT JOIN death_dates as h
    ON a.savvy_pid = h.savvy_pid
  
 )

  " 
  )

bq_project_query(x = pid, query = q_admit_cnts_features)


