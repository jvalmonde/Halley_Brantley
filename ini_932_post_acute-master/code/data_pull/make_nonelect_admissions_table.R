#**********************************************************************#
# AUTHOR: Alden Cabajar
# EDITORS:  Karen Chrisele Bioy
# DESCRIPTION: 
#   Table for admissions in 2018 which only contain non-elective admissions
#**********************************************************************#


#========== LOADING DEPENDENCIES ==================================
library(bigrquery)
library(data.table)
library(magrittr)
library(glue)
library(devtools)
library(lubridate)
library(dplyr)

here <- here::here
source(here("code", "data_pull", "fix_hosp_visits.R"))
devtools::load_all(here("savvyR"))

# name of database and table name
DB <- "foc_interoperability.ini_1085"
pid <- 'research-01-217611'


create_non_elect_admissions_table = function(DB, pid, tbl_name, population, base_year, trunc_yearmo = NULL)
{
  
  end_yearmo <- ifelse(is.null(trunc_yearmo), 
                       glue("{base_year}-12"),
                       glue(gsub("(\\d{3})(\\d{2})$", '\\1-\\2', trunc_yearmo)))
  
  end_list <- seq(as.Date(glue('{end_yearmo}-01')), length = 2, by = 'months')-1
  end_date <- end_list[2]
  
  frail_year <- ifelse(is.null(trunc_yearmo), base_year, trunc_yearmo)
  
  # ========= CREATE PRELIMINARY QUERIES =================================
  
  q_hospital_visits <-
    hospital_visits(
      medical_claims = "df_uhgrd.miniov_inpatient_all",
      srvc_type = "srvc_typ_desc",
      hce_srvc = "hce_srvc_typ_desc",
      day_ct = "day_cnt",
      full_dt = "full_dt",
      allw_amt = "deriv_amt",
      start_date = glue("{base_year}-01-01"),
      end_date = glue("{base_year + 1}-12-31"),
      drg = "drg_desc"
    )
  
  
  #### determine number of hospital visits
  q_hosp_vsts_cnt <- glue(
    "SELECT *
      FROM
        (  SELECT
          savvy_pid,
          EXTRACT(YEAR from admit_full_dt) * 100 + EXTRACT(MONTH from admit_full_dt) as year_mo,
          admit_full_dt,
          hst_hlth_pln_srvc_typ_lvl_1_nm,
          hst_hce_srvc_typ_desc,
          SAFE_CAST (fact_ub92_admis_typ_cd as INT64) as admis_typ_cd,
          sdrg_desc,
              max(fact_stat_day) as ip_days,
              sum(fact_allw_amt) as allw_amt
            FROM
              df_uhgrd.miniov_inpatient_all
            WHERE
              admit_full_dt between '{base_year}-01-01' and '{base_year + 1}-12-31'
            GROUP BY
              savvy_pid,
          year_mo,
          admit_full_dt,
          hst_hlth_pln_srvc_typ_lvl_1_nm,
          hst_hce_srvc_typ_desc,
          fact_ub92_admis_typ_cd,
          sdrg_desc
          ) a
      WHERE
        hst_hlth_pln_srvc_typ_lvl_1_nm = 'inpatient'
        AND hst_hce_srvc_typ_desc = 'med/surg/icu'
        AND admis_typ_cd in (1, 2, 5, 9)")
  
  ##### get snf visits
  q_snf_visits <- 
    hospital_visits(
      medical_claims = "df_uhgrd.miniov_medical_claim",
      srvc_type = "srvc_typ_desc",
      hce_srvc = "hce_srvc_typ_desc",
      day_ct = "day_cnt",
      full_dt = "full_dt",
      allw_amt ="deriv_amt",
      start_date = glue("{base_year}-01-01"),
      end_date = end_date,
      ip_type = "skilled nursing"
    )
  
  admit_cnts_pop <- 
    glue('SELECT savvy_pid, year_nbr, year_mo, full_dt, clm_aud_nbr, dx1_diag_cd, dx2_diag_cd, dx3_diag_cd
                   FROM `df_uhgrd.miniov_medical_claim`
                   WHERE year_nbr = {base_year}')
  
  ###### get members chronic condition in 2017
  cc_labels <- cms_ccw %>% 
    mutate(Label = paste0("cc_",Label))
  
  q_conditions <- chronic_conditions(claim_table = admit_cnts_pop,
                                          icd_table = cc_labels,
                                          start_yearmo = glue('{base_year}01'),
                                          end_yearmo = glue(gsub("(\\d{4})-(\\d{2})-(\\d{2})$", '\\1\\2', end_date)))
  
  #### Add CKD stages
  q_ckd_stages <- chronic_conditions(claim_table = admit_cnts_pop,
                                     icd_table = ckd_stages,
                                     start_yearmo = glue('{base_year}01'),
                                     end_yearmo = glue(gsub("(\\d{4})-(\\d{2})-(\\d{2})$", '\\1\\2', end_date)))
    
  
  # =========== FIX HOSPITAL TRANSFERS ==========================================
  
  glue("
    CREATE OR REPLACE TABLE {DB}_{tbl_name} AS (
      SELECT 
        a.savvy_pid,
        a.admit_full_dt AS admit_dt, 
        SUM(a.ip_days) as ip_days,
        0 AS combined_transfers 
      FROM 
        ({q_hosp_vsts_cnt}) a
      JOIN {DB}_{population} b
      ON a.savvy_pid = b.savvy_pid
      GROUP BY 
        savvy_pid, 
        admit_full_dt
    )
  ") %>%
    bq_project_query(x = pid, query = .)
  
  load_bq(
    pid, 
    glue("SELECT COUNT(distinct savvy_pid) FROM {DB}_{tbl_name}")
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
          (EXTRACT(MONTH FROM admit_dt)) AS year_mo
      FROM
       {DB}_{tbl_name}
    )    
  ") %>%
    bq_project_query(x = pid, query = .)
  
  #======= CREATE FINAL TABLE WITH FRAILTY AND CHRONIC CONDITION FEATURES =====================
  
  ##### joining other features to main cohort table
  
  # get total costs, separate hospital admit spend, snf spend, and overall spend
  q_costs <- glue("
      SELECT
        DISTINCT savvy_pid,
        SUM(
          IF
          (srvc_typ_cd = 'ip'
            AND hce_srvc_typ_desc = 'med/surg/icu',
            deriv_amt,
            0)) AS hosp_spend,
        SUM(
          IF
          (srvc_typ_cd = 'ip'
            AND hce_srvc_typ_desc = 'skilled nursing',
            deriv_amt,
            0)) AS snf_spend,
        SUM(deriv_amt) AS total_spend,
        COUNT(DISTINCT
              IF
              (srvc_typ_cd = 'ip'
                AND hce_srvc_typ_desc = 'skilled nursing',
                full_dt,
                NULL)) AS snf_admits
        FROM df_uhgrd.miniov_medical_claim
        WHERE year_nbr = {base_year +1}
        GROUP BY savvy_pid
      ")
  
  # query for death dates
  q_deaths <- death_dates(glue('{DB}_{population}'))
  
  
  # get fraily feature column names
  
  q_ff <- if(is.null(trunc_yearmo)){
    glue("SELECT * FROM research-01-217611.foc_interoperability.INFORMATION_SCHEMA.COLUMNS
    WHERE table_name = 'ini_1085_frailty_gilbert_{base_year}'")
  } else {
    glue("SELECT * FROM research-01-217611.foc_interoperability.INFORMATION_SCHEMA.COLUMNS
    WHERE table_name = 'ini_1085_frailty_gilbert_{trunc_yearmo}'")
  }
  
  ff_col_names <- load_bq(
    pid,
    q_ff
  ) %>%  as.data.table() %>% 
    .[grepl("frail_", column_name)] %>% 
    .[['column_name']]
  
  
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
  
  #convert nulls in CKD stages to zero
  ckd_stage_null_qr <- sapply(
    unique(ckd_stages$Label),
    function(x) {
      glue("IFNULL(i.{x}, 0) as {x}")
    }
  ) %>% 
    glue_collapse(sep = ",")
  
  
  
  ##### Query main table
  q_admit_cnts_features <- 
  glue(
   "
   CREATE OR REPLACE TABLE {DB}_{tbl_name} as 
   (
      WITH admit_cnts as
      (
        SELECT savvy_pid, COUNTIF(EXTRACT(YEAR from admit_dt) = {base_year + 1}) as admit_counts,
        SUM(IF(admit_dt 
                BETWEEN '{base_year}-01-01' AND 
                DATE_SUB(DATE_TRUNC(DATE_ADD('{end_yearmo}-01', INTERVAL 1 month), month), INTERVAL 1 day) , ip_days, 0)) as hosp_days,
        SUM(IF(admit_dt 
                BETWEEN DATE_SUB(DATE_TRUNC(DATE_ADD('{end_yearmo}-01', INTERVAL 1 month), month), INTERVAL 6 month) AND
                DATE_SUB(DATE_TRUNC(DATE_ADD('{end_yearmo}-01', INTERVAL 1 month), month), INTERVAL 1 day), 1, 0)) as admit_6mos_prior_counts
         FROM {DB}_{tbl_name}
         GROUP BY savvy_pid
      ),
      
      snf_days as 
      (
        SELECT savvy_pid, SUM(ip_days) as snf_days
        FROM
        (
          {q_snf_visits}
        )
        GROUP BY savvy_pid
      ),
      
      condition_flags as 
      ({q_conditions}),
      
      ckd_stages as
      ({q_ckd_stages}),
      
      costs as 
      ({q_costs}),
      
      death_dates as
      ({q_deaths})
      
      SELECT a.savvy_pid
      , a.gender
      , CASE WHEN b.admit_counts is NULL then 0 else admit_counts end as admit_counts
      , CASE WHEN a.mm_{base_year+1} is NULL then 0 else mm_{base_year+1} end as mm_{base_year+1}
      , b.admit_6mos_prior_counts
      , IFNULL(b.hosp_days,0) as hosp_days
      , IFNULL(c.snf_days,0) as snf_days
      , {ff_null_qr}
      , {cc_null_qr}
      , {ckd_stage_null_qr}
      ,IFNULL(g.hosp_spend, 0) as hosp_spend
      , IFNULL(g.snf_spend, 0) as snf_spend
      , IFNULL(g.total_spend, 0) as total_spend
      , IFNULL(g.snf_admits, 0) as snf_admits
      , h.date_of_death
      , ({base_year} - d.birth_year) as age_{base_year}
      FROM {DB}_{population} as a
      
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
      
      LEFT JOIN {DB}_frailty_gilbert_{frail_year} as e
      ON a.savvy_pid = e.savvy_pid
      
      LEFT JOIN condition_flags as f
      ON a.savvy_pid = f.savvy_pid
      
      
      LEFT JOIN costs as g
      ON a.savvy_pid = g.savvy_pid
      
      LEFT JOIN death_dates as h
      ON a.savvy_pid = h.savvy_pid
      
      LEFT JOIN ckd_stages as i
      ON a.savvy_pid = i.savvy_pid
    
   )
  
    " 
  )
  
  bq_project_query(x = pid, query = q_admit_cnts_features)
}


####================= Create admission features table  ==================================

# non-elective, continuously enrolled 2017-2018
create_non_elect_admissions_table(DB, pid, 
                                  tbl_name = "admission_counts_features_non_elect_2017_2018",
                                  population = "pop_ce_2017_2018",
                                  base_year = 2017)

# non-elective, continuously enrolled 2017
create_non_elect_admissions_table(DB, pid, 
                                  tbl_name = "admission_counts_features_non_elect_2017_2018_v2",
                                  population = "pop_ce_2017",
                                  base_year = 2017)

# non-elective, continuously enrolled 2018
create_non_elect_admissions_table(DB, pid, 
                                  tbl_name = "admission_counts_features_non_elect_2018_2019",
                                  population = "pop_ce_2018",
                                  base_year = 2018)

# non-elective, continuously enrolled 2018, truncated to simulate data lag
create_non_elect_admissions_table(DB, pid, 
                                  tbl_name = "admission_counts_features_non_elect_2018_trunc",
                                  population = "pop_ce_2018",
                                  base_year = 2018,
                                  trunc_yearmo = 201810)





# =================== CHECKS =======================================================
tbl_name <- "admission_counts_features_non_elect_2017_2018_v2"
admit_cnts <-  load_bq(db = pid,
                 query = glue(
                 "SELECT count(admit_counts) as cnt, admit_counts
                FROM {DB}_{tbl_name}
                GROUP BY admit_counts
                "))
setDT(admit_cnts)

#### print proportions of > cutoff ####
admit_cnts[,prop := cnt/sum(cnt)][]
lapply(c(0, 1, 2), function(x) admit_cnts[admit_counts > x, sum(prop)])


### print member months
mm <- load_bq(db = pid, 
              query = glue(
                "SELECT count(*) as cnt, mm_2018
                 FROM {DB}_{tbl_name}
                 GROUP BY mm_2018"
              ))

mm



