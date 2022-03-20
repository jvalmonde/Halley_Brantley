# Create HCC tables 

library(devtools)
library(glue)
library(tidyverse)
load_all("~/savvyR")

options(httr_oauth_cache="~/.httr-oauth",httr_oob_default = TRUE)
httr::set_config(httr::config(ssl_verifypeer = 0L))

pid <- 'research-01-217611'

cohort <- 
  duals(
    start_yearmo = 201901,
    end_yearmo = 201912,
    ce = TRUE
  )

start_dt <- '2019-01-01'
end_dt <- '2019-12-31'

db <- 'foc_interoperability.ini_932_duals'

hccs(
    start_dt, 
    end_dt,
    cohort,
    db,
    pid = 'research-01-217611'
  )

claims <- miniov_medical(start_dt, end_dt, cohort)

hcc_table <- glue("{db}_cms_raf")
load_bq(pid, glue("SELECT COUNT(*) FROM {hcc_table}"))

q_navi_dual <- 
  navi_frail_pop(
    hcc_table,
    claims
  )

total_cnt <- load_bq(pid, glue("SELECT COUNT(*) FROM ({cohort})"))
eligible_cnt <- load_bq(pid, glue("SELECT COUNT(*) FROM ({q_navi_dual})"))

eligible_cnt/total_cnt
#################################################################

# Non-SNP, MA population

cohort_ma <- glue("
   SELECT
     savvy_pid,
     COUNT(DISTINCT year_mo) AS mm
   FROM
     (  SELECT 
    savvy_pid, 
    year_mo
  FROM 
    df_uhgrd.miniov_member_detail
  WHERE
    sub_product  IN ('hmo', 'local ppo', 'regional ppo', 'hmopos')
    AND year_mo BETWEEN 201901 AND 201912)
   GROUP BY
     savvy_pid
   HAVING COUNT(DISTINCT year_mo) = 12               
               
")

db_ma <- 'foc_interoperability.ini_932_ma'

hccs(
  start_dt, 
  end_dt,
  cohort_ma,
  db_ma,
  pid = 'research-01-217611'
)

claims_ma <- miniov_medical(start_dt, end_dt, cohort_ma)

hcc_table_ma <- glue("{db_ma}_cms_raf")

q_navi_ma <- 
  navi_frail_pop(
    hcc_table_ma,
    claims_ma
  )

total_cnt_ma <- load_bq(pid, glue("SELECT COUNT(*) FROM ({cohort_ma})"))
eligible_cnt_ma <- load_bq(pid, glue("SELECT COUNT(*) FROM ({q_navi_ma})"))

eligible_cnt_ma/total_cnt_ma


load_bq(pid, glue("SELECT AVG(total_raf) FROM ({q_navi_ma})"))
