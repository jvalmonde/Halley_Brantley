#**********************************************************************#
# AUTHOR: Alden Cabajar
# EDITORS:
# DESCRIPTION: 
#   Answers the following questions:
#   1. Total cost of SNF for top 3 deciles
#   2. SNF is responsible for X% of 30-day cost?
#   3. Can we determine the % of SNF cost associated with a primary admission vs readmission
#**********************************************************************#
library(bigrquery)
library(data.table)
library(glue)
library(devtools)
library(lubridate)
load_all("savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)


### Total cost of SNF for top 3 deciles
# Top 3 cost deciles are at 8-10
top_3_cost_dec <- glue("SELECT * 
     FROM {DB}_cost_categories
      WHERE spend_30_decile in (8,9,10) and hce_srvc_typ_desc = 'skilled nursing'")

top_3_cost_dec_tbl <- load_bq(db = pid, query = top_3_cost_dec)
readr::write_csv(top_3_cost_dec_tbl, "../../figures/top_3_decile_total_cost.csv")


### SNF is responsible for X% of 30-day cost?
pct_30_day_cst <- 
  glue(
   "SELECT sum(total_cost)/(SELECT SUM(total_cost) FROM {DB}_cost_categories) as pct
     ,sum(total_cost) as total_cost
    ,age_group
    FROM  {DB}_cost_categories
    WHERE hce_srvc_typ_desc = 'skilled nursing'
    GROUP BY age_group
   " 
  )
pct_30_day_cst_tbl <- load_bq(db = pid, query = pct_30_day_cst)
readr::write_csv(pct_30_day_cst_tbl, "figures/pct_30_day_cost_due_to_snf.csv")

### % of SNF cost associated with primary admission vs readmission
snf_cost_assoc <- 
  glue("
    SELECT 
      after_readmit,
      SUM(snf_cost) AS snf_cost,
      COUNT(*) AS snf_admits,
      COUNT(DISTINCT savvy_pid) as snf_members,
      SUM(snf_days) AS snf_days
    FROM 
       {DB}_snf
    GROUP BY
      after_readmit
 ") %>%
load_bq(pid, .)

snf_cost_assoc %>%
  mutate(prop = snf_cost/sum(snf_cost))

snf_cost_assoc$snf_cost/1e09
  
snf_time <- 
  glue("
    SELECT 
      *
    FROM 
       {DB}_snf
    WHERE 
       after_readmit = 0
 ") %>%
  load_bq(pid, .)

mean(snf_time$snf_days_from_dis == 0)

snf_readmit_time <- 
  glue("
    SELECT 
      *
    FROM 
       {DB}_snf
    WHERE 
       after_readmit = 1
 ") %>%
  load_bq(pid, .)

snf_readmit_time %<>% 
  mutate(snf_days_from_dis2 = snf_admit_dt - readmit_discharge_dt)
mean(snf_readmit_time$snf_days_from_dis2 == 0, na.rm = T)
