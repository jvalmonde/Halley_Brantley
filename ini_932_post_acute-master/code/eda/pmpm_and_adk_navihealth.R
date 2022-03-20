library(glue)
devtools::load_all("~/savvyR")
library(tidyverse)

start_dt <- '2017-01-01'
end_dt <- '2017-12-31'
pid <- 'research-01-217611'
DB = "foc_interoperability.ini_1085"  


# MA population 2017 ------------------------------------------------------
# continuously enrolled members in 2017
cohort_ma <- glue("
   SELECT
     distinct savvy_pid
   FROM
     foc_interoperability.ini_1085_pop_ce_2017
               
")

# first part of table name
db_ma <- 'foc_interoperability.ini_1085_ma_2017'


claims_ma <- miniov_medical(start_dt, end_dt, cohort_ma)

hcc_table_ma <- glue("{db_ma}_cms_raf")

## frail, medically complex cohort using NaviHealth criteria
q_navi_ma <- 
  navi_frail_pop(
    hcc_table_ma,
    claims_ma
  )

total_cnt_ma <- load_bq(pid, glue("SELECT COUNT(*) FROM ({cohort_ma})"))
# eligible_cnt_ma <- load_bq(pid, glue("SELECT COUNT(*) FROM ({q_navi_ma})"))
# 
# # 0.02445172
# eligible_cnt_ma/total_cnt_ma

# get PMPM and ADK ---------------------------------------------------------
source('code/modeling/admission_negbinom_model/mod_metrics.R')

# required columns for identified frail population
navi_frail_metrics_2018 <- load_bq(pid, 
                                   query = glue("
              SELECT savvy_pid, admit_counts, mm_2018, total_spend, hosp_spend, snf_spend, snf_admits, date_of_death
              FROM 
              `{DB}_admission_counts_features_non_elect_2017_2018_v2`
              WHERE savvy_pid IN (SELECT savvy_pid FROM ({q_navi_ma}))
            "))



pidsAll <- navi_frail_metrics_2018$savvy_pid
pids0 <- filter(navi_frail_metrics_2018, admit_counts > 0)$savvy_pid
pids1 <- filter(navi_frail_metrics_2018, admit_counts > 1)$savvy_pid
pids2 <- filter(navi_frail_metrics_2018, admit_counts > 2)$savvy_pid

# function excludes members with mm_2018 < 0
make_metrics_table(navi_frail_metrics_2018,pidsAll)
make_metrics_table(navi_frail_metrics_2018,pids0)
make_metrics_table(navi_frail_metrics_2018,pids1)
make_metrics_table(navi_frail_metrics_2018,pids2)

# check the distributon of admission counts
hist(navi_frail_metrics_2018$admit_counts,
     main = 'Histogram of 2018 admission counts')



# CHECKS ------------------------------------------------------------------
# number of members for the denominator: 105,395
navi_clean <- filter(navi_frail_metrics_2018, mm_2018 > 0)%>% 
  mutate(death_flag = ifelse(is.na(date_of_death)| date_of_death > '2019-01-01', 0, 1))

# get proportion of members
mem_cnt <- nrow(navi_clean)
n_distinct(filter(navi_clean, admit_counts > 0)$savvy_pid)/mem_cnt
n_distinct(filter(navi_clean, admit_counts > 1)$savvy_pid)/mem_cnt
n_distinct(filter(navi_clean, admit_counts > 2)$savvy_pid)/mem_cnt


