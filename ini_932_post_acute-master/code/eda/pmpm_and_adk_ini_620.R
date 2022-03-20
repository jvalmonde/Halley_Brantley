library(lubridate)
library(here)
library(readr)
devtools::load_all("savvyR")

pid = "research-01-217611"
DB = "foc_interoperability.ini_1085"  
options(scipen = 20)

devtools::load_all("~/ini_620_home_complex_risk_strat/homeriskstratR")
source(here::here("code/modeling/admission_negbinom_model/", "mod_metrics.R"))
ini_620_population_codes <- read_csv("data/ini_620_population_codes.csv")

## get 2017 cohort
q_data_620 <- get_cohort(start_dt = '2017-01-01', ini_620_codes = ini_620_population_codes)

data_620 <- load_bq(pid, q_data_620)

# required columns for identified 620 population
mem_620_2018 <- load_bq(pid, query = glue("
              SELECT savvy_pid, admit_counts, mm_2018, total_spend, hosp_spend, snf_spend, snf_admits, date_of_death
              FROM 
              `{DB}_admission_counts_features_non_elect_2017_2018_v2`
              WHERE savvy_pid IN (SELECT savvy_pid FROM ({q_data_620}))
            "))

# filter out members with mm == 0 in 2018
mem_620_clean <- mem_620_2018 %>% filter(mm_2018 > 0)

make_metrics_table(mem_620_2018,data_620$savvy_pid)
