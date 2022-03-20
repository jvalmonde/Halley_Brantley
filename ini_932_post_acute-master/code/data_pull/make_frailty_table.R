
#**********************************************************************#
# AUTHOR: Karen Chrisele Bioy and Alden Cabajar
# EDITORS: Halley Brantley
# DESCRIPTION: Create bigquery table for frailty indicators
# INPUTS: 
# OUTPUTS: 
#**********************************************************************#

rm(list = ls())

library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
library(devtools)
devtools::load_all("../savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_1085'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)

#### QUERY FOR INI-932 POPULATION ####
# load frail categories dataset from ini_936: frail_diag_gilbert_categories.csv
frail_diag_gilbert_categories <- 
  read_csv('data/frail_diag_gilbert_categories.csv')

frail_diags <- 
  frail_diag_gilbert_categories %>% 
  mutate(Label = paste0("frail_",Label))

# diagnosis information for pac population
pac_pop_dx <- glue('
SELECT
  savvy_pid,
  year_nbr,
  full_dt,
  clm_aud_nbr,
  dx1_diag_cd,
  dx2_diag_cd,
  dx3_diag_cd
FROM
  `df_uhgrd.miniov_medical_claim`
WHERE
  savvy_pid IN (
  SELECT
    DISTINCT savvy_pid
  FROM
    `foc_interoperability.ini_932_pop`)
  AND year_nbr IN (2017,
    2018)
')

# query for frailty indicator variables
q_frail_diags <- condition_flag(
  claim_table = pac_pop_dx,
  icd_table = frail_diags,
  condition_col = "Label",
  icd_col = "dx1_diag_cd",
  date_field = "full_dt",
  dx_fields = c("dx1_diag_cd", "dx2_diag_cd", "dx3_diag_cd"),
  strict = FALSE,
  total_flag = NULL,
  group_vars = c("savvy_pid", "year_nbr")
)

# query to create the table in bigquery
ini_1085_frailty_gilbert<- glue("
create or replace table 
`research-01-217611.foc_interoperability.ini_1085_frailty_gilbert`
  options(
     description='1 Row per member per year for 2017-2018. Flag = 1 if frailty indicator is met at least once in year.'
  ) as 
  
  {q_frail_diags}
")

# Create table in BigQuery
bq_project_query(pid, query=ini_1085_frailty_gilbert)

#### QUERY FOR INI-1085 POPULATION, ON ADMISSION COUNTS MODEL (2017-2018) ####


# FRAILTY FEATURES BY YEAR ------------------------------------------------

frail_diag_gilbert_categories <- 
  read_csv('data/frail_diag_gilbert_categories.csv')

frail_diags <- 
  frail_diag_gilbert_categories %>% 
  mutate(Label = paste0("frail_",Label))

make_frailty_table <- function(DB, pid, population, year, end_yearmo = NULL){

  admit_cnts_pop <- if(is.null(end_yearmo)){
      glue('SELECT savvy_pid, year_nbr, full_dt, clm_aud_nbr, dx1_diag_cd, dx2_diag_cd, dx3_diag_cd
                 FROM `df_uhgrd.miniov_medical_claim`
                 WHERE savvy_pid IN (SELECT DISTINCT savvy_pid FROM `{DB}_{population}`)
                 AND  year_nbr = {year}')
    } else {
      glue('SELECT savvy_pid, year_nbr, full_dt, clm_aud_nbr, dx1_diag_cd, dx2_diag_cd, dx3_diag_cd
                 FROM `df_uhgrd.miniov_medical_claim`
                 WHERE savvy_pid IN (SELECT DISTINCT savvy_pid FROM `{DB}_{population}`)
                 AND  year_mo between {year}01 and {end_yearmo}')
    } 
      
  
  

q_frail_diags <- condition_flag(
  claim_table = admit_cnts_pop,
  icd_table = frail_diags,
  condition_col = "Label",
  icd_col = "dx1_diag_cd",
  date_field = "full_dt",
  dx_fields = c("dx1_diag_cd", "dx2_diag_cd", "dx3_diag_cd"),
  strict = FALSE,
  total_flag = NULL,
  group_vars = c("savvy_pid", "year_nbr")
)

# query to create the table in bigquery for admit counts model (2017-2018)
ini_1085_frailty_gilbert_admit_cnts <- if(is.null(end_yearmo)){
  glue(" create or replace table `{pid}.{DB}_frailty_gilbert_{year}`
  options(
     description='1 Row per member per year for {year}. Flag = 1 if frailty indicator is met at least once in year. Population basis is continuously enrolled medicare members for {year}'
  ) as 
  {q_frail_diags}
")
} else {
  glue("
create or replace table `{pid}.{DB}_frailty_gilbert_{end_yearmo}`
  options(
     description='1 Row per member per year for {year}, up until {end_yearmo}. Flag = 1 if frailty indicator is met at least once in year. Population basis is continuously enrolled medicare members for {year}'
  ) as 
  
  {q_frail_diags}
")
}


# Create table in BigQuery
bq_project_query(pid, query=ini_1085_frailty_gilbert_admit_cnts)

return(admit_cnts_pop)
}

# create the tables
# to be used in the qtr table
# for 2017 continuously enrolled population
admit_cnts_pop <- make_frailty_table(DB,
                   pid, 
                   population = 'pop_ce_2017',
                   year = 2017)

# for 2018 continuously enrolled population
admit_cnts_pop <- make_frailty_table(DB,
                   pid, 
                   population = 'pop_ce_2018',
                   year = 2018)

# for 2018 continuously enrolled population, truncated at 201810 to simulate lag in claims data
admit_cnts_pop <- make_frailty_table(DB,
                                     pid, 
                                     population = 'pop_ce_2018',
                                     year = 2018,
                                     end_yearmo = 201810)



# FRAILTY FEATURES BY QUARTER ---------------------------------------------
### ADDING FRAILTY FEATURES FOR EACH TIME WINDOW
# create fraily features by date first
# change year to create different tables
year = 2018
# if truncated use this instead as table name extension
year = 201810


q_frail_diags_full_dte <- condition_flag(
  claim_table = admit_cnts_pop,
  icd_table = frail_diags,
  condition_col = "Label",
  icd_col = "dx1_diag_cd",
  date_field = "full_dt",
  dx_fields = c("dx1_diag_cd", "dx2_diag_cd", "dx3_diag_cd"),
  strict = FALSE,
  total_flag = NULL,
  group_vars = c("savvy_pid", "full_dt")
) 

tbl_name <- glue("frail_feats_by_quarter_{year}")

query = glue(
  "
 CREATE OR REPLACE TABLE {{DB}_{{tbl_name}_tmp as
 (
   WITH q_df as
   (
     SELECT *
     , EXTRACT(QUARTER FROM full_dt) as quarter
     FROM
     (
      {{q_frail_diags_full_dte}
     )
   )
   , agg_by_q as
   (
      SELECT savvy_pid
      , quarter
     , CASE WHEN SUM(frail_Falls_and_Fractures) > 0 THEN 1 ELSE 0 end as frail_Falls_and_Fractures
     , CASE WHEN SUM(frail_Pressure_Ulcers_and_Weight_Loss) > 0 THEN 1 ELSE 0 end as frail_Pressure_Ulcers_and_Weight_Loss
     , CASE WHEN SUM(frail_Incontinence) > 0 THEN 1 ELSE 0 end as frail_Incontinence
     , CASE WHEN SUM(frail_Anxiety_and_Depression) > 0 THEN 1 ELSE 0 end as frail_Anxiety_and_Depression
     , CASE WHEN SUM(frail_Mobility_Problems) > 0 THEN 1 ELSE 0 end as frail_Mobility_Problems
     , CASE WHEN SUM(frail_Dependence_and_Care) > 0 THEN 1 ELSE 0 end as frail_Dependence_and_Care
     , CASE WHEN SUM(frail_Dementia_and_Delirium) > 0 THEN 1 ELSE 0 end as frail_Dementia_and_Delirium
     FROM q_df
     GROUP BY savvy_pid, quarter
   )
   , wide_to_long as
   (
      SELECT savvy_pid
      , quarter
      , ff 
      , CASE WHEN SUM(SAFE_CAST(flag AS INT64)) > 0 THEN 1 ELSE 0 END AS flag
      FROM (
        SELECT savvy_pid, quarter, 
          REGEXP_REPLACE(SPLIT(pair, ':')[OFFSET(0)], r'^\"|\"$', '') ff, 
          REGEXP_REPLACE(SPLIT(pair, ':')[OFFSET(1)], r'^\"|\"$', '') flag 
        FROM agg_by_q  t, 
        UNNEST(SPLIT(REGEXP_REPLACE(to_json_string(t), r'{|}', ''))) pair
      )
      WHERE NOT LOWER(ff) IN ('savvy_pid', 'quarter')
      GROUP BY savvy_pid, quarter, ff
   )
   
   SELECT *, CONCAT(ff, '_Q' , SAFE_CAST(quarter AS STRING)) as tmp
   FROM wide_to_long
    
 )
  ",
  .open = "{{"
)

bq_project_query(x = pid, query = query)


## A query to create a query for pivoting operations (long to wide)
## In this case, we pivot Quarter variables of frailty features.
qr2 <- 
glue(
  "
  EXECUTE IMMEDIATE
   (
      SELECT 'CREATE OR REPLACE TABLE {DB}_{tbl_name} AS ( SELECT savvy_pid, ' || 
     STRING_AGG(
          'MAX(IF(tmp = \"' || tmp || '\", flag, 0)) as \`' || tmp || '\`'
     ) 
     || ' FROM {DB}_{tbl_name}_tmp  GROUP BY savvy_pid ORDER BY savvy_pid)'
      FROM (
        SELECT tmp 
        FROM {DB}_{tbl_name}_tmp
        GROUP BY tmp
        ORDER BY tmp
      )
  )
  "
)

bq_project_query(x = pid, query = qr2)

## for checking the query
writeLines(query, "querry_ff_by_quarter.txt")

