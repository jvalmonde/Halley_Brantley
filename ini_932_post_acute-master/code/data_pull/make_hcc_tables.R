#**********************************************************************#
# AUTHOR: Karen Chrisele Bioy 
# EDITORS:
# DESCRIPTION: Create HCC table based on navihealth algorithm 
#   and another table to map HCCs to corresponding serious conditions
# INPUTS: 
# OUTPUTS: 
#**********************************************************************#

library(glue)
library(tidyverse)
library(stringr)
library(bigrquery)
devtools::load_all('savvyR')

make_hcc_table <- function(year, end_date){
  
  ## Arguments:
  # year - 
  # end_date - 
  
  start_dt <- paste0(year,'-01-01')
  end_dt <- end_date
  pid <- 'research-01-217611'

# HCC table for MA population ----------------------------------------
# continuously enrolled members
cohort_ma <- glue("
   SELECT
     distinct savvy_pid
   FROM
     foc_interoperability.ini_1085_pop_ce_{year}
               
")
  
# first part of table name
db_ma <- glue("foc_interoperability.ini_1085_ma_{format(as.Date(end_date), '%Y%m')}")

# generates a big query table with hcc flags and raf score
# table name ends in '_cms_raf'
hccs(
  start_dt, 
  end_dt,
  cohort_ma,
  db_ma,
  pid = 'research-01-217611'
)


# Mapping HCCs to conditions ----------------------------------------------


enrichment_hccs <- hccs_navi %>% filter(!HCC %in% c("HCC138", "HCC51", "HCC59", "HCC60"))
hcc_conditions <- as.vector(na.omit(unique(enrichment_hccs$Condition)))
labels <- paste0('serious_', hcc_conditions)

if_statements <- {} ; for (i in 1:length(hcc_conditions)){
  
  x <- filter(enrichment_hccs, Condition %in% hcc_conditions[i]) %>% 
    select(-Description) %>% 
    mutate(hcc = paste0('hcc',str_pad(str_sub(HCC, 4, 7), 3, side = 'left', pad = '0')))
  
  if_statements[i] <- glue('IF({paste(x$hcc, collapse = "+")} > 0, 1, 0) AS {labels[i]}')
  
}


q_hcc_conditions <- glue("
SELECT * 
FROM(
WITH
  hcc_flag AS (
  SELECT
    CAST(savvy_pid AS string) savvy_pid,
    CAST(total_raf AS string) total_raf,
    * EXCEPT(savvy_pid,
      total_raf)
  FROM
    `foc_interoperability.ini_1085_ma_{format(as.Date(end_date), '%Y%m')}_cms_raf`)
SELECT
  CAST(savvy_pid as int64) savvy_pid,
  CAST(total_raf as float64) total_raf,
  hcc_count
FROM (
  SELECT
    savvy_pid,
    total_raf,
    (
    SELECT
      SUM(CAST(value AS int64))
    FROM
      UNNEST(REGEXP_EXTRACT_ALL(to_json_STRING(hcc_flag), r':(\\d+),?')) value ) hcc_count
  FROM
    hcc_flag)) AS a
    
LEFT JOIN (
  SELECT savvy_pid,
    {glue_collapse(if_statements,', \n')}
  FROM
    `foc_interoperability.ini_1085_ma_{format(as.Date(end_date), '%Y%m')}_cms_raf`
) as b
USING (savvy_pid)
")

# query to create the table in bigquery
ini_1085_serious_conditions<- glue("
CREATE OR REPLACE TABLE
`research-01-217611.foc_interoperability.ini_1085_serious_conditions_{format(as.Date(end_date), '%Y%m')}`
  options(
     description='1 Row per member. Contains information on a members raf score, number of HCCs in {year}, and serious illness indicators from the serious illness algorithm.'
  ) as 
  
  ({q_hcc_conditions})
")

# Create table in BigQuery
bq_project_query(pid, query=ini_1085_serious_conditions)
}


# make tables -------------------------------------------------------------
# tables can be found in bigquery
# year input also corresponds to year of continuous enrollment
# only available for years 2017 and 2018

# 2018 cms_raf (hcc) table
make_hcc_table(year = 2017, end_date = '2017-12-31')

# 2018 cms_raf (hcc) table
make_hcc_table(year = 2018, end_date = '2018-12-31')

# truncated 2018 cms_raf (hcc) table
make_hcc_table(year = 2018, end_date = '2018-10-31')




       