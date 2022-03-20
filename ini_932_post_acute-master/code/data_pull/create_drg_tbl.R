#**********************************************************************#
# AUTHOR: Alden Cabajar
# EDITORS:
# DESCRIPTION: script to create 
# INPUTS: 
# OUTPUTS: 
#**********************************************************************#


library(data.table)
library(bigrquery)
library(glue)


### join hosp visits to readmissions and discharge dt
ds <- 'ini_1085'
proj <- 'research-01-217611'
query <- 
glue("
  create or replace table foc_interoperability.{ds}_drg_history_prior_readmission as
  (
  with w_readmit as 
  (
    select a.savvy_pid
    ,a.discharge_dt
    ,a.admit_dt as first_admit_dt
    ,b.readmit_dt
    ,b.days_to_readmit
    from foc_interoperability.ini_932_pop as a
    left join foc_interoperability.ini_932_readmissions as b
    on a.savvy_pid = b.savvy_pid
  )
  
  select a.*
  ,b.* except (savvy_pid)
  from w_readmit as a
  join foc_interoperability.ini_932_2018_hosp_visits as b
  on a.savvy_pid = b.savvy_pid
  
  )
  "
)

bq_project_query(x = proj, query = query)

