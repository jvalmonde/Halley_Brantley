#' Flag hospital visits that end in transfers or begin as transfers to other hospitals
#'
#' \code{add_transfer_flag} Updates the input table with 6 new fields: 
#' prev_admit, next_admit, prev_discharge, next_discharge, prev_ip_days, 
#' next_ip_days, incomplete_admit, and is_transfer
#'
#' @param hosp_visit_tab GBQ table that contains hospital admissions, 
#' needs to contain fields for savvy_pid, admit_dt, and ip_days
#' @import glue
#' @export
add_transfer_flag <- function(hosp_visit_tab)
{
  # first get the discharge date of the admit and what the next admit date was
  q_lead_lag <- glue("
      SELECT
        savvy_pid, 
        admit_dt, 
        ip_days,
        combined_transfers, 
        DATE_ADD(admit_dt, INTERVAL ip_days DAY) AS discharge_dt, 
        LEAD(admit_dt) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
          next_admit,
        LAG(admit_dt) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
          prev_admit
      FROM 
        {hosp_visit_tab}
      ORDER BY
        savvy_pid, 
        admit_dt
  ") 
  
  # also add what the previous discharge date was and what the next value for
  # the length of the stay is
  
  q_los <- glue("
      SELECT
        savvy_pid,
        combined_transfers,
        ip_days,
        admit_dt,
        discharge_dt,
        prev_admit,
        LAG(discharge_dt) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
          prev_discharge,
        next_admit,
        LEAD(discharge_dt) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
          next_discharge,
        LAG(ip_days) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
          prev_ip_days,
        LEAD(ip_days) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
          next_ip_days
      FROM 
        ({q_lead_lag}) ll
  ") 
  
  # add flags for transfers and incomplete admits
  glue("
  CREATE OR REPLACE TABLE {hosp_visit_tab} AS (
    SELECT
      *,
      CASE 
        WHEN DATE_DIFF(next_admit, discharge_dt, DAY) <= 0 THEN 1
        ELSE 0
      END AS incomplete_admit,
      CASE 
        WHEN DATE_DIFF(prev_discharge, admit_dt, DAY) >= 0 THEN 1
        ELSE 0
      END AS is_transfer,
    FROM 
      ({q_los}) los
    ORDER BY
      savvy_pid, 
      admit_dt
  )
  ") %>%
    bq_project_query(x = pid, query = .)
}


#' Fix hospital visits that end in transfers or begin as transfers to other hospitals
#'
#' \code{fix_hosp_visits} Combines all transfers into single visit, with single 
#' admit date and discharge date
#'
#' @param hosp_visit_tab GBQ table that contains hospital admissions, 
#' needs to contain fields for savvy_pid, admit_dt, and ip_days
#' @param combined_visit_tab Name of new table to store combined visits
#' @import glue
#' @export
fix_hosp_visits <- 
  function(
    hosp_visit_tab,
    combined_visit_tab)
{
  q_transfer_lag <- glue("
    SELECT
      *,
      LAG(is_transfer) OVER(PARTITION BY savvy_pid ORDER BY admit_dt) AS 
        transfer_lag
    FROM 
      {hosp_visit_tab}
    ORDER BY
      savvy_pid, 
      admit_dt
  ")
  
  glue("
  CREATE OR REPLACE TABLE {combined_visit_tab} AS (
    SELECT 
      savvy_pid, 
      CASE 
        WHEN is_transfer = 1 AND transfer_lag = 0 THEN 
          ip_days + prev_ip_days
        ELSE ip_days
      END AS ip_days,
      CASE 
        WHEN is_transfer = 1 AND transfer_lag = 0 THEN
          prev_admit
        ELSE admit_dt
      END AS admit_dt,
      CASE 
        WHEN combined_transfers = 1 THEN 1 
        WHEN is_transfer = 1 AND transfer_lag = 0 THEN 1
        ELSE 0 END AS combined_transfers
    FROM 
       ({q_transfer_lag})
    WHERE
      (incomplete_admit = 1 AND is_transfer = 1)
      OR incomplete_admit = 0
  )
  ") %>%
    bq_project_query(x = pid, query = .)
}