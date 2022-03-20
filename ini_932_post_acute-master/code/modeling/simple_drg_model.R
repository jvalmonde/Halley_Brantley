#**********************************************************************#
# Author : Suchit Mehrotra
# Editor : Halley Brantley
# build a simple logistic model trying to predict 30 day readmit using the
# admission drg, age at admit, and the length of stay
#**********************************************************************#
rm(list = ls())

library(bigrquery)
library(recipes)
library(rsample)
library(modelr)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
load_all("../../savvyR")

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
options(scipen = 20)

set.seed(42)

q_30_day_readmits <-
  glue("
    SELECT
      savvy_pid,
    FROM
      {DB}_readmissions
    WHERE
      days_to_readmit <= 30
  ")

q_admission_reasons <-
  glue("
    SELECT
      a.savvy_pid,
      b.drg_desc
    FROM
      {DB}_pop AS a
    LEFT JOIN
      `df_uhgrd.miniov_medical_claim` AS b
    ON
      a.savvy_pid = b.savvy_pid
    WHERE
      b.hce_srvc_typ_desc = 'med/surg/icu'
      AND (b.full_dt BETWEEN a.admit_dt AND a.discharge_dt)
    GROUP BY
      a.savvy_pid,
      b.drg_desc
    ORDER BY
      a.savvy_pid
  ")

response_and_demog <-
  glue("
    SELECT
      a.savvy_pid,
      a.admit_dt,
      a.age_at_admit,
      a.gender,
      a.ip_days,
      CASE
        WHEN a.savvy_pid IN ({q_30_day_readmits}) THEN 1
        ELSE 0
      END AS readmit_30_flag
    FROM
      {DB}_pop AS a
    LEFT JOIN
      ({q_admission_reasons}) AS b
    ON
      a.savvy_pid = b.savvy_pid
    GROUP BY
      a.savvy_pid,
      a.admit_dt,
      a.age_at_admit,
      a.gender,
      a.ip_days
  ") %>%
   load_bq(db = pid, query = .)

drg_flags <-
  load_bq(db = pid, query = q_admission_reasons)

drg_mapping <-
  drg_flags %>%
  select(drg_desc) %>%
  distinct() %>%
  arrange(drg_desc) %>%
  mutate(drg_col_name = paste0("drg", 1:n()))

drg_flags_wide <-
  drg_flags %>%
  left_join(drg_mapping, by = "drg_desc") %>%
  arrange(drg_desc) %>%
  select(savvy_pid, drg_col_name)  %>%
  mutate(indicator = 1) %>%
  pivot_wider(
    names_from = "drg_col_name", values_from = "indicator",
    values_fill = list(indicator = 0)
  )

response_drg <-
  response_and_demog %>%
  inner_join(drg_flags_wide, by = "savvy_pid") %>%
  select(savvy_pid, readmit_30_flag, age_at_admit, gender, ip_days, starts_with("drg"))

# clean up
rm(drg_flags_wide)
rm(response_and_demog)
rm(drg_flags)

#----- model fitting stpes -----#
# drop drgs with less than 0.2% non-zero
drg_props<- 
  response_drg  %>%
  select(starts_with("drg")) %>%
  summarise(across(.fns = mean)) %>%
  unlist() 

top_drgs <- which(drg_props > 0.002) %>% names()
rm(drg_sds)

# subset df to those drgs
final_df <-
  response_drg %>%
  mutate(age_group = cut(age_at_admit, c(0, 65, 70, 75, 80, 85, 120))) %>%
  select(
    savvy_pid, readmit_30_flag, age_group, gender, ip_days, 
    one_of(top_drgs)
  )

train_test_split <- initial_split(final_df)
train_df <- training(train_test_split)
test_df <- testing(train_test_split)

# ----- fit the model ----- #
glm_fit <- glm(
  readmit_30_flag ~ . - savvy_pid,
  data = train_df,
  family = binomial(link = "logit")
)

# save data for coefficient plots
save(glm_fit, drg_mapping, file = "../../data/glm_fit.RData")

# ----- predictions and calibration plot for model ----- #
preds_test <- predict(object = glm_fit, newdata = test_df, type = "response")
y_test <- test_df %>% select(readmit_30_flag) %>% unlist()

calib_plot <- function(y_test, preds_test, n_buckets = 10)
{
  prob_buckets <- quantile(
    preds_test, probs = seq(0, 1, length = n_buckets + 1)
  )
  estim_probs = rep(NA, n_buckets)
  true_probs = rep(NA, n_buckets)

  for(i in 2:length(prob_buckets))
  {
    lower <- prob_buckets[i-1]
    upper <- prob_buckets[i]
    inds <- (preds_test <= upper) & (preds_test >= lower)
    true_probs[i - 1] <- y_test[inds] %>% mean()
    estim_probs[i - 1] <- preds_test[inds] %>% mean()
  }

  list(
    cutoffs = prob_buckets, true_probs = true_probs, estim_probs = estim_probs
  ) %>%
    return()
}

n_buckets = 20
calib_dat_glm <- calib_plot(y_test, preds_test, n_buckets)

plot(calib_dat_glm$estim_probs, calib_dat_glm$true_probs)
abline(a = 0, b = 1)

# ----- add predictions to the data frame ----- #
ids_w_preds <- 
  test_df %>%
  add_predictions(model = glm_fit, type = "response") %>%
  mutate(pred_quant = ntile(pred, n_buckets)) %>%
  select(savvy_pid, pred, readmit_30_flag, pred_quant)

bq_project_query(x = pid, query = glue("drop table {DB}_basic_drg_preds"))

# upload predicitons to GBQ
id_preds_tab <- bq_table(
  project = pid,
  dataset = "foc_interoperability", 
  table = "ini_932_basic_drg_preds"
)

bq_table_upload(
  x = id_preds_tab, 
  values = ids_w_preds, 
  create_disposition = "CREATE_IF_NEEDED", 
  fields = as_bq_fields(ids_w_preds)
)
