library(here)
library(tidyverse)
library(drake)
library(caret)
library(doParallel)
library(data.table)

pid = "research-01-217611"
DB = "foc_interoperability.ini_1085"  
options(scipen = 20)
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
devtools::load_all(here("../savvyR"))

cache <- new_cache(path = "/storage_pool/cache/")
# targets in cache
cache_list <- cached(cache = cache)

# load all fitted models
loadd(starts_with("models"), cache = cache)
models <- lapply(ls(pattern = "models_"), get)
model_names <- gsub('models_', '', ls(pattern = 'models_'))
names(models) <- model_names
# rearrange models according to complexity
models <- models[c('orig_model', 'w_hcc5_plus', 'w_intrct', 'w_admits', 'w_serious_cnd', 'w_ckd_stages')]

# remove to free up space
rm(list = ls(pattern = 'models_'))

# pull 2018-2019 data -----------------------------------------------------
query_2018 <-  glue("SELECT *
              FROM 
              (
                SELECT *
                FROM
                  `{DB}_admission_counts_features_non_elect_2018_2019`
                WHERE
                  age_2018 >= 65
              ) AS a
              LEFT JOIN (
                SELECT *
                FROM
                  `{DB}_frail_feats_by_quarter_2018`) AS b
              USING
                (savvy_pid)
              LEFT JOIN (
                SELECT * EXCEPT (total_raf)
                FROM
                  `{DB}_serious_conditions_2018`) as c
              USING (savvy_pid)")

data_2018 <- load_bq(pid, query_2018)
names(data_2018)[colnames(data_2018) == 'CKD_other'] <- 'CKD_Other'


query_2018_lag <-  glue("SELECT *
              FROM 
              (
                SELECT *
                FROM
                  `{DB}_admission_counts_features_non_elect_2018_trunc`
                WHERE
                  age_2018 >= 65
              ) AS a
              LEFT JOIN (
                SELECT *
                FROM
                  `{DB}_frail_feats_by_quarter_201810`) AS b
              USING
                (savvy_pid)
              LEFT JOIN (
                SELECT * EXCEPT (total_raf)
                FROM
                  `{DB}_serious_conditions_201810`) as c
              USING (savvy_pid)")

data_2018_lag <- load_bq(pid, query_2018_lag)
names(data_2018_lag)[colnames(data_2018_lag) == 'CKD_other'] <- 'CKD_Other'

# preprocess data ---------------------------------------------------------

# preprocessing function for admit count dataframes
preprocess_data = function(df)
{
  # create hcc_over5_flag
  df <- df %>% 
    mutate(hcc_over5_flag = ifelse(hcc_count > 5, 1,0),
           prior_admit_1plus = ifelse(admit_6mos_prior_counts >= 1, 1, 0),
           prior_admit_2plus = ifelse(admit_6mos_prior_counts >= 2, 1, 0))
  
  # vars not included
  vars_not_included <- c("hosp_spend", "snf_spend", "total_spend", "snf_admits",
                         "date_of_death", "mm_2019", "gender", "hcc_count")
  df <- df %>% 
    mutate_if(!(names(.) %in% vars_not_included), ~ifelse(is.na(.), 0, .))
  
  df$gender <- factor(df$gender)
  df$age_cat <- cut(
    df$age_2018,
    breaks = c(65, 70, 75, 80, max(df$age_2018) +
                 1),
    labels = c("65-69", "70-74", "75-79", "80+"),
    right = F
  )
  dmy <- dummyVars(~ age_cat, data = df)
  dmy_df <- predict(dmy, newdata = df)
  colnames(dmy_df) <-  make.names(colnames(dmy_df))
  
  df <- cbind(df, dmy_df)
  
  # use LACE index LOS cutoffs as breaks
  df$hosp_cat <- cut(
    df$hosp_days,
    breaks = c(0, 1, 2, 3, 4,7, 14, max(df$hosp_days) +
                 1),
    labels = c("0", "1", "2", "3", "4-6", "7-13", ">=14"),
    right = F
  )
  
  
  # breaks based on medicare coverage days
  df$snf_cat <- cut(
    df$snf_days,
    breaks = c(0, 21, 101, max(df$snf_days) +
                 1),
    labels = c("0-20", "21-100", ">100"),
    right = F
  )
  
  #filter out members with more than 200 ip days
  # df <- df %>% 
  #   filter(hosp_days <= 200 & snf_days <= 200)
  return(df)

}


## preprocess data
## use data_2018_lag for truncated data
data_clean <- preprocess_data(data_2018)
rm(data_2018)


# get metrics -------------------------------------------------------------
source(here("code/modeling/admission_negbinom_model/", "mod_metrics.R"))


# make metrics table ------------------------------------------------------
# update denominator for 2018-2019 data  
# contains all computed metrics: costs, admits per 1000, mortality rate

make_metrics_table = function(admit_cnt_df,pids){
  
  df <- admit_cnt_df %>% filter(savvy_pid %in% pids) %>% 
    dplyr::select(savvy_pid, admit_counts,snf_admits,
                  total_spend, hosp_spend, snf_spend, mm_2019, date_of_death) %>% 
    filter(mm_2019 > 0) %>% 
    mutate(death_flag = ifelse(is.na(date_of_death)| date_of_death > '2020-01-01', 0, 1)) 
  
  # compute metrics for the predicted members 
  df %>% 
    summarise(PMPM_total = sum(total_spend, na.rm = T)/(sum(mm_2019)),
              PMPM_hosp = sum(hosp_spend, na.rm = T)/(sum(mm_2019)),
              PMPM_snf = sum(snf_spend, na.rm = T)/(sum(mm_2019)),
              admits_1k = (sum(admit_counts,na.rm = T)/(n_distinct(savvy_pid)))*1000,
              snf_admits_1k = (sum(snf_admits,na.rm = T)/(n_distinct(savvy_pid)))*1000,
              mortality_rate = sum(death_flag)/(n_distinct(savvy_pid)))
  
}

# create metrics table ----------------------------------------------------

metrics_tab <- function(data, model, cutoff, target_perc, type = 'uniform'){
  
  if(type == 'uniform'){
    # use same percentage positives with different thresholds
    get_threshold(fitted_model = model, train_data = data, 
                  target_perc = target_perc, cutoff = cutoff)[[1]] %>% 
      get_metrics(model = model, newdata = data, orig_data = data, 
                  threshold = ., count = cutoff) %>% cbind(target_perc = target_perc)
    
  } else if(type == 'fixed'){
    # use population and threshold from >0
    get_threshold(fitted_model = model, train_data = data, 
                      target_perc = target_perc, cutoff = 0)[[1]] %>% 
      get_metrics(
        threshold = .,
        orig_data = data,
        newdata = data %>% filter(
          savvy_pid %in% (gen_cmatrix_prob(
            model,
            newdata = data, 
            threshold = .,
            count = 0
          )[[2]] %>% 
            filter(as.integer(predicted) == 1) %>% 
            pull(savvy_pid))
        ),
        model = model,
        count = cutoff
      ) %>% cbind(target_perc = target_perc)
      }
  }
  

# metrics ------------------------------------------------------
pct = c(0.05, 0.02)
ct = c(0,1,2)
mod = c(1:length(models))

registerDoParallel(cores = detectCores()-1)

## Metrics using different thresholds for each count cutoff, each % predicted desired
metrics_uniform_perc <- foreach(k = mod, .combine = rbind) %:% 
  foreach(i = pct, .combine = rbind) %do% {
    data.frame( sapply(ct, function(x) metrics_tab(data_clean, models[[k]],  
                                                   cutoff = x , target_perc = i,
                                                   type = 'uniform'))) %>% 
      do.call(rbind.data.frame, .) %>% 
      mutate_at(vars(-'cutoff'), ~round(.,3)) %>% 
      cbind(model = names(models[k]))
  }   

metrics_uniform_perc18 <- metrics_uniform_perc %>% 
  arrange(target_perc, model) %>% 
  select(model, target_perc, everything())

## Metrics using just a fixed threshold from cuttoff > 0 and corresponding population
metrics_fixed_0 <- foreach(k = mod, .combine = rbind) %:% 
  foreach(i = pct, .combine = rbind) %do% {
    data.frame( sapply(ct, function(x) metrics_tab(data_clean, models[[k]],  
                                                   cutoff = x , target_perc = i,
                                                   type = 'fixed'))) %>% 
      do.call(rbind.data.frame, .) %>% 
      mutate_at(vars(-'cutoff'), ~round(.,3)) %>% 
      cbind(model = names(models[k]))
  }  

metrics_fixed_0_18 <- metrics_fixed_0 %>% 
 arrange(target_perc, model) %>% 
  select(model, target_perc, everything())

save(metrics_uniform_perc18,
     metrics_fixed_0_18,
     file = '~/bkt_inv_post_acute/metrics_2018.Rdata')

save(metrics_lag_uniform_perc18,
     metrics_lag_fixed_0_18,
     file = '~/bkt_inv_post_acute/metrics_2018_lag.Rdata')