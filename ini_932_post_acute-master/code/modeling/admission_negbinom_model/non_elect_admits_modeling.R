
#**********************************************************************#
# AUTHOR: Alden Cabajar
# EDITORS:
# DESCRIPTION: 
# A workflow on the analysis of adission counts using poisson models
#**********************************************************************#

#=========== LOAD DEPENDENCIES, SETUP BEHAHVIOR ==============================
library(drake)
library(data.table)
library(bigrquery)
library(glue)
library(magrittr)
library(caret)
library(countreg)
library(here)
library(glue)
library(dplyr)

pid = "research-01-217611"
DB = "foc_interoperability.ini_1085"  
options(scipen = 20)

# source some functions
source(here("code/modeling/admission_negbinom_model/", "mod_metrics.R"))


options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
devtools::load_all(here("savvyR"))
cache <- new_cache(path = "/storage_pool/cache/")
con <-
  DBI::dbConnect(bigrquery::bigquery(),
                 dataset = "foc_interoperability",
                 project = "research-01-217611")


get_tbl_by_batch = function(tbl, dataset, batch = 3, filters = NULL, con) {
  cols <- DBI::dbSendQuery(
    con,
    glue::glue( 
      "
    SELECT column_name
    FROM research-01-217611.{dataset}.INFORMATION_SCHEMA.COLUMNS
    WHERE table_name = '{tbl}'
   " 
    )
  ) %>% 
    dbFetch()
  setDT(cols)
  cols[,ind:= 1:nrow(cols)]
  cols[,bin := as.integer(cut(ind, batch))]
  
  
  df_list <- vector(mode = "list", length = batch)
  
  for (i in 1:batch) {
    
    col_  <-  glue::glue_collapse(cols[bin == i][['column_name']], sep = ", ")  
    qr <- if (is.null(filters))  {
      glue::glue("SELECT {col_} FROM {dataset}.{tbl}")
    } else {
      glue::glue("SELECT {col_} FROM {dataset}.{tbl} {filters}")
    }
    
    df_list[[i]] <- DBI::dbSendQuery(con, qr) %>% DBI::dbFetch()
    
  }
  tbl_df <- do.call(cbind, df_list)
  
  return(tbl_df)
}



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
                         "date_of_death", "mm_2018", "gender", "hcc_count")
  df <- df %>% 
    mutate_if(!(names(.) %in% vars_not_included), ~ifelse(is.na(.), 0, .))
  
  df$gender <- factor(df$gender)
  df$age_cat <- cut(
    df$age_2017,
    breaks = c(65, 70, 75, 80, max(df$age_2017) +
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
  df <- df %>% 
    filter(hosp_days <= 200 & snf_days <= 200)
  return(df)
  
  
}


cols <- DBI::dbSendQuery(
  con,
  glue::glue( 
    "
    SELECT table_name, column_name
    FROM research-01-217611.foc_interoperability.INFORMATION_SCHEMA.COLUMNS
    WHERE table_name = 'ini_1085_admission_counts_features_non_elect_2017_2018_v2' or
    table_name = 'ini_1085_frail_feats_by_quarter' or 
    table_name = 'ini_1085_serious_conditions_2017'
   " 
  )
) %>% 
  dbFetch()
cc_cols <-  unique(grep("^cc_", cols$column_name,value = TRUE))
frail_cols <- cols %>% 
  filter(table_name == 'ini_1085_admission_counts_features_non_elect_2017_2018_v2' &
           grepl("frail_", column_name)) %>% 
  pull(column_name)
other_vars <- cols %>% 
  filter(!(column_name %in% c(cc_cols, frail_cols)) & 
           table_name =='ini_1085_admission_counts_features_non_elect_2017_2018_v2') %>%
  pull(column_name)
serious_cnd_vars <- cols %>% 
  filter(grepl("serious", table_name) & grepl("serious", column_name)) %>% 
  pull(column_name)

cc_cols_qr = glue_collapse(cc_cols, sep = ", ")
other_vars_qr = glue_collapse(other_vars, sep= ", ")



#========== PULLING THE DATA AND PREPROCESSING =================================================


query_v2 <-  glue("SELECT *
              FROM 
              (
                SELECT *
                FROM
                  `{DB}_admission_counts_features_non_elect_2017_2018_v2`
                WHERE
                  age_2017 >= 65
              ) AS a
              LEFT JOIN (
                SELECT *
                FROM
                  `{DB}_frail_feats_by_quarter`) AS b
              USING
                (savvy_pid)
              LEFT JOIN (
                SELECT * EXCEPT (total_raf)
                FROM
                  `{DB}_serious_conditions_2017`) as c
              USING (savvy_pid)")
 
                        
data_pull_and_process <- drake_plan(
  cc_df = load_bq(
    db = pid,
    query = glue(
     "SELECT savvy_pid, {cc_cols_qr}
      FROM {DB}_admission_counts_features_non_elect_2017_2018_v2" 
    )
  ), 
  
  admit_and_other_vars = load_bq(
    db = pid,
    query = glue(
      "SELECT {other_vars_qr}
      FROM {DB}_admission_counts_features_non_elect_2017_2018_v2
      WHERE age_2017 >= 65"
      
    )
  ),
  frail_feats_by_qt = load_bq(db = pid, 
                              query = glue("SELECT * FROM {DB}_frail_feats_by_quarter")),
  serious_cnd = load_bq(db = pid,
                        query = glue("SELECT * EXCEPT(total_raf) FROM {DB}_serious_conditions_2017")),
  
  ckd_stages = load_bq(db = pid, 
                       query = glue("SELECT savvy_pid
                       ,CASE WHEN SUM(CKD_Other) > 0 THEN 1 ELSE 0 END AS CKD_Other
                       ,CASE WHEN SUM(CKD_stage_1_4) > 0 THEN 1 ELSE 0 END AS CKD_stage_1_4
                       ,CASE WHEN SUM(CKD_stage_5_ESRD) > 0 THEN 1 ELSE 0 END AS CKD_stage_5_ESRD 
                       FROM {DB}_admission_counts_features_non_elect_2017_2018_v2
                       GROUP BY savvy_pid")),
        
  
  admit_cnt_df_v2 = Reduce(function(x, y) left_join(x, y, by = "savvy_pid"),
                 list(admit_and_other_vars, cc_df, frail_feats_by_qt, serious_cnd)),
  df_preproc_v2 = preprocess_data(admit_cnt_df_v2),
  
  # Create train and test set =======================
  trainIndex_v2 = createDataPartition(1:nrow(df_preproc_v2), p=0.8, list = FALSE),
  
  preProc = preProcess(df_preproc_v2[trainIndex_v2,], 
                       method = list(center = c("snf_days", "hosp_days"))),
  train_df_v2 = predict(preProc, df_preproc_v2[trainIndex_v2,]),
  test_df_v2 = predict(preProc, df_preproc_v2[-trainIndex_v2,]),
  
  # include CKD stages ==================
  train_df_w_CKD = left_join(train_df_v2, ckd_stages, by = "savvy_pid") %>% 
    mutate_at(vars(starts_with("CKD")), ~if_else(is.na(.), as.integer(0), .)),
  
  test_df_w_CKD = left_join(test_df_v2, ckd_stages, by = "savvy_pid") %>% 
    mutate_at(vars(starts_with("CKD")), ~if_else(is.na(.), as.integer(0), .))
)

#make(data_pull_and_process, cache = cache)
  

# ======== MODELING ======================================================================

  

# setup model formulas
feat_sets = list(
  demogs = c('gender', 'age_cat'),
  ip_days = c('hosp_cat', 'snf_cat'),
  cc = cc_cols,
  frailty_qtr = unique(grep("_Q", cols$column_name, value = T)),
  hcc = c('hcc_over5_flag')
)
orig_frml <- reformulate(termlabels = unlist(feat_sets[-which(names(feat_sets) == 'hcc')]),
                                response = 'admit_counts')
frml_w_hcc <- reformulate(unlist(feat_sets), response = 'admit_counts')
frml_w_intrct <- update(frml_w_hcc, . ~ . - age_cat + age_cat.75.79 + age_cat.70.74 + age_cat.80. + 
                          age_cat.80. * cc_Hip_Pelvic_Fracture)
frml_w_admits <- update(
  frml_w_intrct,
  . ~ . + prior_admit_2plus + prior_admit_1plus * age_cat.80.
  )

frml_w_serious_cnd <- update(
  frml_w_admits,
  . ~ . - cc_Cancer_CRC - cc_Cancer_Breast - cc_Cancer_Lung - cc_Cancer_Endo - cc_Cancer_Prostate -
    cc_Diabetes - cc_CKD - cc_A_Fib - cc_Depression - cc_Peripheral_Vascular_Disease  -
    cc_Heart_Failure - cc_Liver_Disease - cc_COPD
) %>% 
  update(
    as.formula( paste(".~ . +",glue_collapse(serious_cnd_vars, sep = "+")))
  )

frml_w_ckd_stages <- update(frml_w_serious_cnd, . ~ . + CKD_Other + CKD_stage_1_4 + CKD_stage_5_ESRD)

model_list <- tibble(
  model_name = c("orig_model", "w_hcc5_plus", "w_intrct", "w_admits", "w_serious_cnd", "w_ckd_stages"),
  frml = c(orig_frml, frml_w_hcc, frml_w_intrct, frml_w_admits, frml_w_serious_cnd, frml_w_ckd_stages)
)

get_metrics = function(model, newdata, orig_data, threshold, count)
{
  ## Arguments:
  # model - model that was used for prediction
  # newdata - subset of the data to get metrics
  # orig_data - the original admission counts df, used for getting other metrics
  # threshold - probability threshold
  # count - count threshold
  
  
  # get confusion matrix
  cm <- gen_cmatrix_prob(model, newdata, threshold = threshold, count = count)
  # filter to only those who were positively predicted
  pid_sub <- filter(cm[[2]], as.character(predicted) == "1") %>% pull(savvy_pid)
  meas <- make_metrics_table(orig_data, pid_sub)
  
  
  dt <- as.data.table(lapply(c("Pos Pred Value", "Recall", "Detection Prevalence"), 
         function(x) cm[[1]]$byClass[x]))
  colnames(dt) <- c("ppv", "Sensitivity", "pct_predicted")
  dt[,cutoff := paste(">" , count)]
  setcolorder(dt, c(4, 1:3))
  cbind(dt, meas)
  
  
  
}


model_plan <- drake_plan(
  trace = TRUE,
  models = target(
    glm.nb(frml, data = data),
    transform= map(
      frml = !!model_list$frml,
      data = c(train_df_v2, 
               train_df_v2, 
               train_df_v2, 
               train_df_v2, 
               train_df_v2, 
               train_df_w_CKD), 
      .data = !!model_list,
      .id = model_name
    )
  ),
  
  ## Metrics using thresholds for each count cutoff, each % predicted desired
  metrics_by_pct = target(
    get_threshold(models, train_df_w_CKD, target_perc = pct, cutoff = ct)[[1]] %>% 
      get_metrics(models, test_df_w_CKD, admit_cnt_df_v2, threshold = ., count = ct),
  transform = cross(
    pct = c(0.05, 0.02),
    ct = c(0, 1, 2),
    models
    
  )),
  
  # Metrics using just a fixed threshold from cuttoff > 0 and corresponding population
  metrics_fixed_th_0 = target(
    get_threshold(models, train_df_w_CKD, pct, cutoff = 0)[[1]] %>% 
    list(
      threshold = .,
      newdata = test_df_w_CKD %>% 
        filter(
          savvy_pid %in% (gen_cmatrix_prob(
            models,
            test_df_w_CKD, 
            threshold = .,
            count = 0
          )[[2]] %>% 
            filter(as.integer(predicted) == 1) %>% 
            pull(savvy_pid))
        ),
      model = models,
      orig_data = admit_cnt_df_v2,
      count = ct
      ) %>% 
      do.call(get_metrics, .),
  transform = cross(
    pct = c(0.05, 0.02),
    ct = c(0, 1, 2),
    models
  )),
  cmb_metrics_by_pct = target(
    bind_rows(metrics_by_pct),
    transform = combine(metrics_by_pct, .by = c(models, pct))
  ),
  
  cmb_metrics_th0_by_pct = target(
    bind_rows(metrics_fixed_th_0),
    transform = combine(metrics_fixed_th_0, .by = c(models, pct))
  )
  
)

# ======== INVESTIGATING FALSE POSITIVES, FALSE NEGATIVES =======================================

# from the best model, get false positives, false negatives
# current best model is `models_w_serious_cnd`
investigate_plan <- drake_plan(
 best_model_predict = target(
   get_threshold(models_w_serious_cnd, train_df_w_CKD, pct, cutoff = 0)[[1]] %>% 
   gen_cmatrix_prob(models_w_serious_cnd, test_df_w_CKD, threshold = ., count = ct) %>% 
   .[[2]],
   
   transform = cross(
     pct = c(0.05, 0.02),
     ct = c(0, 1, 2)
   )
  )
  
)


# ===========================REPORT GENERATION ====================================================

### WARNING: paths specified are not absolute, please change depending on the situation
create_report <- drake_plan(
  report = rmarkdown::render(
    knitr_in(
      "~/ini_932_post_acute/code/modeling/admission_negbinom_model/non_elect_admits_report_model.Rmd"
    ),
    output_dir = "~/ini_932_post_acute/code/modeling/admission_negbinom_model/",
    output_file = file_out(
      "~/ini_932_post_acute/code/modeling/admission_negbinom_model/non_elect_admits_report_model.html"
    )
  )
)
make(
  bind_plans(data_pull_and_process, model_plan, investigate_plan),
  cache = cache,
  seed = 300
)




