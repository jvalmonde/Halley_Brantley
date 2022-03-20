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
setwd("~/ini_932_post_acute/")
source("code/modeling/admission_poisson_model/evaluations.R")
options(httr_oauth_cache = "~/.httr-oauth", httr_oob_default = TRUE)
devtools::load_all("savvyR")

pid = "research-01-217611"
db = "foc_interoperability.ini_1085"  
options(scipen = 20)
remake_all <-  FALSE #should we remake all targets?
sample_ratio <- 1

## Trigger remaking everything by deleting the very first target
if (remake_all)
{
  clean(raw_df)
}

## ==================== PREPROCESS AND LOAD DATA ===============================================
load_data <- drake_plan(
  raw_df = as.data.table(load_bq(
    pid,
    glue(
      "SELECT * FROM {db}_admission_counts_features_2017_2018 WHERE age_2017 >= 65"
    )
  )),
  
  # fraily features by quarter
  raw_ff_qt = as.data.table(load_bq(
    pid,
    glue("SELECT * FROM {db}_frail_feats_by_quarter")
  )), 
  
  # derive age bins
  df_pp = copy(raw_df
               )[, age_bin := cut(age_2017, seq(65, 110, by = 5), 
                                  include.lowest = TRUE)],
  ff_qt_cols = grep("frail_.*_Q\\d$" ,colnames(raw_ff_qt), value = TRUE),
  df_merged = merge(df_pp, raw_ff_qt, by = "savvy_pid", all.x = TRUE
                    )[,c(ff_qt_cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
                      .SDcols = ff_qt_cols][],
  
  
  # partition to test and train sets (80-20)
  # truncate the range of of hospital and snf days, sample from the population
  df_mdl = na.omit(df_merged[hosp_days <= 200 &
                               snf_days <= 200])
                                               
  train_idx = createDataPartition(df_mdl$admit_counts, p = 0.8, 
                                 list = FALSE, times = 1),
  train = df_mdl[train_idx, ],
  test = df_mdl[-train_idx, ],
  
  # Standardize hopsital and snf days
  preProc = preProcess(train, method = list(center = c("snf_days", "hosp_days"))),
  train_fnl = predict(preProc, train),
  test_fnl = predict(preProc, test)
)


## ========================== MODEL BUILDING =================================================
model_plan <- drake_plan(
  ########  DERIVING FORMULAS ###########
  # frailty features (all-year 2017)
  frail_cols = grep("frail_.*[[:alpha:]]$", colnames(train_fnl), value = TRUE),
  # frailty features (by quarter)
  frail_cols_by_qt = grep("frail_.*_Q\\d{1}", colnames(train_fnl), value = TRUE),
  # comorbidity features
  cc_cols = grep("cc_.*", colnames(train_fnl), value = TRUE),
  
  frml = paste("admit_counts ~ " , paste0(frail_cols, collapse = '+'),"+ gender + age_bin") %>% 
    as.formula(),
  frml_ff_qt = paste("admit_counts ~ " , paste0(frail_cols_by_qt, collapse = '+'),
                     "+ gender + age_bin") %>% 
    as.formula(),
  frml_ff_snf_hosp = paste("admit_counts ~ " , paste0(frail_cols_by_qt, collapse = '+'),
                           "+ gender + age_bin + hosp_days + snf_days") %>% 
    as.formula(),
  frml_w_cc = paste("admit_counts ~ ", paste0(frail_cols_by_qt, collapse = "+"), 
                    "+ gender + age_bin + hosp_days + snf_days + ", paste0(cc_cols, collapse ="+")) %>% 
  as.formula(),
  
  ######## VANILLA POISSON MODELS ############
  # baseline model 
  pois_model_base = glm(admit_counts ~ gender + age_bin, data = train_fnl, 
                         family = poisson(link = "log")) %>% lighten_model(),
  # frailty features (all 2017)
  pois_model_ff_all = glm(frml, family = poisson(link = "log"), data = train_fnl
                          ) %>% lighten_model(),
  # using frailty features at each quarter
  pois_model_ff_qt = glm(frml_ff_qt, family = poisson(link = "log"), data= train_fnl
                         ) %>%  lighten_model(),
  # with snf and hops days
  pois_model_ff_snf_hosp = glm(frml_ff_snf_hosp, family = poisson(link = "log"), data= train_fnl
                               ) %>% lighten_model(),

  # with cc
  pois_model_cc = glm(frml_w_cc, family = poisson(link = "log"), data= train_fnl
                               ) %>% lighten_model(),
r
  
  pois_model_list = list("poisson baseline"= pois_model_base, 
                         "poisson w/ frailty feats" = pois_model_ff_all, 
                         "poisson w/ ff by qt" = pois_model_ff_qt,
                         "poisson w/ snf and hosp days" = pois_model_ff_snf_hosp,
                         "poisson w/ comorbidity conds" = pois_model_cc),
  
  

 ######## POISSON MODEL WITH OVERDISPERSION #######

  ov_pois_model = glm(frml, data = train_fnl, family = quasipoisson()) %>%  lighten_model(),
  ov_pois_model_ff_qt = glm(frml_ff_qt, data = train_fnl, family = quasipoisson()) %>%  lighten_model(),
 
  ov_pois_model_list = list("pois w/ disp frailty feats" = ov_pois_model,
                            "pois w/ disp and frailty feats by qt" = ov_pois_model_ff_qt),
 
 ######## ZERO-INFLATED POISSON MODELS ###########
  orig_frml = Reduce(paste, deparse(frml)),
  orig_frml_qt = Reduce(paste, deparse(frml_ff_qt)),
  frml_zif = as.formula(paste(orig_frml, "|age_bin + gender")),
  frml_zif_qt= as.formula(paste(orig_frml_qt, "|age_bin + gender")),
 
 # determine initial coefs for zero count binomial model
 coef_mdl_ff_snf_hosp = coef(glm(update(frml_ff_snf_hosp, I(admit_counts == 0) ~ .), 
                                 data = train_fnl, model = FALSE, y = FALSE)),

 coef_mdl_cc = coef(glm(update(frml_w_cc, I(admit_counts == 0) ~ .), 
                                 data = train_fnl, model = FALSE, y = FALSE)),

 # baseline model
  zeroinf_model_bsl = countreg::zeroinfl(
      admit_counts ~ gender + age_bin | 1, 
      data = train_fnl, dist = "poisson",
      link = "logit" ) %>%  lighten_model(),

 # model with ff as main features, gender and age as zero-inf vars
 zeroinf_model_ff1 = countreg::zeroinfl(
   frml,
   data = train_fnl,
   dist = "poisson",
   link = "logit"
 ) %>%  lighten_model(), 

 zeroinf_model_ff2 = countreg::zeroinfl(
      frml_ff_qt,
      data = train_fnl,
      dist = "poisson",
      link = "logit"
      ) %>%  lighten_model(),


 zeroinf_model_ff3 = zeroinfl(
   frml_ff_snf_hosp,
   data = train_fnl,
   dist = "poisson",
   control = zeroinfl.control(start = list(
     count = coef(pois_model_ff_snf_hosp),
     zero = coef_mdl_ff_snf_hosp
   )),
   link = "logit"
 ) %>%  lighten_model(), 

 
 zeroinf_model_ff4 = zeroinfl(
   frml_w_cc,
   data = train_fnl,
   dist = "poisson",
   control = zeroinfl.control(start = list(
     count = coef(pois_model_cc),
     zero = coef_mdl_cc
   )),
   link = "logit"
 ) %>%  lighten_model(), 
 zeroinf_model_list = list("zero inf baseline" = zeroinf_model_bsl, 
                           "zero inf w/ frailty feats" = zeroinf_model_ff1,
                           "zif w/ ff by qt all" = zeroinf_model_ff2,
                            "zif w/ snf and hosp" = zeroinf_model_ff3,
                           "zif w/ cc" = zeroinf_model_ff4)
 
)


## ========================== MODEL EVALUATION =================================================

evaluation <- drake_plan(
  model_list_all = c(pois_model_list, ov_pois_model_list, zeroinf_model_list),
  
  ### DETERMINE RMSE ###
  rmse_by_mdl = lapply(model_list_all, FUN = rmse, data = test_fnl, response = "admit_counts"),
  rmse_df = melt(as.data.table(data.frame(rmse_by_mdl, check.names = FALSE)),
                 value.name = "RMSE",
                 variable.name = "model type"), 
  
  ### DETERMINE CONFUSION MATRIX FOR > N COUNTS ###
  conf_mt_cutoff0_test = lapply(
    model_list_all,
    FUN = conf_matrix_gen,
    data = test_fnl,
    count = 0,
    cutoff = 0.5,
    response = "admit_counts" 
  ),
  
  conf_mt_cutoff0_train = lapply(
    model_list_all,
    FUN = conf_matrix_gen,
    data = train_fnl,
    count = 0,
    cutoff = 0.5,
    response = "admit_counts" 
  ),
  
  
  conf_mt_cutoff1 = lapply(
    model_list_all,
    FUN = conf_matrix_gen,
    data = test_fnl,
    count = 1,
    cutoff = 0.5,
    response = "admit_counts"
  ),
  conf_mt_cutoff2 = lapply(
    model_list_all,
    FUN = conf_matrix_gen,
    data = test_fnl,
    count = 2,
    cutoff = 0.5,
    response = "admit_counts"
  )
)
  



## ========================== VISUALIZATIONS =================================================


viz <- drake_plan(
  plt_model_coefs = lapply(model_list_all, 
                           FUN = plot_coefs, include_intercept = FALSE)
)



## ========================== REPORT GENERATION =================================================

rmd <- drake_plan(
  report = rmarkdown::render(
    knitr_in(
      "code/modeling/admission_poisson_model/admission_poisson_model.Rmd"
    ),
    output_dir = "code/modeling/admission_poisson_model/",
    output_file = file_out(
      "code/modeling/admission_poisson_model/admission_poisson_model.html"
    )
  )
)


# CONCATENATE PLANS
full_plan <- bind_plans(load_data, model_plan, evaluation, viz, rmd)

# make workflow
make(full_plan)


