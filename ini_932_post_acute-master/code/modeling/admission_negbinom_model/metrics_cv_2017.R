library(bigrquery)
library(drake)
library(caret)
library(countreg)
library(here)
library(glue)
library(dplyr)
library(doParallel)
library(modelr)
library(purrr)
library(broom)
library(data.table)
library(tidyr)
devtools::load_all("savvyR")

#source some functions
source(here("code/modeling/admission_negbinom_model", "mod_metrics.R"))
cache <- new_cache(path = "/storage_pool/cache/")

# loading data ------------------------------------------------------------
# load preprocessed data

loadd(train_df_w_CKD, cache = cache)
loadd(test_df_w_CKD, cache = cache)
data <- rbind(train_df_w_CKD, test_df_w_CKD)

# load all fitted models
loadd(starts_with("models"), cache = cache)
models <- lapply(ls(pattern = "models_"), get)
model_names <- gsub('models_', '', ls(pattern = 'models_'))
names(models) <- model_names
# rearrange models according to complexity
models <- models[c('orig_model', 'w_hcc5_plus', 'w_intrct', 'w_admits', 'w_serious_cnd', 'w_ckd_stages')] 

model_list <- tibble(
  model_name = names(models),
  frml = c(
    models$orig_model$call[[2]] %>% as.formula(),
    models$w_hcc5_plus$call[[2]] %>% as.formula(),
    models$w_intrct$call[[2]] %>% as.formula(),
    models$w_admits$call[[2]] %>% as.formula(),
    models$w_serious_cnd$call[[2]]%>% as.formula(),
    models$w_ckd_stages$call[[2]] %>% as.formula()
  )
)

# remove to free up space
rm(list = ls(pattern = 'models_'))

# parallel CV -------------------------------------------------------------

# #check number of workers available
# getDoParWorkers()

registerDoParallel(cores = 8)

# create k-folds
# currently using 20-80 split
set.seed(1085)
folds <- crossv_mc(data, n= 10, test = 0.8)

# function to fit models simultaneously
mod_cv <- function(formula){
  
  foreach( i =  1:length(formula$model_name), .combine = rbind) %dopar% {
    
    folds %>% 
      dplyr::mutate(model = map(train, function(x) glm.nb(formula$frml[[i]], data = x)),
                    frml = formula$model_name[i])
  }
}


# fit models: 20-80 split per fold
## do cross validation per model
## executed in sections due to memory allocation errors

# orig model
start <- proc.time(); folds_mod_orig <- mod_cv(formula = model_list[1,]); proc.time() - start

# hcc5+ model
start <- proc.time(); folds_mod_hcc5 <- mod_cv(formula = model_list[2,]); proc.time() - start

# with interaction model
start <- proc.time(); folds_mod_intrct <- mod_cv(formula = model_list[3,]); proc.time() - start

# with admits model
start <- proc.time(); folds_mod_admits <- mod_cv(formula = model_list[4,]); proc.time() - start

# with serious model
start <- proc.time(); folds_mod_serious <- mod_cv(formula = model_list[5,]); proc.time() - start

# with ckd model
start <- proc.time(); folds_mod_ckd_stages <- mod_cv(formula = model_list[6,]); proc.time() - start

## join folds
folds_all <- list(folds_mod_orig, folds_mod_hcc5, folds_mod_intrct, 
                  folds_mod_admits, folds_mod_serious, folds_mod_ckd_stages)


# FUNCTION: get metrics ---------------------------------------------------

cv_metrics <- function(cv_data, perc, cutoff, type = c('uniform', 'fixed')){
  if(type == 'uniform'){
    # use same percentage positives with different thresholds
    folds2 <- cv_data %>% 
      mutate(threshold = map2(model, train, ~ get_threshold(fitted_model = .x,  
                                                            train_data =  .y$data[.y$idx,], 
                                                            target_perc = perc, 
                                                            cutoff = cutoff)[[1]][1]),
             metrics = map2(model, test, ~ get_metrics(model = .x,
                                                       newdata = .y$data[.y$idx,],
                                                       orig_data = .y$data[.y$idx,],
                                                       threshold = threshold,
                                                       count = cutoff)) 
      )
    
  } else if(type == 'fixed'){
    # use population and threshold from >0
    folds2 <- cv_data %>% 
      mutate(zero_threshold = map2(model, train, ~ get_threshold(fitted_model = .x,  
                                                                 train_data =  .y$data[.y$idx,], 
                                                                 target_perc = perc, 
                                                                 cutoff = 0)[[1]][1]), 
             zero_pop = map2(model, test , ~.y$data[.y$idx,] %>% 
                               filter(
                                 savvy_pid %in% (gen_cmatrix_prob(
                                   .x,
                                   .y$data[.y$idx,] , 
                                   threshold = zero_threshold,
                                   count = 0
                                 )[[2]] %>% 
                                   filter(as.integer(predicted) == 1) %>% 
                                   pull(savvy_pid)))),
             metrics = map2(model, zero_pop, ~ get_metrics(model = .x,
                                                           newdata = .y,
                                                           orig_data = .y,
                                                           threshold = zero_threshold,
                                                           count = cutoff)))
    
    
    
  }
  
  temp <- dplyr::select(folds2, frml, metrics) %>% 
    unnest(cols = c(metrics)) %>% cbind(target_perc = perc*100)
  MM <- aggregate(temp[-c(1:5,ncol(temp))], list(temp$frml), FUN = function(x) mean(x)) %>% mutate_at(vars(-'Group.1'), funs(round(.,2))) #sapply(temp[-1], FUN = mean)  %>% round(2) %>% as.data.frame() 
  colnames(MM)[-1] <- paste('MM', colnames(MM[-1]), sep = '_')
  SEM <- aggregate(temp[-c(1:5,ncol(temp))], list(temp$frml), FUN = function(x) sd(x)/sqrt(length(x))) %>% mutate_at(vars(-'Group.1'), funs(round(.,2)))
  colnames(SEM)[-1] <- paste('SEM', colnames(SEM[-1]), sep = '_')
  
  temp_data <- left_join(MM,SEM) %>% cbind(cutoff = paste('>',cutoff))
  
  labs <- colnames(temp[-c(1:5,ncol(temp))])
  
  metric_tab <- sapply(labs, FUN = function(x) {dplyr::select(temp_data, contains(x), cutoff, Group.1) %>% 
      unite(x, paste0('MM_', x), paste0('SEM_', x), remove = TRUE , sep = ' (') %>% 
      mutate(x = paste0(x,')')) %>% 
      dplyr::select(x)
  }) ; metric_tab <- as.data.frame(metric_tab)
  
  colnames(metric_tab) <- sub('.x', '', colnames(metric_tab))
  
  metric_tab <- cbind(formula = temp_data$Group.1, cutoff = paste('>',cutoff), metric_tab , target_perc = perc*100)
  
  return(list(metric_tab, temp))
  
}

# create metrics table ----------------------------------------------------
# %predicted
pct = c(0.05, 0.02)
# cutoffs
ct = c(0, 1, 2)

## get metrics by model only, due to memory restrictions
## current setup is for best performing model 

## Metrics using different thresholds for each count cutoff, each % predicted desired
metrics_uniform <- foreach(i = pct, .combine = cbind) %do% {
  sapply(ct, function(x) cv_metrics(folds_all[[5]], i, x, type = 'uniform'))
}


metrics_uniform_serious <- metrics_uniform[1,] %>% combine()



## Metrics using just a fixed threshold from cuttoff > 0 and corresponding population
metrics_fixed <- foreach(i = pct, .combine = cbind) %do% {
  sapply(ct, function(x) cv_metrics(folds_all[[5]], i, x, type = 'fixed'))
}

metrics_fixed_serious <- metrics_fixed[1,] %>% combine()

save(metrics_uniform_serious,
     metrics_fixed_serious,
     file = '~/bkt_inv_post_acute/cv_metrics_10fold_serious.Rdata')


# ploting ppv and sensitivity -----------------------------------------------------------------
data <- metrics_uniform %>% 
  filter(target_perc == 2) %>%  
  select(-target_perc)

means_ppv <- aggregate(ppv~frml, data, function(x) mean(x))
means_sen  <- aggregate(Sensitivity~frml, data, function(x) mean(x))


plot_data <- data %>% 
  gather(key ='metrics', value = value, -c(frml, cutoff)) 

ggplot() +
  geom_boxplot(data =  filter(plot_data, metrics == "ppv"), aes(x = frml, y = value, fill = frml) ) +
  geom_label(data  = means_ppv, aes(x =frml,  label =paste(round(ppv,4)*100, '%'), y =  ppv), vjust = 0.75) +
  scale_y_continuous(labels = scales::percent, limits = c(0.445, 0.465)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Comparison of Models Positive Predictive Values",
       subtitle = 'Predicting 2% of the population with at least one admission',
       y = "Positive Predictive Value",
       x = 'Models')


ggplot() +
  geom_boxplot(data =  filter(plot_data, metrics == "Sensitivity"), aes(x = frml, y = value, fill = frml) ) +
  geom_label(data  = means_sen, aes(x =frml,  label =paste(round(Sensitivity,4)*100, '%'), y =  Sensitivity), vjust = 0.75) +
  scale_y_continuous(labels = scales::percent, limits = c(.07,.09)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Comparison of Models Sensitivity Values",
       subtitle = 'Predicting 2% of the population with at least one admission',
       y = "Sensitivity",
       x = 'Models')
