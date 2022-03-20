library(yardstick)
library(caret)


get_threshold <- function(fitted_model, train_data, target_perc, cutoff){
  
# get probabilities from train set
train_probs <- pnbinom(q = cutoff, 
                     mu = predict(fitted_model, train_data, type = 'response'), 
                     size = fitted_model$theta, 
                     lower.tail = FALSE)  

# get top n% of the population
top_pop <- data.frame(savvy_pid = train_data$savvy_pid,
                      truth = train_data$admit_counts,
                      predicted = train_probs)

top_pop <- top_pop %>% 
  arrange(desc(predicted)) %>% 
  top_n(n = length(train_data$admit_counts)*target_perc)

# get n% threshold
target_threshold <- tail(top_pop,1)$predicted

return(list(target_threshold, top_pop))
}


# Confusion Matrix (Probability) ------------------------------------------
gen_cmatrix_prob <- function(fitted_model, newdata, threshold = 0.5, count = 0){
  
  if('zeroinfl' %in% class(fitted_model)){
    
    #proability of having no admissions 
    zero_pred <- predict(fitted_model, newdata, type  ='zero')
    
    total_pr <- (1-zero_pred)*(pnbinom(q = count, 
                                       mu = predict(fitted_model, newdata, type = 'count'), 
                                       size = fitted_model$theta, 
                                       lower.tail = FALSE))
  } else {
    
    total_pr <- pnbinom(q = count, 
                        mu = predict(fitted_model, newdata, type = 'response'), 
                        size = fitted_model$theta, 
                        lower.tail = FALSE)  
  }
  
  
  # Create confusion matrix
  pred_labels <- factor(ifelse(total_pr >= threshold, 1, 0), levels = c(1,0))
  newdata$actual_labels <- factor(ifelse(newdata$admit_counts >  count , 1, 0), levels = c(1,0))
  
  ## table for prediction labels
  pred_table <- data.frame(savvy_pid = newdata$savvy_pid,
                          truth = newdata$actual_labels,
                          predicted = pred_labels)
  
  ## creating confusion matrix
  cm <- confusionMatrix(data = pred_table$predicted, 
                        reference = pred_table$truth, 
                        mode = 'everything', positive ='1') 
  
  ## values to return
  return(list(cm, pred_table))
}


# model rmse --------------------------------------------------------------
mod_rmse <- function(fitted_model, data){
  pred = predict(fitted_model, data, type = 'response')
  rmse = round(sqrt(mean((data$admit_counts - pred)^2)),3)#same as RMSE(pred, newdata$admit_counts)
}


# summary metric table -----------------------------------------------------------

metric_smry <- function(nb_mods, zinb_mods, count, formulas, threshold = 0.5){
  
  nb_metrics <- sapply(nb_mods, function(x) gen_cmatrix_prob(x, test_df, threshold = threshold, count = count))
  zinb_metrics <- sapply(zinb_mods, function(x) gen_cmatrix_prob(x, test_df, threshold = threshold, count = count))
  
  data_frame(models = c("gender, age, ip days",
                        "gender, age, ip days,\n chronic conditions",
                        "gender, age, ip days,\n chronic conditions, frailty categories",
                        "gender, age, ip days,\n chronic conditions, frailty categories by quarter"),
             `feature count` = sapply(formulas, function(x) length(all.vars(x))-1),
             `RMSE (NB)` = sapply(nb_mods, function(x) mod_rmse(x, test_df)),
             `RMSE (ZINB)` = sapply(zinb_mods, function(x) mod_rmse(x, test_df)),
             `PPV (NB)` = sapply(1:4, function(x) round(nb_metrics[1,][[x]]$byClass['Pos Pred Value'][[1]],3)),
             `PPV (ZINB)` = sapply(1:4, function(x) round(zinb_metrics[1,][[x]]$byClass['Pos Pred Value'][[1]],3)),
             `Recall (NB)` = sapply(1:4, function(x) round(nb_metrics[1,][[x]]$byClass['Recall'][[1]],3)),
             `Recall (ZINB)` = sapply(1:4, function(x) round(zinb_metrics[1,][[x]]$byClass['Recall'][[1]],3)),
             `ACC (NB)` = sapply(1:4, function(x) round(nb_metrics[1,][[x]]$overall['Accuracy'][[1]],3)),
             `ACC (ZINB)` = sapply(1:4, function(x) round(zinb_metrics[1,][[x]]$overall['Accuracy'][[1]],3)),
  ) %>%
    kable(format = 'html', escape = FALSE, table.attr = "style='width:70%'") %>%
    kable_styling() %>%
    column_spec(1,width = '20em')
}



# Positive Prediction Summary ---------------------------------------------
pospred_smry <- function(nb_mods, zinb_mods, count, threshold = 0.5){

  nb_metrics <- sapply(nb_mods, function(x) gen_cmatrix_prob(x, test_df, threshold = threshold, count = count))
  zinb_metrics <- sapply(zinb_mods, function(x) gen_cmatrix_prob(x, test_df, threshold = threshold, count = count))
  
  data_frame(models = c("gender, age, ip days",
                        "gender, age, ip days,\n chronic conditions",
                        "gender, age, ip days,\n chronic conditions, frailty categories",
                        "gender, age, ip days,\n chronic conditions, frailty categories by quarter"),
             `NB Models` = sapply(1:4, function(x) 
               round(sum(nb_metrics[1,][[x]]$table[1,])/sum(nb_metrics[1,][[x]]$table)*100, 3)),
             `ZINB Models` = sapply(1:4, function(x) 
               round(sum(zinb_metrics[1,][[x]]$table[1,])/sum(zinb_metrics[1,][[x]]$table)*100, 3))
  )%>%
    kable(format = 'html', escape = FALSE, table.attr = "style='width:100%'") %>%
    kable_styling() %>%
    column_spec(1,width = '20em')
  
}




# make metrics table ------------------------------------------------------
# contains all computed metrics: costs, admits per 1000, mortality rate

make_metrics_table = function(admit_cnt_df,pids){
  
  df <- admit_cnt_df %>% filter(savvy_pid %in% pids) %>% 
    dplyr::select(savvy_pid, admit_counts,snf_admits,
                  total_spend, hosp_spend, snf_spend, mm_2018, date_of_death) %>% 
    filter(mm_2018 > 0) %>% 
    mutate(death_flag = ifelse(is.na(date_of_death)| date_of_death > '2019-01-01', 0, 1)) 
  
  # compute metrics for the predicted members 
  df %>% 
    summarise(PMPM_total = sum(total_spend, na.rm = T)/(sum(mm_2018)),
              PMPM_hosp = sum(hosp_spend, na.rm = T)/(sum(mm_2018)),
              PMPM_snf = sum(snf_spend, na.rm = T)/(sum(mm_2018)),
              admits_1k = (sum(admit_counts,na.rm = T)/(n_distinct(savvy_pid)))*1000,
              snf_admits_1k = (sum(snf_admits,na.rm = T)/(n_distinct(savvy_pid)))*1000,
              mortality_rate = sum(death_flag)/(n_distinct(savvy_pid)))

}




# overall metrics table ---------------------------------------------------

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
  pid_sub <- dplyr::filter(cm[[2]], as.character(predicted) == "1") %>% pull(savvy_pid)
  meas <- make_metrics_table(orig_data, pid_sub)
  
  
  dt <- as.data.table(lapply(c("Pos Pred Value", "Recall", "Detection Prevalence"), 
                             function(x) cm[[1]]$byClass[x]))
  colnames(dt) <- c("ppv", "Sensitivity", "pct_predicted")
  dt[,cutoff := paste(">" , count)]
  setcolorder(dt, c(4, 1:3))
  cbind(dt, meas)
  
  
  
}
