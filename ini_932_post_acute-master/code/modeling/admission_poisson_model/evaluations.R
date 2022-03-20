#**********************************************************************#
# AUTHOR: Alden Cabajar
# EDITORS:
# DESCRIPTION: 
# functions for evaluating models
#**********************************************************************#

library(data.table)
library(pscl)


predict_ = function(model_output, newdata, return_cmp = TRUE, response)
{
  # a modified predict function for light models
  
  frml_str <- paste0(deparse(model_output$frml), collapse = "")
  
  if ("zeroinfl" %in% model_output$class)
  {
    if (grepl("\\|", frml_str))
    {
      frml_count <- as.formula(gsub("(.*)\\|.*", "\\1", frml_str))
      frml_zero <- as.formula(paste(response, "~", gsub(".*\\|(.*)", "\\1", frml_str)))
      
    } else
    {
      frml_count <- as.formula(frml_str)
      frml_zero <- as.formula(frml_str)
    }
    model_mt_cnt <- model.matrix(frml_count, data = newdata)
    model_mt_zero <- model.matrix(frml_zero, data = newdata)
    
    count_coef_idx <- grep("count_", names(model_output$coefficients))
    zero_coef_idx <- grep("zero_", names(model_output$coefficients))
    model_coefs_cnt <- model_output$coefficients[count_coef_idx]
    model_coefs_zero <- model_output$coefficients[zero_coef_idx]
    
    lambda <- exp(model_mt_cnt %*% model_coefs_cnt)
    pr <- exp(model_mt_zero %*% model_coefs_zero)
    expected_value <- lambda * (1-pr)
    
    if (return_cmp)
    {
      return(list(
        lambda = lambda,
        pr = pr,
        expected_value = expected_value
      ))
    } else
    {
      return(expected_value)
    }
    
  } else if ("glm" %in% model_output$class)
  {
    model_mt_cnt <- model.matrix(as.formula(frml_str), data = newdata)
    lambda <- exp(model_mt_cnt %*% model_output$coefficients)
    
    return(lambda)
    
  }
    
}


rmse = function(model_output, data, response)
{
  actual <- data[[response]]
  predicted <- predict_(model_output, newdata = data, response = response, return_cmp = FALSE)
 return(sqrt(mean((actual - predicted)^2)))
}



conf_matrix_gen = function(model_output, data, count, cutoff, response) 
{
  # function generates a confusion matrix from probability predictions of model_output
  # data = data for prediction
  # count = cutoff count to get positive (> count) and negative classes ( <= count)
  # cutoff = cutoff probability to label it positive
  # response = name of response variable
  
  if ("glm" %in% model_output$class)
  { 
    if (model_output$family$family == 'poisson')
    {
      lambda <- predict_(model_output, newdata = data)
      total_pr <- ppois(q = count, lambda = lambda, lower.tail = FALSE) 
      
    } else if (model_output$family$family == 'quasipoisson')
    {
      lambda <- predict_(model_output, newdata = data)
      disp <- model_output$dispersion
      total_pr <- pnbinom(q = count, mu = lambda, size = lambda/(disp - 1), lower.tail =FALSE)
      
    }
  } else if ('zeroinfl' %in% model_output$class)
  {
    preds <- predict_(model_output, newdata = data, response = "admit_counts")
    pr <- preds$pr
    lambda <- preds$lambda
     ## Pr (y > n) where n != 0 
    total_pr <- (1- pr) * ppois(count, lambda, lower.tail = FALSE) 
    
  }
  
  # Create confusion matrix
  pred_labels <- factor(ifelse(total_pr >= cutoff, 1, 0), levels = c(1, 0))
  actual_labels <- factor(ifelse(data[[response]] >  count, 1, 0), levels = c(1,0))
  cf <- confusionMatrix(pred_labels, actual_labels, positive = "1")#$byClass['Pos Pred Value']
  
  
  return(cf)
  
}



plot_coefs = function(model_output, include_intercept = TRUE)
{
  
    
  out <- model_output[c("coefficients", "std_error")]
  model_df <- as.data.table(out)[,var_name := names(model_output$coefficients)][]
  if ("glm" %in% model_output$class)
  {
    model_coefs <- list(count = model_df)
    
  } else if ("zeroinfl" %in% model_output$class)
  {
    count_coefs <- model_df[grepl("count_" ,var_name)]
    zero_coefs <- model_df[grepl("zero_" ,var_name)]
    
    model_coefs <- list(count = count_coefs, zero = zero_coefs)
    
    
  }
  
  
  # remove intercept related coefficients
  if (!include_intercept)
  {
    model_coefs <-
      lapply(model_coefs, function(x)
        x[!grepl("Intercept", var_name), ])
    
  }
  
  # change to inverse link, derive CIs at inverse link
  model_coefs <- 
    lapply(
      model_coefs, 
      function(x) {
        x[,':='(
          inv_link = exp(coefficients),
          upr_ci = exp(coefficients + std_error * 1.96),
          lwr_ci = exp(coefficients - std_error * 1.96)
        )][(upr_ci - lwr_ci) <= 0.5]
        )]
  })
  
  # plotting the variables
  plt_list <- 
    lapply(
      model_coefs,
      function(x) {
          
        x %>% 
        ggplot(aes(x = reorder(var_name, inv_link), y = inv_link)) +
          geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), col = '#4285f4')+
          geom_point(col = 'grey35') +
          geom_hline(yintercept = 1, col = '#fcaf03') +
          coord_flip() + 
          labs(x = 'Model Coefficient', y = "expected counts ratio") + 
          theme_rnd(13)
      
      }
      
    )
  
  return(plt_list)
  
}

# removes unnecesary components from the model. Enough to do predictions.
lighten_model = function(model)
{
  coefs <- coef(model)
  se <- sqrt(diag(vcov(model)))
  disp <- summary(model)$dispersion
  
  return(list(
    coefficients = coefs,
    family = model$family,
    std_error = se,
    dispersion = disp,
    frml = model$formula,
    class = class(model)
  ))
  
}

