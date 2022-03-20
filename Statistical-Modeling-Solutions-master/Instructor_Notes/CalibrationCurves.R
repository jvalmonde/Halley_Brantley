library(tidyverse)
library(caret)


expit <- function(x){
    return(1/(1+exp(-x)))
}


expit <- function(x){
    return(1/(1+exp(-x)))
}

cal_plot <- function(dat, eqn, nbins, quant = TRUE){
    # Fit logistic regression models to each split
    fit <- glm(formula(eqn), data = dat, family = binomial)
    
    # Set break points 
    if (quant){
        breakpts <- c(0, quantile(expit(predict(fit)), 
                                  seq(0, 1, length.out = nbins)), 1)
    } else {
        breakpts <- seq(0, 1, length.out = nbins+1)
    }
    
    # Get response variable
    resp <- sym(str_extract(eqn, "[A-Za-z]+"))
    
    dat %>% 
        mutate(
            # add predictions
            pred = predict(fit), 
            # bin predictions into groups
            pred_group =  as.numeric(as.character(
                cut(expit(pred), breakpts, labels = breakpts[-1], include.lowest = FALSE)
            ))) %>%
        group_by(pred_group) %>%
        # summarize observed data in groups
        summarise(
            ct = n(),
            resp_prob = mean(!!resp)
        ) %>%
        # make plot
        ggplot(aes(x = pred_group, y=resp_prob)) +
        geom_point(
            aes(size = ct),
            alpha = 0.3) +
        geom_abline(slope = 1, intercept = 0) +
        labs(x = "Predicted Probablity",
             y = "Observed Frequency",
             col = NULL) +
        theme_bw()
}

cal_plot(dat = Donner, eqn = "survived ~ I(age^2) + age + sex", nbins = 10, quant = TRUE)
cal_plot(dat = Donner, eqn = "survived ~ age + sex", nbins = 10, quant = TRUE)


