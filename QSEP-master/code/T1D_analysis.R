################################################################################
# QSEP Exploratory Analysis - T1D
# Halley Brantley
################################################################################
library(savvy)
library(rlang)
library(tidyverse)
library(quantreg)
library(modelr)
library(broom)
rm(list=ls())
load("QSEP.RData")
################################################################################
set.seed(23456)

family_sample <- members %>%
  filter(T1D_Flag == 1, Age < 16, Year == 2015, MM == 12) %>%
  select(FamilyID) %>%
  unique() 

members <-
  members %>% 
  filter(FamilyID %in% family_sample[,1]) %>%
  mutate_at(vars(Same_HHwSC, RAF_2014), funs(as.numeric))

spend <- spend %>% filter(FamilyID %in% family_sample[,1])
  

spend <-
  members %>%
  select(Indv_Sys_Id, Year, MM, Same_HHwSC, RAF_2014, Gdr_Cd) %>%
  right_join(spend) 

sick_child_spend <- 
  spend %>% 
  filter(SickChild_Flag == 1 & Age < 16)

adult_family_spend <- 
  spend %>% 
  filter(SickChild_Flag == 0, 
         Age > 30 & Age < 55, 
         Same_HHwSC == 1)

adult_family_spend$After_diag <- 
  ifelse(adult_family_spend$Month_IND_T1D > -1, 1, 0)


adult_family_spend$male <- ifelse(adult_family_spend$Gdr_Cd == "M", 1, 0)

numHouse <- members %>%
  filter(SickChild_Flag == 0, 
         Age > 30 & Age < 55, 
         Same_HHwSC == 1) %>%
  select(Indv_Sys_Id, FamilyID) %>%
  distinct() %>%
  group_by(FamilyID) %>%
  summarise(
    adult_ct = n()
  )

adult_mean <- 
  adult_family_spend %>%
  filter(Month_IND_T1D > -25 & Month_IND_T1D < 25) %>%
  left_join(numHouse) %>%
  group_by(Indv_Sys_Id, After_diag) %>%
  summarise(
    Total_Allow = mean(Total_Allow), 
    RAF = mean(RAF_2014),
    MM = mean(MM),
    ct = n(), 
    Gdr_Cd = first(Gdr_Cd), 
    Age = mean(Age), 
    single_adult = mean(ifelse(adult_ct == 1, 1, 0))
  ) %>% 
  filter(ct > 3) 

unique(adult_mean$single_adult)

adult_mean$male <- ifelse(adult_mean$Gdr_Cd == "M", 1, 0)

ggplot(adult_mean, aes(x=Age, y=sqrt(Total_Allow), group = After_diag, col=factor(After_diag))) +
  geom_point(alpha=.3)+
  geom_smooth() + 
  facet_grid(.~male) 

add_noise <- function(x) {
  x$data$Total_Allow <-  x$data$Total_Allow + rnorm(nrow(x$data), mean = 20, sd = 2)
  #x$data$Age <-  x$data$Total_Allow + rnorm(nrow(x$data), mean = 0, sd = .3)
  return(x)
}

adult_mean[, c("RAF", "Age")] <- scale(adult_mean[, c("RAF", "Age")])
adult_mean <- as.data.frame(adult_mean)
model_form <- formula(sqrt(Total_Allow) ~ After_diag + male + Age + RAF + single_adult)

model_lm <- 
  adult_mean %>% 
  modelr::bootstrap(1000) %>%
  mutate(strap2 = map(strap, add_noise)) %>%
  mutate(model = map(strap2, ~lm(model_form, data=.)))

model_q50 <- 
  adult_mean %>% 
  modelr::bootstrap(1000) %>%
  mutate(strap2 = map(strap, add_noise)) %>%
  mutate(model = map(strap2, ~rq(model_form, data=., 
                                 tau = .5)))

model_q75 <- 
  adult_mean %>% 
  modelr::bootstrap(1000) %>%
  mutate(strap2 = map(strap, add_noise)) %>%
  mutate(model = map(strap2, ~rq(model_form, data=., tau = .75)))

rq_tidy <- function(fit) {
  df <- data.frame(term = names(coef(fit)), estimate = coef(fit))
  row.names(df) <- NULL
  return(df)
}


param_lm <- 
  model_lm %>%
  mutate(param = map(model, rq_tidy)) %>%
  select(.id, param) %>% 
  unnest() %>%
  mutate(model = "Squared Error")

param_q50 <- 
  model_q50 %>%
  mutate(param = map(model, rq_tidy)) %>%
  select(.id, param) %>% 
  unnest() %>%
  mutate(model = "Median")

param_q75 <- 
  model_q75 %>%
  mutate(param = map(model, rq_tidy)) %>%
  select(.id, param) %>% 
  unnest() %>%
  mutate(model = "75th quantile")


plot.df <- bind_rows(param_lm, param_q50, param_q75) %>%   
  filter(term != "(Intercept)") %>%
  group_by(term, model) %>%
  summarise(
    mean = mean(estimate), 
    q025 = quantile(estimate, .025), 
    q975 = quantile(estimate, .975)
  ) %>%
  mutate(term2 = factor(as.character(term), 
                        levels = c("RAF", "After_diag", "Age", "single_adult", "male"), 
                        labels = c("RAF", "After child's diagnosis", "Age", 
                                   "No other 'parents' on plan", "Male"))) 


ggplot(plot.df, aes(x = term2, col = model))+
  geom_point(aes(y = mean), position = position_dodge(width = .5)) +
  geom_hline(aes(yintercept = 0), col="red") +
  geom_errorbar(aes(ymin = q025, ymax = q975), position = position_dodge(width = .5)) +
  geom_label(data = subset(plot.df, term == "After_diag"), 
             aes(y=mean, label=round(mean, 2)), 
             vjust = "center", hjust = -.5,  position = position_dodge(width = .8)) +
  coord_flip() +
  labs(x = "Coefficient Estimate", 
       y = NULL, 
       col = "Model", 
       title = "Linear effect on the square root of total spend of adults age 30-55 
       in the same household as a sick child.") + 
  theme_savvy()
ggsave("../figures/parameter_est_T1D.png", width = 8, height = 5)


