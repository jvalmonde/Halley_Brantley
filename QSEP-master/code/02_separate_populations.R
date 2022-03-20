################################################################################
# QSEP separate populations
# Halley Brantley
################################################################################
library(tidyverse)
rm(list=ls())
load("../data/members.Rdata")
load("../data/spend.Rdata")
load("../data/family.Rdata")
################################################################################

# Identify sick children (under 16)
sick_children <- members %>%
  filter(SickChild_Flag == 1, Age < 16) %>%
  select(Indv_Sys_Id, FamilyID, Zip, Age, ends_with("Flag")) %>%
  select(-Depression_Flag, -PCP_Flag, -SickChild_Flag) %>%
  distinct() %>%
  as_tibble()

sick_children$FlagSum <- 
  sick_children %>% 
  select(ends_with("Flag")) %>%
  rowSums()

sick_children$Multiple_Flag <- as.numeric(sick_children$FlagSum > 1)

save(sick_children, file = "../data/sick_children.Rdata")

# Determine the age of the youngest sick child per family (all conditions), and
# the number od sick children per family
sick_child_age <- 
  sick_children %>%
  group_by(FamilyID, Zip) %>%
  summarise(
    numSick = n(),
    sick_child_age = min(Age)
  ) %>% 
  ungroup()
################################################################################

# Determine age of oldest person in the family
adult_family_age <- 
  members %>%
  group_by(FamilyID, Zip, Year) %>%
  summarise(
    max_adult_age = max(Age)
  )

# Select adult family members 
adult_family <- 
  members %>%
  inner_join(sick_child_age) %>%
  filter(SickChild_Flag == 0, 
         Age >= 16) %>%
  dplyr::select(Indv_Sys_Id, FamilyID, Year, MM, Same_HHwSC, RAF_2014, Gdr_Cd, 
                MedianHouseholdIncome,  Zip, Age, sick_child_age, numSick,
                ends_with("Flag")) %>%
  left_join(adult_family_age) %>%
  mutate(Age_diff_adult = abs(Age - max_adult_age),
         Age_diff_child = Age - sick_child_age) %>%
  filter(Age_diff_adult < 15 & Age_diff_child > 14 & Age_diff_child < 60)

save(adult_family, file = "../data/adult_family.Rdata")

# Determine number of adults per family
numHouse <- adult_family %>%
  dplyr::select(Indv_Sys_Id, FamilyID, Zip) %>%
  distinct() %>%
  group_by(FamilyID) %>%
  summarise(
    adult_ct = n()
  )

table(numHouse$adult_ct)  

# Number of children per family and age of youngest child
numChild <- members %>%
  inner_join(sick_children[,c("FamilyID", "Zip")]) %>%
  filter(Age < 16) %>%
  dplyr::select(Indv_Sys_Id, FamilyID, Zip, Age, SickChild_Flag) %>%
  distinct() %>%
  group_by(FamilyID, Zip) %>%
  summarise(child_ct = n(), 
            min_child_age = min(Age, na.rm = TRUE))

table(numChild$child_ct)

incomeQ <- quantile(members$MedianHouseholdIncome, 
                    c(0, .25, .5, .75, 1))

adult_covar <- 
  adult_family %>%
  left_join(family[, c("FamilyID", "Year", "plan_type")]) %>%
  left_join(numHouse) %>%
  left_join(numChild) %>%
  select(-Depression_Flag, -SickChild_Flag)

spend <- spend %>% select(-Zip, -Age)

# Dataset to be used in models
adult_model_data <- 
  adult_covar %>%
  inner_join(spend) 

# Create variables for models
adult_model_data <- 
  adult_model_data %>%
  mutate(After_ASD = case_when(Month_IND_ASD > -1 ~ 1, 
                               TRUE ~ 0),
         After_Asthma = case_when(Month_IND_Asthma > -1 ~ 1, 
                                  TRUE ~ 0),
         After_CA = case_when(Month_IND_CA > -1 ~ 1, 
                              TRUE ~ 0),
         After_Cerebral = case_when(Month_IND_Cerebral > -1 ~ 1, 
                                    TRUE ~ 0),
         After_T1D = case_when(Month_IND_T1D > -1 ~ 1, 
                               TRUE ~ 0),
         After_Trauma = case_when(Month_IND_Trauma > -1 ~ 1, 
                                  TRUE ~ 0),
         male = case_when(Gdr_Cd == "M" ~ 1, 
                          Gdr_Cd == "F" ~ 0, 
                          TRUE ~ NA_real_),
         positive_spend = as.numeric(Total_Allow > 1), 
         single_adult = as.numeric(adult_ct == 1),
         num_children = case_when(
           child_ct > 3 ~ "4+", 
           child_ct > 1 ~ "2-3",
           child_ct == 1 ~ "1"
         ),
         multiple_sick = case_when(
           numSick == 1 ~ 0,
           numSick > 1 ~ 1
         ),
         youngest_child = 
           case_when(
             min_child_age < 3 ~ "Infant",
             min_child_age < 13 ~ "Child",
             TRUE ~ "Teen"),
         time = as.numeric(as.factor(Year_Mo)), 
         plan_sub = fct_relevel(plan_type, "OTHER"), 
         RAF_cat = cut(RAF_2014, breaks=c(0, 0.25, 1, 2, 5, 10, 200)), 
         Income_cat = cut(MedianHouseholdIncome, c(0, 50, 100, 150, 260), 
                         include.lowest = TRUE),
         logSpend = log(Total_Allow))

adult_model_data$MM <- factor(adult_model_data$MM, 
                              levels = c(12, 4, 5, 6, 7, 8, 9, 10, 11))
################################################################################
# Data sanity checks
adult_covar_ID <- unique(adult_covar$Indv_Sys_Id)
adult_mod_ID <- unique(adult_model_data$Indv_Sys_Id)

# Adult members before and after merge with spend should be the same
length(adult_covar_ID)
length(adult_mod_ID)

# Should not have any duplicate member-time combinations
which(duplicated(adult_model_data[,c("Indv_Sys_Id", "time")]))

# Should not have missing youngest child age
sum(is.na(adult_model_data$min_child_age))

# Check plan types
unique(adult_model_data$plan_sub)

# Check number of children
unique(adult_model_data$child_ct)
table(adult_model_data$num_children)
table(adult_model_data$multiple_sick)

save(adult_model_data, file = "../data/adult_model_data.Rdata")
