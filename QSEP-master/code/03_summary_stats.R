################################################################################
# QSEP separate populations
# Halley Brantley
################################################################################
library(tidyverse)
rm(list=ls())
load("../data/members.Rdata")
load("../data/sick_children.Rdata")
load("../data/adult_family.Rdata")
load("../data/spend.Rdata")
load("../data/family.Rdata")
################################################################################

# Numbers of Sick Children

sick_child_ct <- sick_children %>% select(Indv_Sys_Id, ends_with("Flag")) %>%
  distinct() %>%
  select(ends_with("Flag")) %>%
  colSums()

adult_ct <- adult_family %>% select(Indv_Sys_Id, T1D_Flag:Trauma_Flag) %>%
  distinct() %>%
  select(ends_with("Flag")) %>%
  colSums()

total_ct <- tibble(Diagnosis = names(adult_ct),
                   Children = sick_child_ct[1:6], 
                   Adults = adult_ct) %>% 
  arrange(Children) 

latex(total_ct, 
      title = '', 
      rowname = NULL, 
      file = "../tables/data_counts.tex", 
      na.blank = FALSE)

# Numbers of adult family members

adult_long <- 
  adult_family %>% 
    select(Indv_Sys_Id, FamilyID, T1D_Flag:Trauma_Flag) %>%
    distinct() %>%
    gather(key = "diag", value = "flag", T1D_Flag:Trauma_Flag)

adult_ct <- 
  adult_long %>% 
  group_by(diag) %>%
  summarise(
    adult_ct = sum(flag)
  )

family_ct <- 
adult_long %>% 
  select(FamilyID, diag, flag) %>%
  distinct() %>%
  group_by(diag) %>%
  summarise(
    family_ct = sum(flag)
  )

bind_cols(adult_ct, family_ct[,2])

# Percent of members enrolled for the full year ~65-70%
members %>%
  group_by(Year) %>%
  summarise(
    perctFull = sum(MM ==12)/n()
  )

sick_children %>%
  summarise(
    Asthma = sum(Asthma_Flag), 
    ASD = sum(ASD_Flag), 
    Cancer = sum(CA_Flag), 
    Cerbral = sum(Cerebral_Flag),
    T1D = sum(T1D_Flag), 
    Trauma = sum(Trauma_Flag),
    Multiple = sum(FlagSum > 1)
  )

