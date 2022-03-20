################################################################################
# QSEP Exploratory Analysis
# Halley Brantley
################################################################################
library(savvy)
library(rlang)
library(tidyverse)
rm(list=ls())
load("QSEP.RData")
################################################################################
set.seed(23456)
family_sample <- members %>%
  filter(SickChild_Flag == 1, Age < 16) %>%
  select(FamilyID) %>%
  unique() 
  
members <- members %>% filter(FamilyID %in% family_sample[,1])
spend <- spend %>% filter(FamilyID %in% family_sample[,1])

members %>% filter(SickChild_Flag == 1, Age < 16) %>%
  select(Indv_Sys_Id, ends_with("Flag")) %>%
  distinct() %>% 
  select(ends_with("Flag")) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate(Multi = ifelse(T1D_Flag+CA_Flag+ASD_Flag+Cerebral_Flag+Asthma_Flag+Trauma_Flag > 1, 1, 0)) %>%
  colSums()

spend <-
  members %>%
  select(Indv_Sys_Id, Year, MM, Same_HHwSC) %>%
  right_join(spend) %>%
  mutate_at(vars(Same_HHwSC), funs(as.numeric))

sick_child_spend <- 
  spend %>% 
  filter(SickChild_Flag == 1 & Age < 16)

adult_family_spend <- 
  spend %>% 
  filter(SickChild_Flag == 0, 
         Age > 30 & Age < 55, 
         Same_HHwSC == 1)

# Number of adults per fam
adult_family_spend %>%
  group_by(FamilyID) %>%
  summarise(
    num_ind = n_distinct(Indv_Sys_Id)
  ) %>%
  select(num_ind) %>%
  table()

before_trauma <- sick_child_spend %>% 
  filter(Trauma_Flag==1 & Month_IND_Trauma < 0)

before_trauma %>% 
  select(ends_with("Allow")) %>%
  colSums()

################################################################################

plot_spend <- function(sick_child_spend, flag, Month_Ind, max_y = 1500){
  Month_Ind <- enquo(Month_Ind)
  flag <- enquo(flag)
  
  child_condition <- 
    sick_child_spend %>%
    filter(!!flag == 1, !!Month_Ind > -7, !!Month_Ind < 7) %>%
    mutate(Month_diag = factor(!!Month_Ind))
  
  medianBefore <- child_condition %>%
    filter(!!Month_Ind < 0) %>%
    summarise(median(Total_Allow)) %>% as.numeric()
  q75Before <- child_condition %>%
    filter(!!Month_Ind < 0) %>%
    summarise(quantile(Total_Allow, .75)) %>% as.numeric()
  
  counts <-
    child_condition %>%
    group_by(Month_diag) %>%
    summarise( ct = n() )

  p1 <- child_condition %>%
    ggplot(aes(x = Month_diag, y = Total_Allow)) +
    geom_boxplot(outlier.color = "white") +
    geom_hline(yintercept = medianBefore, col="blue") +
    geom_hline(yintercept = q75Before, col="blue") +
    coord_cartesian(ylim = c(0,max_y)) +
    labs(y = "Total ($)",
         x = "Months Since Child's Diagnosis",
         fill = "After Diagnosis") +
    geom_label(data = counts, aes(y = max_y*.9, label = ct),
               position = "identity",
               hjust = "middle") +
    theme_savvy()
  
  return(p1)
}

#ASD
plot_spend(sick_child_spend, ASD_Flag, Month_IND_ASD) + 
  labs(title = "Sick Child Spending: ASD")
ggsave("../figures/sick_child_spend_ASD.png", width = 8, height = 4)

plot_spend(adult_family_spend, ASD_Flag, Month_IND_ASD, 500) + 
  labs(title = "Adult Family Member Spending: ASD") 
ggsave("../figures/adult_family_spend_ASD.png", width = 8, height = 4)

# T1D
plot_spend(sick_child_spend, T1D_Flag, Month_IND_T1D, 3000) + 
  labs(title = "Sick Child Spending: T1D")
ggsave("../figures/sick_child_spend_T1D.png", width = 8, height = 4)

plot_spend(adult_family_spend, T1D_Flag, Month_IND_T1D, 500) + 
  labs(title = "Adult Family Member Spending: T1D") 
ggsave("../figures/adult_family_spend_T1D.png", width = 8, height = 4)

# Cerebral
plot_spend(sick_child_spend, Cerebral_Flag, Month_IND_Cerebral, 3000) + 
  labs(title = "Sick Child Spending: Cerebral Palsy")
ggsave("../figures/sick_child_spend_Cerebral.png", width = 8, height = 4)

plot_spend(adult_family_spend, Cerebral_Flag, Month_IND_Cerebral, 500) + 
  labs(title = "Adult Family Member Spending: Cerebral Palsy") 
ggsave("../figures/adult_family_spend_Cerebral.png", width = 8, height = 4)

# Asthma
plot_spend(sick_child_spend, Asthma_Flag, Month_IND_Asthma, 1500) + 
  labs(title = "Sick Child Spending: Asthma")
ggsave("../figures/sick_child_spend_Asthma.png", width = 8, height = 4)

plot_spend(adult_family_spend, Asthma_Flag, Month_IND_Asthma, 500) + 
  labs(title = "Adult Family Member Spending: Asthma") 
ggsave("../figures/adult_family_spend_Asthma.png", width = 8, height = 4)

# Cancer
plot_spend(sick_child_spend, CA_Flag, Month_IND_CA, 1500) + 
  labs(title = "Sick Child Spending: CA")
ggsave("../figures/sick_child_spend_CA.png", width = 8, height = 4)

plot_spend(adult_family_spend, CA_Flag, Month_IND_CA, 500) + 
  labs(title = "Adult Family Member Spending: CA") 
ggsave("../figures/adult_family_spend_CA.png", width = 8, height = 4)

# Trauma
plot_spend(sick_child_spend, Trauma_Flag, Month_IND_Trauma, 3000) + 
  labs(title = "Sick Child Spending: Trauma")
ggsave("../figures/sick_child_spend_Trauma.png", width = 8, height = 4)

plot_spend(adult_family_spend, Trauma_Flag, Month_IND_Trauma, 500) + 
  labs(title = "Adult Family Member Spending: Trauma") 
ggsave("../figures/adult_family_spend_Trauma.png", width = 8, height = 4)
################################################################################  



################################################################################


condition_t_test <- function(Month_Ind){
  adult_condition <- 
    adult_family_spend %>%
    filter(!!Month_Ind > -13 & !!Month_Ind < 13) %>%
    mutate(After_Diagnosis = !!Month_Ind >= 0) %>%
    group_by(Indv_Sys_Id, After_Diagnosis) %>% 
    summarise(
      ct = n(),
      total = mean(Total_Allow), 
      rx = mean(RX_Allow), 
      dr = mean(DR_Allow)
    ) %>%
    arrange(After_Diagnosis, Indv_Sys_Id) 
  
  num_totals <- 
    adult_condition %>%
    group_by(Indv_Sys_Id) %>%
    summarise(ct = n()) %>% 
    filter(ct == 2)
  
  adult_condition %>%
    filter(Indv_Sys_Id %in% num_totals$Indv_Sys_Id) %>%
    group_by(After_Diagnosis) %>%
    summarise(
      mean_Tot = mean(total)
    ) %>%
    print()
  
  adult_condition %>% 
    filter(Indv_Sys_Id %in% num_totals$Indv_Sys_Id) %>%
    t.test(total ~ After_Diagnosis, data=.,  paired=TRUE)
}

# condition_t_test("Month_IND_ASD")
# condition_t_test("Month_IND_Asthma")
# condition_t_test("Month_IND_T1D")
# condition_t_test("Month_IND_Cerebral")
# condition_t_test("Month_IND_CA")
