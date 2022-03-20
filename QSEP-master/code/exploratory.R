################################################################################
# QSEP Exploratory Analysis
# Halley Brantley
################################################################################
library(tidyverse)
library(savvy)
library(rlang)
rm(list=ls())
################################################################################
set.seed(12345)

spend_ym <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_YearMo_v2",
                     as.is = TRUE)

members_2015 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_2015_v2",
                     as.is = TRUE)

members_2015 %>% filter(SickChild_Flag == 1) %>% summarise(max(Age))

family_sample <- 
  members_2015 %>% 
  filter(SickChild_Flag == 1 & Age < 16) %>%
  select(FamilyID) %>%
  unique()  %>%
  sample_n(size = 25000)

spend_ym <- spend_ym %>% 
  filter(FamilyID %in% family_sample)

members_2015 <- members_2015 %>% 
  filter(FamilyID %in% family_sample)

family_2015 <- family_2015 %>%
  filter(FamilyID %in% family_sample)

write.csv(spend_ym, file = "/work/spend.csv", row.names=FALSE)
################################################################################

spend_ym$Year <- as.numeric(str_sub(spend_ym$Year_Mo, 1, 4))
spend_ym$Month <- as.numeric(str_sub(spend_ym$Year_Mo, 5, -1))

spend_ym <- 
  spend_ym %>%
  mutate_at(
    vars(Month_IND_T1D:Age), 
    funs(as.numeric)
  )

remove_negatives <- function(x) ifelse(x < 0, 0, x)

spend_ym <- spend_ym %>%
  mutate_at(
    vars(IP_Allow:RX_Allow), 
    funs(remove_negatives)
  )

sick_child_spend <- 
  spend_ym %>% 
  filter(SickChild_Flag == 1 & Age < 16)

adult_family_spend <- 
  spend_ym %>% 
  filter(SickChild_Flag == 0, 
         Age > 35 & Age < 55,
         FamilyID %in% sick_child_spend$FamilyID)

adult_family_spend %>%
  group_by(FamilyID) %>%
  summarise(
    num_ind = n_distinct(Indv_Sys_Id)
  ) %>%
  select(num_ind) %>%
  table()

# Compare total spend to sum of spend categories
adult_family_spend %>%
  mutate(Total2 = rowSums(select(., IP_Allow:RX_Allow))) %>%
  ggplot(aes(Total_Allow, Total2)) + geom_point() + 
  geom_abline(slope=1, intercept = 0, col="red")

# Number of months enrolled, sick child
sick_child_spend %>%
  group_by(Indv_Sys_Id) %>%
  summarise(
    numMonths = n()
  ) %>%
  select(numMonths) %>%
  hist(binwidth = 1)


################################################################################

plot_child_spend <- function(sick_child_spend, flag, Month_Ind){
  Month_Ind <- enquo(Month_Ind)
  flag <- enquo(flag)
  
  child_condition <- 
    sick_child_spend %>%
    filter(!!flag == 1, !!Month_Ind > -6 & !!Month_Ind < 6) 
  
  medianBefore <- child_condition %>%
    filter(!!Month_Ind < 0) %>%
    summarise(median(Total_Allow)) %>%
    as.numeric()
  
  counts <-
    child_condition %>%
    group_by(!! Month_Ind) %>%
    summarise(
      ct = n()
    )
  
  # 
  p1 <- child_condition %>%
        ggplot(aes(x= quo_text(Month_Ind), y = Total_Allow)) +
           geom_boxplot(outlier.color = "white") +
          geom_hline(yintercept = medianBefore, col="blue") +
          coord_cartesian(ylim = c(0,1500)) +
          labs(y = "Total ($)",
               x = "Months Since Child's Diagnosis",
               fill = "After Diagnosis",
               title = paste0("Sick Child Spending - ", str_sub(flag, 1, -6))) +
          geom_label(data = counts, aes(y = 1400, label = ct),
                     position = "identity",
               hjust = "middle") +
          theme_savvy()

  return(p1)
}

plot_child_spend(sick_child_spend, ASD_Flag, Month_IND_ASD)

plot_child_spend("T1D_Flag", "Month_IND_T1D")
plot_child_spend("Cerebral_Flag", "Month_IND_Cerebral")
plot_child_spend("Asthma_Flag", "Month_IND_Asthma")
plot_child_spend("CA_Flag", "Month_IND_CA")
################################################################################  

plot_adult_spend <- function(flag, Month_Ind, adult_spend){
  
  adult_condition <- 
    adult_spend %>%
    filter(!!flag == 1, !!Month_Ind > -7 & !!Month_Ind < 12, 
           Total_Allow > 0) %>%
    mutate(Month_Cat = factor(ifelse(!!Month_Ind < 0, 
                              "-6 to -1", 
                              ifelse(!!Month_Ind < 6, 
                                     "0 to 5",
                                     "6 to 11")))) %>%
    group_by(Indv_Sys_Id, Month_Cat) %>%
    summarise(
      Total = mean(Total_Allow)
    )
  
  
  medianBefore <- adult_condition %>%
    filter(Month_Cat == "-6 to -1") %>%
    ungroup() %>%
    summarise(median(Total)) %>%
    as.numeric()
  
  counts <-
    adult_condition %>%
    group_by(Month_Cat) %>%
    summarise(
      ct = n()
    )
  

  p1 <- 
    adult_condition %>%
    ggplot(aes(x=Month_Cat, y = Total)) + 
    geom_boxplot() + 
    geom_hline(yintercept = medianBefore, col="blue") +
    coord_cartesian(ylim = c(0,2500)) +
    labs(y = "Total ($)", 
         x = "Months Since Child's Diagnosis", 
         title = paste0("Adult Family Member Spending: ", str_sub(flag, 1, -6))) +
    theme_savvy() +
    geom_label(data = counts, aes(y = 1400, label = ct), position = "identity",
               hjust = "middle")
    
  
  return(p1)
}


plot_adult_spend("ASD_Flag", "Month_IND_ASD")
plot_adult_spend("T1D_Flag", "Month_IND_T1D", adult_family_spend)
plot_adult_spend("Cerebral_Flag", "Month_IND_Cerebral", adult_family_spend)
plot_adult_spend("Asthma_Flag", "Month_IND_Asthma", adult_family_spend)
plot_adult_spend("CA_Flag", "Month_IND_CA", adult_family_spend)

ggplot(adult_family_spend) + 
  geom_histogram(aes(x=Total_Allow, fill="Total"), bins = 100) + 
  geom_histogram(aes(x=IP_Allow, fill="IP"), alpha = .5, bins = 100) +
  geom_histogram(aes(x=ER_Allow, fill="ER"), alpha = .5, bins = 100) +
  xlim(c(5000, 30000))




adult_family_spend %>%
  select(ends_with("Allow")) %>%
  summary()
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

condition_t_test("Month_IND_ASD")
condition_t_test("Month_IND_Asthma")
condition_t_test("Month_IND_T1D")
condition_t_test("Month_IND_Cerebral")
condition_t_test("Month_IND_CA")
