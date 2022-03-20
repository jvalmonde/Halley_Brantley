################################################################################
# QSEP combine yearly data
# Halley Brantley
################################################################################
library(savvy)
library(rlang)
library(tidyverse)
rm(list=ls())
################################################################################

# Import and combine member information

members_2014 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_2014_v2",
                          as.is = TRUE)
members_2015 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_2015_v2",
                     as.is = TRUE)
members_2016 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_2016_v2",
                          as.is = TRUE)

members_2014 <- 
  members_2014 %>%
  mutate(Year = 2014) %>%
  rename(MM = MM_2014) 

members_2015 <- 
  members_2015 %>%
  mutate(Year = 2015) %>%
  rename(MM = MM_2015)

members_2016 <- 
  members_2016 %>%
  mutate(Year = 2016) %>%
  rename(MM = MM_2016) 

members <- rbind(members_2014, members_2015, members_2016) %>% 
  mutate_at(vars(MM, Age, Total_Allow:RX_Allow, Same_HHwSC,
                 RAF_2014, MedianHouseholdIncome), funs(as.numeric)) %>%
  rename(MedianHH_raw = MedianHouseholdIncome) %>%
  mutate(MedianHouseholdIncome = MedianHH_raw / 1000)

# Identify years members weren't enrolled
non_enrolled <- members %>%
  dplyr::select(Year, Indv_Sys_Id, MM) %>%
  filter(MM <= 3) %>% 
  as_tibble()

# Only include members enrolled for more than 3 months
members <- members %>% filter(MM > 3)

save(members, file = "../data/members.Rdata")

################################################################################



# Remove members form spend if enrolled less than 3 months during the year
spend_ym <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_YearMo_v2",
                      as.is = TRUE)
spend_ym$Year <- as.numeric(str_sub(spend_ym$Year_Mo, 1, 4))
spend <- anti_join(spend_ym, non_enrolled)

# Convert columns to numeric
spend <- 
  spend %>%
  mutate_at(
    vars(Month_IND_T1D:RX_Allow, Depression_Flag, MaintenanceDrug_Flag, 
         SickChild_Flag), 
    funs(as.numeric)
  )
  
remove_negatives <- function(x) ifelse(x < 0, 0, x)

spend <- spend %>%
  mutate_at(
    vars(Total_Allow:RX_Allow), 
    funs(remove_negatives)
  )

spend$Month <- as.numeric(str_sub(spend$Year_Mo, 5, -1))

save(spend, file = "../data/spend.Rdata")

################################################################################

family_2014 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.FamilySummary_2014_v2",
                         as.is = TRUE)
family_2014 <- 
  family_2014 %>% 
  mutate(Year = 2014)

family_2015 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.FamilySummary_2015_v2",
                         as.is = TRUE)
family_2015 <- 
  family_2015 %>%
  mutate(Year = 2015)

family_2016 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.FamilySummary_2016_v2",
                         as.is = TRUE)

family_2016 <- 
  family_2016 %>%
  mutate(Year = 2016)

family <- 
  bind_rows(family_2014, family_2015, family_2016) %>% 
  mutate_at(vars(FamilySize:IP_Ratio), funs(as.numeric)) %>%
  mutate_at(vars(Product_Desc:FinancialMarket_Nm), funs(as.factor)) 

family <- 
  family %>%
  mutate(ProductGroup = factor(ProductGroup, 
                               levels = unique(family$ProductGroup), 
                               labels = trimws(unique(family$ProductGroup))),
    plan_type = fct_collapse(ProductGroup, OTHER = c("OTHER", "", 
                                                     "SELECT", "INDEMNITY",
                                                     "DIFFERENTIAL PPO",
                                                     "NON-DIFFERENTIAL PPO"))) 

save(family, file = "../data/family.Rdata")
################################################################################


