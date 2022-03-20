################################################################################
# QSEP Exploratory Analysis
# Halley Brantley
################################################################################
library(tidyverse)
library(savvy)
rm(list=ls())


members_2015 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.MemberSummary_2015_v2",
                          as.is = TRUE)

family_2015 <- read.odbc("Devsql10", dbQuery = "SELECT *
                         FROM pdb_QSEP.dbo.FamilySummary_2015_v2",
                         as.is = TRUE)

members_2015 <- 
  members_2015 %>% 
  mutate_at(
    vars(MM_2015, Total_Allow:Trauma_Cnt),
    funs(as.numeric)
  )

family_2015 <- 
  family_2015 %>% 
  mutate_at(
    vars(FamilySize:IP_Ratio),
    funs(as.numeric)
  )

members_2015 %>%
  group_by(Age_Group) %>%
  summarise(
    numSickChild = sum(SickChild_Flag), 
    numNotSick = sum(SickChild_Flag == 0)
  )

names(family_2015)

sum(family_2015$SickChildren_Ratio == 0, na.rm=T)/
  sum(family_2015$ChildrenCnt > 0, na.rm=T)

family_2015 %>%
  filter(SickChildren_Ratio == 0) %>%
  select(ChildrenCnt) %>%
  hist()

# Show counts of families by number of adults and number of children
family_2015 %>%
  mutate(AdultCnt = FamilySize - ChildrenCnt) %>%
  ggplot() + 
  geom_bar(aes(x = AdultCnt, fill="Adults"), stat = "count") + 
  theme_savvy() +
  geom_bar(aes(x=ChildrenCnt, fill="Children"), stat="count", alpha=.5) +
  labs(y = "# of people per family",
       x = "Count",
       fill = NULL)

# Some families have no adults and other families have no children. 

members_2015 %>%
  group_by(Age_Group) %>%
  summarise(
    min_age = min(Age), 
    max_age = max(Age)
  )

# There are 4 age groups: Adult, Adolescent, Child, Newborn and Infant, these 
# groups were calculated as of the start of 2014. 