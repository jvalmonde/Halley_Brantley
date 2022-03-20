#**********************************************************************#
# AUTHOR: Halley Brantley
# EDITORS:
# DESCRIPTION: makes a plot to find the location of discharge for
#   hospitalizations
# INPUTS: 
# OUTPUTS: 
#**********************************************************************#
rm(list = ls())

library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
load_all("~/savvyR")

pid <- 'research-01-217611'
options(httr_oauth_cache="~/.httr-oauth",httr_oob_default = TRUE)

# Get members discharged to home

q_total <- glue("
SELECT 
  year_nbr,
  COUNT(DISTINCT savvy_pid) as cnt
FROM 
  `df_uhgrd.miniov_medical_claim` 
WHERE
  dscrg_sts_cd_desc <> 'unknown'
GROUP BY 
  year_nbr
ORDER BY
  year_nbr, 
  cnt desc                      
")

counts_total <- load_bq(pid, q_total)

q_discharged <- glue("
SELECT
  year_nbr, 
  dscrg_sts_cd_desc,
  COUNT(*) as discharge_cnt
FROM (
  SELECT DISTINCT
    year_nbr,
    dscrg_sts_cd_desc,
    savvy_pid,
    full_dt
  FROM 
    `df_uhgrd.miniov_medical_claim` 
  WHERE
    hce_srvc_typ_desc IN ('med/surg/icu', 'transplants')
)
  GROUP BY 
    year_nbr,
    dscrg_sts_cd_desc
  ORDER BY
    year_nbr, 
    discharge_cnt desc  
")

counts <- load_bq(pid, q_discharged)

counts %>% 
  group_by(year_nbr) %>%
  mutate(total = sum(discharge_cnt)) %>%
  filter(year_nbr == 2019, discharge_cnt > 1000) %>%
  ggplot(
    aes(
      x = fct_reorder(dscrg_sts_cd_desc, discharge_cnt), 
      y = discharge_cnt/1000
    )
  ) +
  geom_bar(
    aes(), 
    stat = "identity"
  ) +
  coord_flip () +
  theme_rnd(14) +
  scale_fill_rnd()+
  labs(x = NULL, y = "Discharges (thousands)", fill = NULL, title = '2019 Discharges') +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "top")
ggsave(file = "../../figures/discharges_2019.png", width = 11, height = 6)  
