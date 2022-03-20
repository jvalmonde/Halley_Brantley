#******************************************************************************#
# Author : Suchit Mehrotra
# Editor : Halley Brantley
# Purpose: 
#   Plot spend by categories based on model predictions
#******************************************************************************#
library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
library(scales)
load_all("../../savvyR")
options(httr_oauth_cache = "~/.httr-oauth", 
        httr_oob_default = TRUE,
        scipen = 20)

pid <- 'research-01-217611'
DB <- 'foc_interoperability.ini_932'
fig_folder <- '../../figures/readmission_model_30days'

ids_w_preds <- 
  glue("
    SELECT
      * 
    FROM 
      {DB}_basic_drg_preds
  ") %>%
  load_bq(db = pid, query = .)


# how many people are in each group; used for calculating spend in other
# situations
counts_by_group <- 
  ids_w_preds %>%
  mutate(ntile = ifelse(pred_quant <= 18, '<19', pred_quant)) %>% 
  mutate(
    ntile = factor(
      ntile, 
      levels = c("<19", "19", "20"), 
      labels = c("Bottom 90%", "90-95%", "Top 5%")
    )
  ) %>% 
  group_by(ntile) %>%
  summarise(member_count = n())

# Cost/spend information by category
q_preds_cost <-
  glue("
    SELECT
      a.savvy_pid,
      a.pred,
      a.readmit_30_flag,
      a.pred_quant,
      b.spend_30, 
      b.spend_60,
      b.spend_90,
      b.srvc_typ_cd,
      b.hce_srvc_typ_desc
    FROM
      {DB}_basic_drg_preds AS a
    LEFT JOIN
      {DB}_costs AS b
    ON 
      a.savvy_pid = b.savvy_pid
  ") 

spend_information <- 
  glue("
    SELECT
      savvy_pid,
      srvc_typ_cd,
      hce_srvc_typ_desc,
      pred_quant,
      SUM(spend_30) AS spend_30
    FROM
      ({q_preds_cost})
    GROUP BY
      savvy_pid,
      srvc_typ_cd,
      hce_srvc_typ_desc, 
      pred_quant
  ") %>%
  load_bq(db = pid, query = .)

# Separate 90-95th percentiles and 95-100 percentiles of predictions
spend_w_count <- 
  spend_information %>%
  mutate(ntile = ifelse(pred_quant <= 18, '<19', pred_quant)) %>% 
  mutate(
    ntile = factor(
      ntile, 
      levels = c("<19", "19", "20"), 
      labels = c("Bottom 90%", "90-95%", "Top 5%")
    )
  ) %>% 
  left_join(
    counts_by_group, 
    by = "ntile"
  )

################################################################################
# Plot ip spend in 30 days post-discharge by category 
ip_spend <-
  spend_w_count %>%
  filter(srvc_typ_cd == 'ip') %>%
  group_by(ntile, hce_srvc_typ_desc, member_count) %>%
  summarise(
    total_spend = sum(spend_30)
  ) %>%
  mutate(pmpm = total_spend / member_count)


ip_spend %>% 
  ggplot(
    aes(
      x = fct_reorder(hce_srvc_typ_desc, pmpm), 
      y = pmpm, 
      fill = factor(ntile)
    )
  ) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_rnd() + 
  scale_fill_rnd() +
  labs(
    x = "Service Description", 
    y = "Spend ($)",
    title = "Average Inpatient Spend by Category", 
    subtitle = "Thirty Days After Discharge", 
    fill = "Risk Group"
  )

ggsave(filename = file.path(fig_folder, "ip_spend_by_risk_group.png"),
       width = 10, 
       height = 6)
##############################################################################
# Box plot of re-admission costs of med/surg/icu 
costs_by_person <- 
  glue("
    SELECT
      savvy_pid, 
      readmit_30_flag,
      pred_quant,
      SUM(med_spend) AS med_spend, 
      SUM(snf_spend) AS snf_spend 
    FROM (
      SELECT
        savvy_pid, 
        pred_quant,
        CASE 
          WHEN hce_srvc_typ_desc = 'med/surg/icu' THEN spend_30
          ELSE 0
        END AS med_spend, 
        CASE 
          WHEN hce_srvc_typ_desc = 'skilled nursing' THEN spend_30
          ELSE 0
        END AS snf_spend, 
        readmit_30_flag
      FROM 
        ({q_preds_cost})
    )
    GROUP BY 
      savvy_pid, 
      readmit_30_flag,
      pred_quant
  ") %>%
  load_bq(db = pid, query = .) %>%
  mutate(ntile = ifelse(pred_quant <= 18, '<19', pred_quant)) %>% 
  mutate(
    ntile = factor(
      ntile, 
      levels = c("<19", "19", "20"), 
      labels = c("Bottom 90%", "90-95%", "Top 5%")
    )
  )

costs_by_person %>%
  group_by(ntile, readmit_30_flag) %>%
  summarise(
    snf_spend = mean(snf_spend), 
    med_spend = mean(med_spend)
  )

readmit_costs <- costs_by_person %>%
  pivot_longer(cols = c("med_spend", "snf_spend")) %>% 
  filter(name == 'med_spend', 
         readmit_30_flag == 1)


readmit_costs %>%
ggplot(aes(x = ntile, y = value)) + 
  geom_boxplot(position = "dodge") + 
  coord_cartesian(ylim = c(0, 30000)) +
  theme_rnd() +
  labs(
    y = "Re-admission Cost ($)", 
    x = NULL
  )

ggsave(filename = file.path(fig_folder, "readmission_cost_boxplot.png"), 
       width = 13, height = 7)

##############################################################################
# Plots below are not used in presentations/report
##############################################################################
# plot total spend in 30 days after discharge by prediction group
total_spend <- 
  spend_w_count %>%
  group_by(savvy_pid, ntile) %>%
  summarise(
    total_spend = sum(spend_30)
  )

avg_spend <- 
  total_spend %>% 
  group_by(ntile) %>% 
  summarise(total_spend = mean(total_spend))

ggplot(total_spend, aes(x = ntile, y = total_spend)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 4e04)) +
  geom_point(data = avg_spend, col = 'red', size = 3) +
  geom_label(data = avg_spend, aes(y = total_spend - 2500, label = comma(total_spend, 1))) +
  theme_rnd() + 
  labs(
    x = "Risk Group", 
    y = "Total Spend ($)", 
    title = "Spend by Risk Group",
    subtitle = "Thirty Days After Discharge"
  )
ggsave(filename = file.path(fig_folder, "total_spend_boxplots.png"),
       width = 10, 
       height = 6)

################################################################################
# plot average costs of med/surg/icu and skilled nursing by readmit status

costs_by_readmit_status <- 
  glue("
    select
      readmit_30_flag, 
      hce_srvc_typ_desc,
      SUM(spend_30) as total_spend, 
      pred_quant
    FROM
      ({q_preds_cost})
    GROUP BY
      readmit_30_flag, 
      pred_quant,
      hce_srvc_typ_desc
  ") %>%
  load_bq(db = pid, query = .)

costs_by_readmit_status <- 
  costs_by_readmit_status %>%  
  mutate(pred_quant = ifelse(pred_quant <= 18, '<19', pred_quant)) %>% 
  mutate(
    pred_quant = factor(
      pred_quant, 
      levels = c("<19", "19", "20"), 
      labels = c("Bottom 90%", "90-95%", "Top 5%")
    )
  ) %>% 
  group_by(pred_quant, readmit_30_flag, hce_srvc_typ_desc) %>%
  summarise(total_spend = sum(total_spend))

counts_by_readmit_ntile <- 
  ids_w_preds %>%
  mutate(pred_quant = ifelse(pred_quant <= 18, '<19', pred_quant)) %>% 
  mutate(
    pred_quant = factor(
      pred_quant, 
      levels = c("<19", "19", "20"), 
      labels = c("Bottom 90%", "90-95%", "Top 5%")
    )
  ) %>% 
  group_by(pred_quant, readmit_30_flag) %>%
  summarise(member_count = n())

costs_by_readmit_status %>%
left_join(
  counts_by_readmit_ntile,
  by = c("pred_quant", "readmit_30_flag")
) %>%
mutate(avg_spend = total_spend / member_count) %>%
filter(hce_srvc_typ_desc %in% c("med/surg/icu", "skilled nursing")) %>%
ggplot(aes(x = pred_quant, y = avg_spend, fill = factor(readmit_30_flag))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ hce_srvc_typ_desc) + 
  theme_rnd() + 
  scale_fill_rnd() +
  labs(fill = "Readmit Flag")

ggsave(filename = file.path(fig_folder, "ip_spend_by_risk_group.png"),
       width = 10, 
       height = 6)

################################################################################
# density plot of med/surg/icu spend
costs_by_person %>%
  filter(readmit_30_flag == 1) %>%
  select(ntile, med_spend) %>%
  ggplot(aes(x = med_spend, color = ntile)) + 
  geom_density() + 
  labs(
    x = "Med/Surg/ICU Spend", 
    y = "", 
    color = "Risk Group"
  ) + 
  theme_rnd() +
  scale_color_rnd()

ggsave(filename = file.path(fig_folder, "density_readmit_cost.png"),
       width = 10, 
       height = 6)

# density plot of snf spend
costs_by_person %>%
  select(ntile, snf_spend) %>%
  ggplot(aes(x = snf_spend, color = ntile)) + 
  geom_density() + 
  labs(
    x = "SNF Spend", 
    y = "", 
    color = "Risk Group"
  ) +
  theme_rnd() +
  scale_color_rnd()

ggsave(filename = file.path(fig_folder, "density_snf_cost.png"),
       width = 10, 
       height = 6)







