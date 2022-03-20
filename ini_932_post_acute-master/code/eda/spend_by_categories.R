#**********************************************************************#
# AUTHOR: Halley Brantley
# EDITORS:
# DESCRIPTION: 
#   plots to identify types of spend in different deciles of 30-day spend. We
#   want to see what the most expensive individuals are spending money on
#**********************************************************************#

rm(list = ls())

library(bigrquery)
library(tidyverse)
library(glue)
library(devtools)
library(lubridate)
library(ggrepel)
library(scales)
load_all("../../savvyR")

pid <- 'research-01-217611'
DB <- "foc_interoperability.ini_932"
options(httr_oauth_cache="~/.httr-oauth",httr_oob_default = TRUE)
options(scipen = 20)

costs <- load_bq(pid, glue("SELECT * FROM {DB}_cost_decile"))
cost_categories <- load_bq(pid, glue("SELECT * FROM {DB}_cost_categories"))

###############################################################################

readmits <- load_bq(pid, glue("SELECT * FROM {DB}_readmits"))

total_cost <- 
  load_bq(pid, glue("
    SELECT 
      age_group,
      SUM(spend_30) as total_cost,
      SUM(spend_30)/COUNT(DISTINCT savvy_pid) as cost_per_member
    FROM 
      {DB}_costs
    GROUP BY 
      age_group"))


costs_by_decile <- load_bq(pid, glue("
SELECT 
  age_group, 
  spend_30_decile,
  SUM(spend_30) as decile_cost,
  COUNT(DISTINCT savvy_pid) as n_members
FROM
  {DB}_cost_decile
GROUP BY 
  age_group,
  spend_30_decile
"))

#**********************************************************************#
# eda for the one-pager document; we just want to get some specific numbers
#**********************************************************************#
# Total Spend
total_cost %>%
  summarise(sum(total_cost))

# total spend by age group
cost_categories %>%
  group_by(age_group) %>%
  summarise(costs_billion = sum(total_cost) / 10^9) %>%
  mutate(total = sum(costs_billion))

# spend by category of service; want numbers for inpatient spend
cost_categories %>%
  group_by(hce_srvc_typ_desc) %>%
  summarise(costs_billion = sum(total_cost) / 10^9) %>%
  mutate(prop = costs_billion/sum(costs_billion)) %>%
  arrange(desc(prop))

# percet of SNF spend in top 3 deciles
cost_categories %>%
  filter(hce_srvc_typ_desc == 'skilled nursing') %>%
  group_by(spend_30_decile) %>%
  summarise(decile_spend = sum(total_cost)) %>%
  mutate(prop = decile_spend/sum(decile_spend)) %>%
  filter(spend_30_decile > 7) %>%
  summarise(sum(prop))

# proportion of spend for a cost decile; what proportion of money is being spend
# by the highest spending individuals
decile_summary <- 
  costs_by_decile %>%
  mutate(total_spend = sum(decile_cost)) %>%
  group_by(spend_30_decile, total_spend) %>%
  summarise(decile_spend = sum(decile_cost)) %>%
  mutate(prop = decile_spend / total_spend)

# costs in the top 3 deciles
decile_summary %>%
  filter(spend_30_decile > 7) %>%
  ungroup() %>%
  summarise(
    spend = sum(decile_spend), 
    prop = sum(prop)
  )

# spend by hce_srvc_typ for the top three deciles
cost_categories %>% 
  filter(srvc_typ_cd == 'ip') %>%
  mutate(total_spend = sum(total_cost)) %>%
  group_by(spend_30_decile, hce_srvc_typ_desc, total_spend) %>%
  summarise(decile_spend = sum(total_cost)) %>%
  ungroup() %>%
  mutate(prop = decile_spend / total_spend) %>%
  filter(spend_30_decile %in% c(8, 9, 10)) %>%
  arrange(spend_30_decile, hce_srvc_typ_desc) %>%
  select(spend_30_decile, hce_srvc_typ_desc, prop) %>%
  print(n = 100)

# Total SNF and Readmit Spend
cost_categories %>%
  group_by(hce_srvc_typ_desc) %>%
  summarise(cost = sum(total_cost)) %>%
  mutate(prop = cost/sum(cost),
         cost = cost/1e09) %>%
  arrange(desc(prop))

total_cost <- 
  costs_by_decile %>% 
  summarise(sum(decile_cost)) %>%
  unlist()

total_cost <- 
  costs_by_decile %>% 
  summarise(sum(decile_cost)) %>%
  unlist()

#**********************************************************************#
# make the total spend by decile plot
#**********************************************************************#
px <- 
  costs_by_decile %>%
  mutate(
    proportion = decile_cost/ total_cost
  ) %>%
  ggplot(
    aes(
      x = factor(spend_30_decile), 
      y = decile_cost/1e06, 
      group = age_group
    )
  ) + 
  geom_bar(
    aes(fill = age_group), 
    stat = "identity",
    position = "dodge"
  ) +
  geom_label(
    aes(label = percent(proportion, 1), group = age_group), 
    position = position_dodge(width = 1), 
    size = 3
  ) +
  labs(
    y = "Millions ($)", 
    x = "Spend Decile", 
    fill = NULL,
    subtitle = 
      glue("Figure 1: Medical Spend 30 Days Post Discharge \n (Total: {round(sum(total_cost)/1e09, 1)} billion)")
    ) +
  theme_rnd(15) +
  scale_fill_rnd() 
px

ggsave(px, filename = "../../figures/total_spend_by_decile.png", width = 8, height = 6)


#**********************************************************************#
# make the quantile plots
#**********************************************************************#
quants <- 
  costs %>%
  group_by(age_group) %>%
  summarise(
    across(starts_with("spend"), ~quantile(.x, probs = seq(0.005, .99, 0.005)))
  ) %>% 
  mutate(probs = seq(0.005, .99, 0.005)) %>% 
  select(-spend_30_decile) %>%
  pivot_longer(cols = starts_with("spend")) %>% 
  mutate(
    label = factor(
      name, 
      levels = c("spend_30", "spend_60", "spend_90"),
      labels = c("30 days", "60 days", "90 days")
    )
  )

quants %>%
  ggplot(aes(x = probs, y = value)) + 
  geom_line(size = 1.5, aes(col = label, linetype = age_group)) +
  labs(
    y = "Healthcare Spend (Non-Pharmacy $)",
    x = "Proportion of Post-discharge Individuals \n With Spend Below Threshold",
    col = NULL,
    linetype = NULL
  ) +
  theme_rnd() +
  scale_color_rnd()

ggsave("../../figures/spend_post_discharge.png", width = 8, height = 6)

quants %>%
  filter(probs < 0.6) %>%
  ggplot(aes(x = probs, y = value)) + 
  geom_line(size = 1.5, aes(col = label, linetype = age_group)) +
  labs(
    y = "Healthcare Spend (Non-Pharmacy $)",
    x = "Proportion of Post-discharge Individuals \n With Spend Below Threshold",
    col = NULL,
    linetype = NULL
  ) +
  geom_point(
    data = . %>% filter(probs == 0.5)
  ) +
  geom_label_repel(
    data = . %>% filter(probs == 0.5), 
    aes(label = round(value))
  ) +
  theme_rnd() +
  scale_color_rnd()

ggsave("../../figures/spend_post_discharge_05.png", width = 8, height = 6)

# get the costs for the population; want to plot spend in different
# categories to understand where the money is going
cost_plot <- cost_categories %>%
  filter(
    spend_30_decile > 7
  ) %>%
  group_by(spend_30_decile) %>%
  mutate(prop_cost = total_cost/sum(total_cost)) 

plot_categories <- function(df) {
  df %>%
    ggplot(aes(x = fct_reorder(hce_srvc_typ_desc, total_cost), 
               y = total_cost/1e06,
               fill = factor(spend_30_decile))) + 
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(x = "Spend Category", y = 'Millions ($)', fill = "Spend \n Decile") +
    theme_rnd() +
    scale_fill_rnd()
}

plot_categories(cost_plot %>% filter(srvc_typ_cd == 'ip')) + 
  labs(subtitle = "Fig. 2A Inpatient spend for top 3 spend deciles")

ggsave("../../figures/inpatient_spend.png", width = 8, height = 6)

cost_plot %>% 
  filter(
    srvc_typ_cd == 'op', 
    !hce_srvc_typ_desc %in% c(
      "mh/sa outpatient", "op hospice", "rx - pharmacy dispensed",
      "urgicenter"
    )
  ) %>%
  plot_categories() +
  labs(subtitle = "Fig. 2B Outpatient spend for top 3 spend deciles")

ggsave("../../figures/outpatient_spend.png", width = 8, height = 6)


# Get total hospital and snf spend in top 3 deciles
cost_plot %>%
  filter(srvc_typ_cd == 'ip') %>%
  ungroup() %>%
  group_by(hce_srvc_typ_desc) %>%
  summarise(cost = sum(total_cost)/1e09) %>%
  arrange(desc(cost))


cost_plot %>% 
  filter(
    srvc_typ_cd == 'dr',
    !hce_srvc_typ_desc %in% c(
      "otorhinolaryngology", "pulmonary", "wellness visits",
      "immunizations", "venipuncture", "ophthalmalogy - exams",
      "ophthalmalogy - services", "allergy treatment", "allergy tests"
    )
  ) %>%
  plot_categories() +
  labs(title = "Physician Spend")

ggsave("../../figures/physician_spend.png", width = 8, height = 6)

# Percent that have a re-admission
readmits %>% 
  group_by(age_group) %>%
  mutate(prop = member_count/sum(member_count)) %>%
  filter(readmit == 1) %>%
  ggplot(aes(x = age_group, y = member_count)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = percent(prop))) +
  theme_bw() +
  labs(x = NULL, y = "Individuals", title = "Re-admissions")

ggsave("../../figures/readmits.png", width = 8, height = 6)
