#******************************************************************************#
# Author : Suchit Mehrotra
# Editor : Halley Brantley
# Purpose: 
#   this script includes a cost benefit analysis to determine how much
#   an intervention would need to reduce the re-admission rate to achieve a 
#   return on investment (ROI) of 0, 1, 2, and 3
#******************************************************************************#
library(tidyverse)
library(glue)
library(devtools)
load_all("../../savvyR")

# Costs per person
program_spend <- 400
avg_readmit_costs <- c(7500, 12500, 15000, 20000)

# Readmission rate in population
old_readmission <- 0.2
new_readmission <- c(0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2)

tbl <- 
  expand.grid(
    old_readmission, 
    program_spend, 
    avg_readmit_costs, 
    new_readmission
  ) %>%
  tibble() %>%
  rename( 
    old_readmit_rate = Var1, 
    program_spend = Var2, 
    avg_readmit_costs = Var3, 
    new_readmit_rate = Var4
  )

profit_table <- 
  tbl %>%
  mutate(
    old_readmission_spend =  old_readmit_rate * avg_readmit_costs,
    new_readmission_spend = new_readmit_rate * avg_readmit_costs,
    total_spend = (program_spend + new_readmission_spend),
    pmpm_profit = (old_readmission_spend - total_spend) 
  ) %>%
  select(avg_readmit_costs, new_readmit_rate, pmpm_profit)

roi_labels <- 
  data.frame(
    x = rep(0.2, 4), 
    y = c(-400, 400, 800, 1200),
    label = glue('ROI: {c(-1, 1, 2, 3)}')
  )

profit_table %>%
  ggplot(
    aes(
      x = new_readmit_rate,
      y = pmpm_profit
    )
  ) + 
  geom_line(aes(color = factor(avg_readmit_costs)), size = 2) + 
  geom_hline(yintercept = c(-400, 400, 800, 1200), 
             col = 'red', 
             linetype = 2, 
             size = 1) +
  geom_label(data = roi_labels, aes(x = x, y = y, label = label)) +
  scale_x_reverse() +
  scale_color_rnd() +
  theme_rnd() + 
  labs(
    x = "New Readmit Rate", 
    y = "Profit ($)", 
    color = "Avg. Readmit \n Cost ($)",
    subtitle = "Figure 4. Potential business scenarios with a $400 intervention cost"
  ) 

ggsave(filename = "../../figures/cost_scenarios.png", width = 9, height = 6)
  
  

