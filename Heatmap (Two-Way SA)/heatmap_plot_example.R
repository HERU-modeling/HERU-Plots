rm(list = ls()) # to clean the workspace

library(dplyr) # to manipulate data
library(reshape2) # to transform data
library(ggplot2) # for nice looking plots
library(tidyverse)
library(data.table)
library(tidyr)
library(RColorBrewer)
library(Rmisc)
library(grid)
library(gridExtra)
library(lattice)

# Call model setup functions
# source("R/input_parameter_functions.R")
# source("R/outcome_functions.R")

# Load parameters
# source("Analysis/00_load_parameters.R")

load(file = "Heatmap (Two-Way SA)/data_input.RData")

# Labels
df_dsa_twsa_itt_ps_labels <- read.csv(file = "Heatmap (Two-Way SA)/labels.csv", header = TRUE, colClasses = c(NA, NA, "NULL", "NULL", "NULL", "NULL"))

# Combine outcomes into data frame
df_incremental_temp_itt_ps <- cbind(df_dsa_twsa_itt_ps_labels, df_incremental_twsa_itt_ps_scaled)

df_twsa_itt_ps <- df_incremental_temp_itt_ps %>%
  select("perc_improvement_tx", "perc_improvement_death", "n_inc_qalys_adj_2020_scaled", "n_inc_odf_adj_2020_scaled", "n_inc_odn_adj_2020_scaled")

# Cohort scaling factor
# n_pop_cohort <- l_params_bnx_itt$n_pop_oat

# Scale outputs
df_twsa_itt_ps_scaled <- df_twsa_itt_ps %>%
  as_tibble() %>%
  mutate(
    n_inc_qalys_adj_total_max_scaled = n_inc_qalys_adj_2020_scaled,
    n_inc_odf_adj_max_scaled = n_inc_odf_adj_2020_scaled,
    n_inc_odn_adj_max_scaled = n_inc_odn_adj_2020_scaled
  )

plot_twsa_itt_ps_ly <- ggplot(df_twsa_itt_ps_scaled, aes(x = perc_improvement_tx, y = perc_improvement_death, fill = n_inc_qalys_adj_total_max_scaled)) +
  theme_bw() +
  geom_tile() +
  # Default of midpoint = 0.5, can change this to other values to re-center the midpoint
  scale_fill_gradient2(
    low = "#FF0000",
    mid = "#FFFFCC",
    high = "#075AFF",
    limits = c(-4000, 1500)
  ) +
  geom_text(aes(label = round(n_inc_qalys_adj_total_max_scaled, 0)), color = "black", size = 1.8, fontface = "bold") +
  scale_x_continuous(breaks = c(-.40, -.30, -.20, -.10, 0, .10, .20, .30, .40), labels = c("+40%", "+30%", "+20%", "+10%", "Base", "-10%", "-20%", "-30%", "-40%")) +
  scale_y_continuous(breaks = c(-.40, -.30, -.20, -.10, 0, .10, .20, .30, .40), labels = c("+40%", "+30%", "+20%", "+10%", "Base", "-10%", "-20%", "-30%", "-40%")) +
  labs(
    x = "X Label",
    y = "Y Label"
  ) +
  theme(
    axis.title.x = element_text(size = 8, hjust = 0.5),
    axis.title.y = element_text(size = 8, hjust = 0.5)
  ) +
  theme(
    axis.text = element_text(size = 7, color = "black")
  ) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(
    c(0, 0.35, 0, 0.35),
    "cm"
  )) +
  coord_fixed()

ggsave(plot_twsa_itt_ps_ly,
  filename = "Heatmap (Two-Way SA)/heatmap_twsa_plot_example.png",
  width = 6, height = 6, dpi = 600
)
