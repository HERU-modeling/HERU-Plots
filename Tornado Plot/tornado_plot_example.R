rm(list = ls()) # to clean the workspace

library(dplyr) # to manipulate data
library(reshape2) # to transform data
library(ggplot2) # for nice looking plots
library(tidyverse)
library(data.table)
library(tidyr)
library(RColorBrewer)

## Load DSA output files
# QALYs
load(file = "Tornado Plot/data_input.RData")

# Load data sets
df_owsa_itt_ps <- l_owsa_itt_ps$df_owsa_itt_ps

# DSA labels
df_dsa_ly_labels_itt_ps <- read.csv(file = "tornado_labels.csv", header = TRUE)

# Subset by mean
# Deterministic
n_base_itt_ps <- l_owsa_itt_ps$n_base_itt_ps

## Combine data frames
# ITT-PS
df_owsa_itt_ps <- as_tibble(df_owsa_itt_ps) %>%
  mutate(base = n_base_itt_ps) %>%
  mutate(diff = ifelse(abs(Upper - Lower) > 0, abs(Upper - Lower), abs(base - Upper)))

#########################
#### Tornado diagram ####
#########################
# ITT-PS
v_order_parameters <- df_owsa_itt_ps %>%
  arrange(diff) %>%
  mutate(var_name = factor(x = var_name, levels = var_name)) %>%
  select(var_name) %>%
  unlist() %>%
  levels()

# width of columns in plot (value between 0 and 1)
width <- 0.6
# get data frame in shape for ggplot and geom_rect
df.2 <- df_owsa_itt_ps %>%
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key = "type", value = "output.value", Lower:Upper) %>%
  # just reordering columns
  select(var_name, type, output.value, diff, base) %>%
  # create the columns for geom_rect
  mutate(
    var_name = factor(var_name, levels = v_order_parameters),
    ymin = pmin(output.value, base),
    ymax = pmax(output.value, base),
    xmin = as.numeric(var_name) - width / 2,
    xmax = as.numeric(var_name) + width / 2
  )
# Add value labels for SA ranges
data_merge2 <- inner_join(df.2, df_dsa_ly_labels_itt_ps, by = c("var_name", "type")) # Applying inner_join() function

# create plot
# (use scale_x_continuous to change labels in y axis to name of parameters)
p_tornado_itt_ps <- ggplot() +
  geom_rect(
    data = data_merge2,
    aes(ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin, fill = type)
  ) +
  geom_text(data = data_merge2, aes(y = output.value, x = (xmax + xmin) / 2, label = dsa_itt_ps_u), hjust = 1) +
  geom_text(data = data_merge2, aes(y = output.value, x = (xmax + xmin) / 2, label = dsa_itt_ps_l), hjust = 0) +
  theme_bw() +
  scale_fill_manual(values = c(
    "Upper" = "midnightblue",
    "Lower" = "slategray2"
  )) +
  theme(
    axis.title.y = element_blank(), legend.position = "none",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = df.2$base) +
  scale_x_continuous(
    breaks = c(seq_along(v_order_parameters)),
    labels = v_order_parameters
  ) +
  xlab("Parameter") +
  ylab("Incremental life years") +
  ylim(-3500, 0) +
  coord_flip()

# Output plots
ggsave("tornado_plot_example.png", p_tornado_itt_ps, height = 5, width = 7, dpi = 320)
