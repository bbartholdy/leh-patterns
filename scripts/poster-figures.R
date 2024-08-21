# This script provides the figures that are used on the poster
library(ggplot2)
library(dplyr)
library(patchwork)

source("scripts/01_wrangle-data.R")


# Missing teeth in sample ------------------------------------------------


## proportion of each tooth present in the full sample
  # plot provides the colours and legend for the poster
  # colours are added to /figures/dental_arcade_empty_mapping.jpg in inkscape
percent_inventory <- dental_inventory_long |>
  group_by(tooth) |>
  summarise(
    n = n(),
    present = sum(presence)
  ) |>
  mutate(prop = round(present / n, 2))

percent_inventory |>
  ggplot(aes(x = tooth, fill = prop)) +
    geom_bar() +
    #scale_fill_viridis_c(limits = c(0,1))
    scale_fill_viridis_c() +
    theme(legend.text.position = "left")
ggsave("figures/ePPA2024-poster_missing-teeth_cols-legend.png", width = 9, height = 6, units = "in")


# LEH patterns -----------------------------------------------------------


## Lesion frequency per tooth in full sample
  # plot provides the colours and legend for the poster
  # colours are added to /figures/dental_arcade_empty_mapping.jpg in inkscape
lesion_freq_full <- hypoplasia_long |>
  group_by(tooth) |>
  dental_ratio(count = leh_bin)

lesion_freq_full |>
  ggplot(aes(x = tooth, fill = ratio)) +
    geom_bar() +
    #scale_fill_viridis_c(limits = c(0,1))
    scale_fill_viridis_c()
ggsave("figures/ePPA2024-poster_full-lesion-frequency_cols-legend.png", width = 10, height = 6, units = "in")


## Lesion frequency in complete-scores sample

lesion_freq_complete <- hypoplasia_scored_long |>
  group_by(tooth) |>
  dental_ratio(count = leh_bin)

# produce a plot for the colours and legend
lesion_freq_complete |>
  ggplot(aes(x = tooth, fill = ratio)) +
    geom_bar() +
    scale_fill_viridis_c()
    #scale_fill_viridis_c(limits = c(0,1)) # extend the legend to 1? (will show lesion frequency out of total possible frequency for the sample)
#ggsave("figures/lesion-frequency_cols-legend.png", width = 8, height = 6, units = "in")


## Isomere and antimere symmetry

ul_scores_plot <- hypoplasia_present_long |>
  filter(region == "maxilla" & side == "left") |>
  ggplot(aes(x = tooth, y = ID, fill = as_factor(leh_bin))) +
    geom_tile() +
    scale_x_discrete(position = "top") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      #axis.title.x = element_blank()
    )

ur_scores_plot <- hypoplasia_present_long |>
  filter(region == "maxilla" & side == "right") |>
  ggplot(aes(x = tooth, y = ID, fill = as_factor(leh_bin))) +
    geom_tile() +
    scale_x_discrete(position = "top")

ll_scores_plot <- hypoplasia_present_long |>
  filter(region == "mandible" & side == "left") |>
  ggplot(aes(x = tooth, y = fct_rev(ID), fill = as_factor(leh_bin))) +
    geom_tile() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
  scale_x_discrete(limits = c("LLI1", "LLI2", "LLC1", "LLP1", "LLP2", "LLM1", "LLM2"))

lr_scores_plot <- hypoplasia_present_long |>
  filter(region == "mandible" & side == "right") |>
  ggplot(aes(x = tooth, y = fct_rev(ID), fill = as_factor(leh_bin))) +
    geom_tile() +
    scale_x_discrete(limits = rev(c("LRI1", "LRI2", "LRC1", "LRP1", "LRP2", "LRM1", "LRM2")))

(ur_scores_plot + ul_scores_plot) /
  (lr_scores_plot + ll_scores_plot) + plot_layout(guides = "collect", axis_titles = "collect", axes = "collect") &
  labs(x = "Tooth", y = "ID", fill = "LEH lesions") &
  theme(
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) &
  #scale_fill_manual(labels = c("Present", "Absent"), values = viridisLite::magma(2, begin = 0.3, end = 0.7))
  scale_fill_manual(labels = c("Present", "Absent"), values = viridisLite::viridis(2, begin = 0.1, end = 0.7))

ggsave("figures/ePPA2024-poster_leh-quadrant-symmetry_heatmap.png", width = 9, height = 6)
