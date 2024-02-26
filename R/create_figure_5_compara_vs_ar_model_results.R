########################################################################################
#
# Author: Mark D. Nelms, Ph.D., mnelms@rti.org
#
# Version: 1.0 
#
#
# Description: 
#
# Notes:
#
#
# Potential Issues: None known
#########################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,
  janitor,
  ggplot2,
  scales,
  ggthemes,
  geomtextpath,
  patchwork,
  tidyverse
)

input <- "inputs"
output <- "outputs"
figures <- "figures"


# Load data ---------------------------     ---------------------------     ---------------------------


## CoMPARA and 14-assay AR model predictions
compara_ar14 <- read_csv(
  here::here(output,'/02/CoMPARA and AR14 predictions.csv'),
  show_col_types = FALSE
) %>% 
  select(-`...1`)


# AR agonist results - CoMPARA and AR model ---------------------------     ---------------------------     ---------------------------


## Keep chemicals in clusters where at least 1 chemical has an AR agonist prediction from CoMPARA
compara_model_agonist_clusters <- compara_ar14 %>% 
  group_by(cluster_id) %>% 
  filter(any(CoMPARA_Ago_pred == 1)) %>% 
  mutate(
    ar_mod_res = case_when(
      AUC.Agonist.x.bin == 1 ~ "agonist",
      AUC.Agonist.x.bin == 0 ~ "inactive",
      is.na(AUC.Agonist.x.bin) ~ "not tested"
    ),
    compara_ag_res = case_when(
      CoMPARA_Ago_pred == 1 ~ "agonist",
      CoMPARA_Ago_pred == 0 ~ "inactive"
    ),
    compara_ar_mod_res = case_when(
      compara_ag_res == "agonist" & ar_mod_res == "agonist" ~ "agonist/agonist",
      compara_ag_res == "agonist" & ar_mod_res == "inactive" ~ "agonist/inactive",
      compara_ag_res == "inactive" & ar_mod_res == "agonist" ~ "inactive/agonist",
      compara_ag_res == "inactive" & ar_mod_res == "inactive" ~ "inactive/inactive"
    )
  ) %>% 
  ungroup()

## Bar chart of CoMPARA predictions vs AR model results for chemicals
## in clusters where 1+ chemicals is predicted to be an AR agonist in CoMPARA
(com_mod_ag_bar <- ggplot(compara_model_agonist_clusters, aes(x = compara_ag_res, fill = ar_mod_res)) +
  geom_bar() +
    scale_x_discrete(
      name = "CoMPARA prediction",
      labels = c(
        "agonist" = "Agonist",
        "inactive" = "Inactive"
      )
    ) +
    scale_y_continuous(
      name = "Number of chemicals",
      breaks = scales::breaks_pretty()
    ) +
  scale_fill_manual(
    name = "AR Model",
    values = c(
      "agonist" = "#C43D4D",
      "inactive" = "#1E87A5",
      "not tested" = "#B3B5B8"
    ),
    labels = c(
      "agonist" = "Agonist",
      "inactive" = "Inactive",
      "not tested" = "Not Tested"
    )
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.title = element_text(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    #legend.justification = c(1, 1),
    legend.position = "right",
    legend.text = element_text(size = 10), # Reduce size of legend text
    legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
    legend.direction = "vertical",
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    text = element_text(family = "Inconsolata")
  )
)

## Save above bar chart
ggsave(
  com_mod_ag_bar,
  filename = here::here(figures, "Fig5A_CoMPARA agonist vs AR model results.jpg"),
  device = "jpg",
  width = 15,
  height = 26,
  units = "cm"
)

## Bar chart of CoMPARA predictions vs AR model results for chemicals
## in clusters where 1+ chemicals is predicted to be an AR agonist in CoMPARA
## broken out by cluster
(com_mod_ag_bar_per_clus <- compara_model_agonist_clusters %>%
  filter(!is.na(AUC.Agonist.x.bin)) %>%
  mutate(cluster_id = as_factor(cluster_id)) %>%
  ggplot(aes(x = cluster_id, fill = compara_ar_mod_res)) +
  geom_bar() +
  scale_x_discrete(
    name = "Clusters"
  ) +
  scale_y_continuous(
    name = "Number of chemicals",
    breaks = scales::breaks_pretty()
  ) +
  scale_fill_viridis(
    name = "CoMPARA/AR Model\nResults",
    labels = c(
      "agonist/agonist" = "Agonist/Agonist",
      "agonist/inactive" = "Agonist/Inactive",
      "inactive/agonist" = "Inactive/Agonist",
      "inactive/inactive" = "Inactive/Inactive"
    ),
    discrete = TRUE
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.title = element_text(),
    axis.text.x = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # legend.justification = c(1, 1),
    legend.position = "right",
    legend.text = element_text(size = 10), # Reduce size of legend text
    legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
    legend.direction = "vertical",
    legend.title.align = 0.5,
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    text = element_text(family = "Inconsolata")
  )
)

## Save above bar chart
ggsave(
  com_mod_ag_bar_per_clus,
  filename = here::here(figures, "Fig5B_CoMPARA agonist vs AR model results per cluster.jpg"),
  device = "jpg",
  width = 26,
  height = 15,
  units = "cm"
)


# AR antagonist results - CoMPARA and AR model ---------------------------     ---------------------------     ---------------------------


## Keep chemicals in clusters where at least 1 chemical has an AR antagonist prediction from CoMPARA
compara_model_antagonist_clusters <- compara_ar14 %>% 
  group_by(cluster_id) %>% 
  filter(any(CoMPARA_Anta_pred == 1)) %>% 
  mutate(
    ar_mod_res = case_when(
      AUC.Antagonist.x.bin == 1 ~ "antagonist",
      AUC.Antagonist.x.bin == 0 ~ "inactive",
      is.na(AUC.Antagonist.x.bin) ~ "not tested"
    ),
    compara_antag_res = case_when(
      CoMPARA_Anta_pred == 1 ~ "antagonist",
      CoMPARA_Anta_pred == 0 ~ "inactive"
    ),
    compara_ar_mod_res = case_when(
      compara_antag_res == "antagonist" & ar_mod_res == "antagonist" ~ "antagonist/antagonist",
      compara_antag_res == "antagonist" & ar_mod_res == "inactive" ~ "antagonist/inactive",
      compara_antag_res == "inactive" & ar_mod_res == "antagonist" ~ "inactive/antagonist",
      compara_antag_res == "inactive" & ar_mod_res == "inactive" ~ "inactive/inactive"
    )
  ) %>% 
  ungroup()

## Bar chart of CoMPARA predictions vs AR model results for chemicals
## in clusters where 1+ chemicals is predicted to be an AR agonist in CoMPARA
(com_mod_antag_bar <- ggplot(compara_model_antagonist_clusters, aes(x = compara_antag_res, fill = ar_mod_res)) +
    geom_bar() +
    scale_x_discrete(
      name = "CoMPARA prediction",
      labels = c(
        "antagonist" = "Antagonist",
        "inactive" = "Inactive"
      )
    ) +
    scale_y_continuous(
      name = "Number of chemicals",
      breaks = scales::breaks_pretty()
    ) +
    scale_fill_manual(
      name = "AR Model",
      values = c(
        "antagonist" = "#C43D4D",
        "inactive" = "#1E87A5",
        "not tested" = "#B3B5B8"
      ),
      labels = c(
        "antagonist" = "Antagonist",
        "inactive" = "Inactive",
        "not tested" = "Not Tested"
      )
    ) +
    ggthemes::theme_fivethirtyeight() +
    theme(
      axis.title = element_text(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      #legend.justification = c(1, 1),
      legend.position = "right",
      legend.text = element_text(size = 10), # Reduce size of legend text
      legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
      legend.direction = "vertical",
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent"),
      text = element_text(family = "Inconsolata")
    )
)

## Save above bar chart
ggsave(
  com_mod_antag_bar,
  filename = here::here(figures, "Fig5C_CoMPARA antagonist vs AR model results.jpg"),
  device = "jpg",
  width = 15,
  height = 26,
  units = "cm"
)

## Bar chart of CoMPARA predictions vs AR model results for chemicals
## in clusters where 1+ chemicals is predicted to be an AR agonist in CoMPARA
## broken out by cluster
(com_mod_antag_bar_per_clus <- compara_model_antagonist_clusters %>%
    filter(!is.na(AUC.Antagonist.x.bin)) %>%
    mutate(cluster_id = as_factor(cluster_id)) %>%
    ggplot(aes(x = cluster_id, fill = compara_ar_mod_res)) +
    geom_bar() +
    scale_x_discrete(
      name = "Clusters"
    ) +
    scale_y_continuous(
      name = "Number of chemicals",
      breaks = scales::breaks_pretty()
    ) +
    scale_fill_viridis(
      name = "CoMPARA/AR Model\nResults",
      labels = c(
        "antagonist/antagonist" = "Antagonist/Antagonist",
        "antagonist/inactive" = "Antagonist/Inactive",
        "inactive/antagonist" = "Inactive/Antagonist",
        "inactive/inactive" = "Inactive/Inactive"
      ),
      discrete = TRUE
    ) +
    ggthemes::theme_fivethirtyeight() +
    theme(
      axis.title = element_text(),
      axis.text.x = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      # legend.justification = c(1, 1),
      legend.position = "right",
      legend.text = element_text(size = 10), # Reduce size of legend text
      legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
      legend.direction = "vertical",
      legend.title.align = 0.5,
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent"),
      text = element_text(family = "Inconsolata")
    )
)

## Save above bar chart
ggsave(
  com_mod_antag_bar_per_clus,
  filename = here::here(figures, "Fig5D_CoMPARA antagonist vs AR model results per cluster.jpg"),
  device = "jpg",
  width = 26,
  height = 15,
  units = "cm"
)


# Combine panels to 1 jpg ---------------------------     ---------------------------     ---------------------------


combined_plots <- com_mod_ag_bar + com_mod_ag_bar_per_clus +
  com_mod_antag_bar + com_mod_antag_bar_per_clus +
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1, 4))

ggsave(
  combined_plots,
  filename = here::here(figures, "Fig5_Combined bar charts CoMPARA vs AR model results.jpg"),
  device = "jpg",
  width = 26,
  height = 15,
  units = "cm"
)
