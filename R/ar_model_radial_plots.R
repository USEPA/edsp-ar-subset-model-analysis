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


# Parameters ---------------------------     ---------------------------     ---------------------------


auc_limits <- 0.21


# Load data files ---------------------------     ---------------------------     ---------------------------


## AR model information for 
## 2017 11-assay full model, 
## 2020 11-assay subset model matching 2017 full model, and
## 2020 14-assay full model
ar_model_info <- read_csv(
  here::here("outputs/02/AR14_AR11-2017_AR11sub.csv"), 
  show_col_types = FALSE) %>% 
  janitor::clean_names() %>% 
  select(
    dss_tox_gsid_ar11_2017, name_ar14, casrn_ar11_2017, starts_with("auc_a")
  ) %>% 
  # Rename columns to make it easier to manipulate later
  rename(
    dtxsid = dss_tox_gsid_ar11_2017, 
    name = name_ar14,
    casrn = casrn_ar11_2017,
    agonist_pred_ar11_2017 = auc_agonist_bin_ar11_2017,
    antagonist_pred_ar11_2017 = auc_antagonist_bin_ar14_2017,
    agonist_pred_ar14 = auc_agonist_bin_ar14,
    antagonist_pred_ar14 = auc_antagonist_bin_ar14,
    agonist_pred_ar11_2020 = auc_agonist_bin_ar11sub_2020,
    antagonist_pred_ar11_2020 = auc_antagonist_bin_ar11sub_2020
  ) %>% 
  relocate(contains("ar11_2017"), .before = auc_agonist_ar14) 


# 2017 model antagonist -ve and 2020 antagonist +ve ---------------------------     ---------------------------     ---------------------------


## Keep only chemicals where 2017 model has antagonist -ve prediction & 2020 models have antagonist +ve prediction
ar17_antag_neg_ar20_antag_pos <- ar_model_info %>% 
  filter(antagonist_pred_ar11_2017 == 0 & antagonist_pred_ar14 == 1 & antagonist_pred_ar11_2020 == 1)

## Convert to long form to make it easier for plotting
ar17_antag_neg_ar20_antag_pos_long <- ar17_antag_neg_ar20_antag_pos %>%
  pivot_longer(
    cols = contains("auc"),
    names_to = c("prediction", "source"),
    names_pattern = "auc_(agonist|antagonist)_(.*)", #Matches in 1st () will go to "prediction" col, matches in 2nd () will go to "source" col
    values_to = "auc"
  ) %>%
  relocate(c(prediction, source, auc), .after = casrn) %>%
  # Create col used for labelling in radial plots
  tidyr::unite("source2", c(source, prediction), remove = FALSE) %>%
  # Convert to factor and order so agonist & antagonist predictions are together
  mutate(
    source2 = factor(source2,
      levels = c(
        "ar11_2017_agonist", "ar11sub_2020_agonist", "ar14_agonist",
        "ar11_2017_antagonist", "ar11sub_2020_antagonist", "ar14_antagonist"
      )
    ),
    name_wrapped = str_wrap(name, width = 20, whitespace_only = FALSE), # Wraps chemical name at closest non-letter character after 20 characters - Needed for facet labelling
    auc_gt_0_21 = if_else(auc >= auc_limits, round(auc, digits = 2), NA_integer_) # Keep only AUC values beyond limits of radial plots
  ) 

## Radial plot for chemicals where 2017 AR model has -ve antagonist prediction, and
## 2020 AR model has +ve antagonist prediction
##  - Limiting y-axis to 0.21
(rad_plot_ar17_antag_neg_ar20_antag_pos_0.21 <- ar17_antag_neg_ar20_antag_pos_long %>%
  group_by(dtxsid) %>%
  ggplot(aes(x = source2, y = auc, fill = source)) +
  geom_col(position = "dodge") +
  # Add horizontal line at median subset_AUC
  geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
  # Add vertical line between agonist and antagonist models
  geom_vline(xintercept = c(0.5, 3.5), colour = "#222222") +
  # Add label for AUC beyond limits
  geom_text(
    aes(label = auc_gt_0_21), 
    size = 10/.pt, 
    family = "Inconsolata", 
    nudge_y = -0.04
  ) + 
  facet_wrap(facets = ~ fct_reorder(name_wrapped, auc)) + # Order facets by increasing AUC
  coord_curvedpolar() +
  scale_x_discrete(
    labels = c(
      "ar11_2017_agonist" = "AR11 2017\nAgonist",
      "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
      "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
      "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
      "ar14_agonist" = "AR14 2020\nAgonist",
      "ar14_antagonist" = "AR14 2020\nAntagonist"
    )
  ) +
  scale_y_continuous(
    name = "Model AUC",
    limits = c(0, auc_limits),
    breaks = scales::pretty_breaks(n = 5),
    oob = squish
  ) +
  scale_fill_manual(
    name = "Source",
    values = c(
      "ar11_2017" = "#bf4321",
      "ar11sub_2020" = "#b6c568",
      "ar14" = "#92ccd8"
    ),
    labels = c(
      "ar11_2017" = "AR11 2017",
      "ar11sub_2020" = "AR11 2020",
      "ar14" = "AR14 2020"
    ),
    aesthetics = c("fill")
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(vjust = 0.6),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.95, 0.2),
    legend.text = element_text(size = 14), # Reduce size of legend text
    legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
    legend.direction = "vertical",
    panel.background = element_rect(fill = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff"),
    strip.background = element_rect(fill = "#ffffff"),
    strip.text = element_text(size = 12),
    text = element_text(family = "Inconsolata")
  )
)

if (!file.exists(here::here(figs, "AR_2017_Antagonist_negative_2020_Antagonist_positive_radial_plots_0_21.png"))) {
  ## Save above radial plot
  ggsave(
    rad_plot_ar17_antag_neg_ar20_antag_pos_0.21,
    filename = "AR_2017_Antagonist_negative_2020_Antagonist_positive_radial_plots_0_21.png",
    path = here(figs),
    device = "png",
    height = 30,
    width = 30,
    units = "cm"
  )
}

## Radial plot for chemicals where 2017 AR model has -ve antagonist prediction, and
## 2020 AR model has +ve antagonist prediction
##  - Only chemicals with 1+ AUC >0.21
# (rad_plot_ar17_antag_neg_ar20_antag_pos_gt_0.21 <- ar17_antag_neg_ar20_antag_pos_long %>% 
#   group_by(dtxsid) %>% 
#   filter(any(auc > auc_limits)) %>% 
#   ggplot(aes(x = source2, y = auc, fill = source)) +
#   geom_col(position = "dodge") +
#   # Add horizontal line at median subset_AUC
#   geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
#   # Add vertical line between agonist and antagonist models
#   geom_vline(xintercept = c(0.5, 3.5), colour = "#222222") +
#   facet_wrap(facets = ~fct_reorder(name_wrapped, auc)) + # Order facets by increasing AUC
#   coord_curvedpolar() +
#   scale_x_discrete(
#     labels = c(
#       "ar11_2017_agonist" = "AR11 2017\nAgonist",
#       "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
#       "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
#       "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
#       "ar14_agonist" = "AR14 2020\nAgonist",
#       "ar14_antagonist" = "AR14 2020\nAntagonist"
#     )
#   ) +
#   scale_y_continuous(
#     name = "Model AUC",
#     limits = c(0, 1),
#     breaks = scales::pretty_breaks(n = 5),
#     oob = squish
#   ) +
#   scale_fill_manual(
#     name = "Source",
#     values = c(
#       "ar11_2017" = "#bf4321",
#       "ar11sub_2020" = "#b6c568",
#       "ar14" = "#92ccd8"
#     ),
#     labels = c(
#       "ar11_2017" = "AR11 2017",
#       "ar11sub_2020" = "AR11 2020",
#       "ar14" = "AR14 2020"
#     ),
#     aesthetics = c("fill")
#   ) +
#   ggthemes::theme_fivethirtyeight() +
#   theme(
#     axis.text.x = element_text(vjust = 0.6),
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     #legend.justification = c(1, 1),
#     #legend.position = c(0.9, 0.2),
#     legend.text = element_text(size = 10), # Reduce size of legend text
#     legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
#     legend.direction = "horizontal",
#     # panel.background = element_rect(fill = "transparent"),
#     # plot.background = element_rect(fill = "transparent"),
#     strip.text = element_text(size = 14),
#     text = element_text(family = "Inconsolata")
#   )
# )
# 
# if (!file.exists(here::here(figs, "AR_2017_Antagonist_negative_2020_Antagonist_positive_radial_plots_gt_0_21.png"))) {
#   ## Save above radial plot
#   ggsave(
#     rad_plot_ar17_antag_neg_ar20_antag_pos_gt_0.21,
#     filename = "AR_2017_Antagonist_negative_2020_Antagonist_positive_radial_plots_gt_0_21.png",
#     path = here(figs),
#     device = "png",
#     height = 25,
#     width = 25,
#     units = "cm"
#   )
# }

## Combine radial plots for chemicals where 2017 AR model has -ve antagonist prediction, and
## 2020 AR model has +ve antagonist prediction
# (ar17_antag_neg_ar20_pos_combine_plot <- rad_plot_ar17_antag_neg_ar20_antag_pos_0.21 + rad_plot_ar17_antag_neg_ar20_antag_pos_gt_0.21 +
#   # Left plot will be 2x as wide as right plot
#   plot_layout(widths = c(2, 1), guides = "collect") +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     panel.background = element_rect(fill = "#ffffff"),
#     plot.background = element_rect(fill = "#ffffff"),
#     strip.background = element_rect(fill = "#ffffff")
#   )
# )
# 
# if (!file.exists(here::here(figs, "AR_2017_Antagonist_negative_2020_Antagonist_positive_radial_plots_combined.png"))) {
#   ## Save combined radial plots
#   ggsave(
#     ar17_antag_neg_ar20_pos_combine_plot,
#     filename = "AR_2017_Antagonist_negative_2020_Antagonist_positive_radial_plots_combined.png",
#     path = here(figs),
#     device = "png",
#     height = 30,
#     width = 55,
#     units = "cm"
#   )
# }


# 2017 model agonist -ve and 2020 14-assay AR agonist +ve ---------------------------     ---------------------------     ---------------------------


## Keep only chemicals where 2017 model has agonist -ve prediction & 2020 AR14 model have agonist +ve prediction
ar17_ag_neg_ar20_ag_pos <- ar_model_info %>% 
  filter(agonist_pred_ar11_2017 == 0 & agonist_pred_ar14 == 1)

## Convert to long form to make it easier for plotting
ar17_ag_neg_ar20_ag_pos_long <- ar17_ag_neg_ar20_ag_pos %>%
  pivot_longer(
    cols = contains("auc"),
    names_to = c("prediction", "source"),
    names_pattern = "auc_(agonist|antagonist)_(.*)", #Matches in 1st () will go to "prediction" col, matches in 2nd () will go to "source" col
    values_to = "auc"
  ) %>%
  relocate(c(prediction, source, auc), .after = casrn) %>%
  # Create col used for labelling in radial plots
  tidyr::unite("source2", c(source, prediction), remove = FALSE) %>%
  # Convert to factor and order so agonist & antagonist predictions are together
  mutate(
    source2 = factor(source2,
                     levels = c(
                       "ar11_2017_agonist", "ar11sub_2020_agonist", "ar14_agonist",
                       "ar11_2017_antagonist", "ar11sub_2020_antagonist", "ar14_antagonist"
                     )
    ),
    name_wrapped = str_wrap(name, width = 20, whitespace_only = FALSE), # Wraps chemical name at closest non-letter character after 20 characters - Needed for facet labelling
    auc_gt_0_21 = if_else(auc >= auc_limits, round(auc, digits = 2), NA_integer_) # Keep only AUC values beyond limits of radial plots
  ) 

## Radial plot for chemicals where 2017 AR model has -ve agonist prediction, and
## 2020 14-assay AR model has +ve agonist prediction
##  - Limiting y-axis to 0.21
(rad_plot_ar17_ag_neg_ar20_ag_pos_0.21 <- ar17_ag_neg_ar20_ag_pos_long %>% 
    group_by(dtxsid) %>%
    ggplot(aes(x = source2, y = auc, fill = source)) +
    geom_col(position = "dodge") +
    # Add horizontal line at median subset_AUC
    geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
    # Add vertical line between agonist and antagonist models
    geom_vline(xintercept = c(0.5, 3.5), colour = "#222222") +
    geom_text(aes(label = auc_gt_0_21), size = 10/.pt, family = "Inconsolata", nudge_y = -0.04) + # Add label for AUC beyond limits
    facet_wrap(facets = ~ fct_reorder(name_wrapped, auc)) + # Order facets by increasing AUC
    coord_curvedpolar() +
    scale_x_discrete(
      labels = c(
        "ar11_2017_agonist" = "AR11 2017\nAgonist",
        "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
        "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
        "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
        "ar14_agonist" = "AR14 2020\nAgonist",
        "ar14_antagonist" = "AR14 2020\nAntagonist"
      )
    ) +
    scale_y_continuous(
      name = "Model AUC",
      limits = c(0, auc_limits),
      breaks = scales::pretty_breaks(n = 5),
      oob = squish
    ) +
    scale_fill_manual(
      name = "Source",
      values = c(
        "ar11_2017" = "#bf4321",
        "ar11sub_2020" = "#b6c568",
        "ar14" = "#92ccd8"
      ),
      labels = c(
        "ar11_2017" = "AR11 2017",
        "ar11sub_2020" = "AR11 2020",
        "ar14" = "AR14 2020"
      ),
      aesthetics = c("fill")
    ) +
    ggthemes::theme_fivethirtyeight() +
    theme(
      axis.text.x = element_text(vjust = 0.6),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.justification = c(1, 1),
      legend.position = c(0.75, 0.2),
      legend.text = element_text(size = 14), # Reduce size of legend text
      legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
      legend.direction = "vertical",
      panel.background = element_rect(fill = "#ffffff"),
      plot.background = element_rect(fill = "#ffffff"),
      strip.background = element_rect(fill = "#ffffff"),
      strip.text = element_text(size = 12),
      text = element_text(family = "Inconsolata")
    )
)

if (!file.exists(here::here(figs, "AR11_2017_Agonist_negative_AR14_2020_Agonist_positive_radial_plots_0_21.png"))) {
  ## Save above radial plot
  ggsave(
    rad_plot_ar17_ag_neg_ar20_ag_pos_0.21,
    filename = "AR11_2017_Agonist_negative_AR14_2020_Agonist_positive_radial_plots_0_21.png",
    path = here(figs),
    device = "png",
    height = 30,
    width = 30,
    units = "cm"
  )
}

## Radial plot for chemicals where 2017 AR model has -ve agonist prediction, and
## 2020 14-assay AR model has +ve agonist prediction
##  - Only chemicals with 1+ AUC >0.21
# (rad_plot_ar17_ag_neg_ar20_ag_pos_gt_0.21 <- ar17_ag_neg_ar20_ag_pos_long %>% 
#     group_by(dtxsid) %>% 
#     filter(any(auc > auc_limits)) %>% 
#     ggplot(aes(x = source2, y = auc, fill = source)) +
#     geom_col(position = "dodge") +
#     # Add horizontal line at median subset_AUC
#     geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
#     # Add vertical line between agonist and antagonist models
#     geom_vline(xintercept = c(0.5, 3.5), colour = "#222222") +
#     facet_wrap(facets = ~fct_reorder(name_wrapped, auc)) + # Order facets by increasing AUC
#     coord_curvedpolar() +
#     scale_x_discrete(
#       labels = c(
#         "ar11_2017_agonist" = "AR11 2017\nAgonist",
#         "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
#         "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
#         "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
#         "ar14_agonist" = "AR14 2020\nAgonist",
#         "ar14_antagonist" = "AR14 2020\nAntagonist"
#       )
#     ) +
#     scale_y_continuous(
#       name = "Model AUC",
#       limits = c(0, 1),
#       breaks = scales::pretty_breaks(n = 5),
#       oob = squish
#     ) +
#     scale_fill_manual(
#       name = "Source",
#       values = c(
#         "ar11_2017" = "#bf4321",
#         "ar11sub_2020" = "#b6c568",
#         "ar14" = "#92ccd8"
#       ),
#       labels = c(
#         "ar11_2017" = "AR11 2017",
#         "ar11sub_2020" = "AR11 2020",
#         "ar14" = "AR14 2020"
#       ),
#       aesthetics = c("fill")
#     ) +
#     ggthemes::theme_fivethirtyeight() +
#     theme(
#       axis.text.x = element_text(vjust = 0.6),
#       legend.background = element_blank(),
#       legend.key = element_blank(),
#       #legend.justification = c(1, 1),
#       #legend.position = c(0.9, 0.2),
#       legend.text = element_text(size = 10), # Reduce size of legend text
#       legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
#       legend.direction = "horizontal",
#       # panel.background = element_rect(fill = "transparent"),
#       # plot.background = element_rect(fill = "transparent"),
#       strip.text = element_text(size = 14),
#       text = element_text(family = "Inconsolata")
#     )
# )
# 
# if (!file.exists(here::here(figs, "AR11_2017_Agonist_negative_AR14_2020_Agonist_positive_radial_plots_gt_0_21.png"))) {
#   ## Save above radial plot
#   ggsave(
#     rad_plot_ar17_ag_neg_ar20_ag_pos_gt_0.21,
#     filename = "AR11_2017_Agonist_negative_AR14_2020_Agonist_positive_radial_plots_gt_0_21.png",
#     path = here(figs),
#     device = "png",
#     height = 25,
#     width = 25,
#     units = "cm"
#   )
# }
# 
# ## Combine radial plots for chemicals where 2017 AR model has -ve antagonist prediction, and
# ## 2020 AR model has +ve antagonist prediction
# (ar17_ag_neg_ar20_ag_pos_combine_plot <- rad_plot_ar17_ag_neg_ar20_ag_pos_0.21 + rad_plot_ar17_ag_neg_ar20_ag_pos_gt_0.21 +
#   # Left plot will be 4x as wide as right plot
#   plot_layout(widths = c(4, 1), guides = "collect") +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     panel.background = element_rect(fill = "#ffffff"),
#     plot.background = element_rect(fill = "#ffffff"),
#     strip.background = element_rect(fill = "#ffffff")
#   )
# )
# 
# if (!file.exists(here::here(figs, "AR11_2017_Agonist_negative_AR14_2020_Agonist_positive_radial_plots_combined.png"))) {
#   ## Save combined radial plots
#   ggsave(
#     ar17_ag_neg_ar20_ag_pos_combine_plot,
#     filename = "AR11_2017_Agonist_negative_AR14_2020_Agonist_positive_radial_plots_combined.png",
#     path = here(figs),
#     device = "png",
#     height = 30,
#     width = 40,
#     units = "cm"
#   )
# }


# 2017 model +ve and 2020 model -ve ---------------------------     ---------------------------     ---------------------------


# 2017 model Antagonist +ve and 14-assay 2020 model Antagonist -ve ---------------------------     ---------------------------

## Keep only chemicals where 2017 model has antagonist +ve prediction & 14-assay 2020 model has antagonist -ve prediction
ar17_antag_pos_ar20_antag_neg <- ar_model_info %>% 
  filter(antagonist_pred_ar11_2017 == 1 & antagonist_pred_ar14 == 0)

## Convert to long form to make it easier for plotting
ar17_antag_pos_ar20_antag_neg_long <- ar17_antag_pos_ar20_antag_neg %>%
  pivot_longer(
    cols = contains("auc"),
    names_to = c("prediction", "source"),
    names_pattern = "auc_(agonist|antagonist)_(.*)", #Matches in 1st () will go to "prediction" col, matches in 2nd () will go to "source" col
    values_to = "auc"
  ) %>%
  relocate(c(prediction, source, auc), .after = casrn) %>%
  # Create col used for labelling in radial plots
  tidyr::unite("source2", c(source, prediction), remove = FALSE) %>%
  # Convert to factor and order so agonist & antagonist predictions are together
  mutate(
    source2 = factor(source2,
                     levels = c(
                       "ar11_2017_agonist", "ar11sub_2020_agonist", "ar14_agonist",
                       "ar11_2017_antagonist", "ar11sub_2020_antagonist", "ar14_antagonist"
                     )
    ),
    name_wrapped = str_wrap(name, width = 20, whitespace_only = FALSE), # Wraps chemical name at closest non-letter character after 20 characters - Needed for facet labelling
    auc_gt_0_21 = if_else(auc >= auc_limits, round(auc, digits = 2), NA_integer_) # Keep only AUC values beyond limits of radial plots
  ) 

## Radial plot for chemicals where 2017 AR model has +ve antagonist prediction, and
## 14-assay 2020 AR model has -ve antagonist prediction
##  - Limiting y-axis to 0.21
(rad_plot_ar17_antag_pos_ar20_antag_neg_0.21 <- ar17_antag_pos_ar20_antag_neg_long %>% 
    group_by(dtxsid) %>%
    ggplot(aes(x = source2, y = auc, fill = source)) +
    geom_col(position = "dodge") +
    # Add horizontal line at median subset_AUC
    geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
    # Add vertical line between agonist and antagonist models
    geom_vline(xintercept = c(0.5, 3.5), colour = "#222222") +
    geom_text(aes(label = auc_gt_0_21), size = 10/.pt, family = "Inconsolata", nudge_y = -0.04) + # Add label for AUC beyond limits
    facet_wrap(facets = ~ fct_reorder(name_wrapped, auc)) + # Order facets by increasing AUC
    coord_curvedpolar() +
    scale_x_discrete(
      labels = c(
        "ar11_2017_agonist" = "AR11 2017\nAgonist",
        "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
        "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
        "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
        "ar14_agonist" = "AR14 2020\nAgonist",
        "ar14_antagonist" = "AR14 2020\nAntagonist"
      )
    ) +
    scale_y_continuous(
      name = "Model AUC",
      limits = c(0, auc_limits),
      breaks = scales::pretty_breaks(n = 5),
      oob = squish
    ) +
    scale_fill_manual(
      name = "Source",
      values = c(
        "ar11_2017" = "#bf4321",
        "ar11sub_2020" = "#b6c568",
        "ar14" = "#92ccd8"
      ),
      labels = c(
        "ar11_2017" = "AR11 2017",
        "ar11sub_2020" = "AR11 2020",
        "ar14" = "AR14 2020"
      ),
      aesthetics = c("fill")
    ) +
    ggthemes::theme_fivethirtyeight() +
    theme(
      axis.text.x = element_text(vjust = 0.6),
      legend.background = element_blank(),
      legend.key = element_blank(),
      # legend.justification = c(1, 1),
      # legend.position = c(0.75, 0.2),
      legend.text = element_text(size = 14), # Reduce size of legend text
      legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "#ffffff"),
      plot.background = element_rect(fill = "#ffffff"),
      strip.background = element_rect(fill = "#ffffff"),
      strip.text = element_text(size = 12),
      text = element_text(family = "Inconsolata")
    )
)

if (!file.exists(here::here(figs, "AR_2017_Antagonist_positive_14_assay_2020_Antagonist_negative_radial_plots_0_21.png"))) {
  ## Save above radial plot
  ggsave(
    rad_plot_ar17_antag_pos_ar20_antag_neg_0.21,
    filename = "AR_2017_Antagonist_positive_14_assay_2020_Antagonist_negative_radial_plots_0_21.png",
    path = here(figs),
    device = "png",
    height = 30,
    width = 30,
    units = "cm"
  )
}


# 2017 model Agonist +ve and 14-assay 2020 model Aagonist -ve ---------------------------     ---------------------------

## Keep only chemicals where 2017 model has agonist +ve prediction & 14-assay 2020 model has agonist -ve prediction
ar17_ag_pos_ar20_ag_neg <- ar_model_info %>% 
  filter(agonist_pred_ar11_2017 == 1 & agonist_pred_ar14 == 0)

## Convert to long form to make it easier for plotting
ar17_ag_pos_ar20_ag_neg_long <- ar17_ag_pos_ar20_ag_neg %>%
  pivot_longer(
    cols = contains("auc"),
    names_to = c("prediction", "source"),
    names_pattern = "auc_(agonist|antagonist)_(.*)", #Matches in 1st () will go to "prediction" col, matches in 2nd () will go to "source" col
    values_to = "auc"
  ) %>%
  relocate(c(prediction, source, auc), .after = casrn) %>%
  # Create col used for labelling in radial plots
  tidyr::unite("source2", c(source, prediction), remove = FALSE) %>%
  # Convert to factor and order so agonist & antagonist predictions are together
  mutate(
    source2 = factor(source2,
                     levels = c(
                       "ar11_2017_agonist", "ar11sub_2020_agonist", "ar14_agonist",
                       "ar11_2017_antagonist", "ar11sub_2020_antagonist", "ar14_antagonist"
                     )
    ),
    name_wrapped = str_wrap(name, width = 20, whitespace_only = FALSE), # Wraps chemical name at closest non-letter character after 20 characters - Needed for facet labelling
    auc_gt_0_21 = if_else(auc >= auc_limits, round(auc, digits = 2), NA_integer_) # Keep only AUC values beyond limits of radial plots
  ) 

## Radial plot for chemicals where 2017 AR model has +ve agonist prediction, and
## 14-assay 2020 AR model has -ve agonist prediction
##  - Limiting y-axis to 0.21
(rad_plot_ar17_ag_pos_ar20_ag_neg_0.21 <- ar17_ag_pos_ar20_ag_neg_long %>% 
    group_by(dtxsid) %>%
    ggplot(aes(x = source2, y = auc, fill = source)) +
    geom_col(position = "dodge") +
    # Add horizontal line at median subset_AUC
    geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
    # Add vertical line between agonist and antagonist models
    geom_vline(xintercept = c(0.5, 3.5), colour = "#222222") +
    geom_text(aes(label = auc_gt_0_21), size = 10/.pt, family = "Inconsolata", nudge_y = -0.04) + # Add label for AUC beyond limits
    facet_wrap(facets = ~ fct_reorder(name_wrapped, auc)) + # Order facets by increasing AUC
    coord_curvedpolar() +
    scale_x_discrete(
      labels = c(
        "ar11_2017_agonist" = "AR11 2017\nAgonist",
        "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
        "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
        "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
        "ar14_agonist" = "AR14 2020\nAgonist",
        "ar14_antagonist" = "AR14 2020\nAntagonist"
      )
    ) +
    scale_y_continuous(
      name = "Model AUC",
      limits = c(0, auc_limits),
      breaks = scales::pretty_breaks(n = 5),
      oob = squish
    ) +
    scale_fill_manual(
      name = "Source",
      values = c(
        "ar11_2017" = "#bf4321",
        "ar11sub_2020" = "#b6c568",
        "ar14" = "#92ccd8"
      ),
      labels = c(
        "ar11_2017" = "AR11 2017",
        "ar11sub_2020" = "AR11 2020",
        "ar14" = "AR14 2020"
      ),
      aesthetics = c("fill")
    ) +
    ggthemes::theme_fivethirtyeight() +
    theme(
      axis.text.x = element_text(vjust = 0.6),
      legend.background = element_blank(),
      legend.key = element_blank(),
      # legend.justification = c(1, 1),
      # legend.position = c(0.9, 0.2),
      legend.text = element_text(size = 14), # Reduce size of legend text
      legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "#ffffff"),
      plot.background = element_rect(fill = "#ffffff"),
      strip.background = element_rect(fill = "#ffffff"),
      strip.text = element_text(size = 12),
      text = element_text(family = "Inconsolata")
    )
)

if (!file.exists(here::here(figs, "AR_2017_Agonist_positive_14_assay_2020_Agonist_negative_radial_plots_0_21.png"))) {
  ## Save above radial plot
  ggsave(
    rad_plot_ar17_ag_pos_ar20_ag_neg_0.21,
    filename = "AR_2017_Agonist_positive_14_assay_2020_Agonist_negative_radial_plots_0_21.png",
    path = here(figs),
    device = "png",
    height = 30,
    width = 30,
    units = "cm"
  )
}

## Combine radial plots for chemicals where 2017 AR model has -ve antagonist prediction, and
## 2020 AR model has +ve antagonist prediction
(ar17_pos_ar20_neg_combine_plot <- 
    rad_plot_ar17_antag_pos_ar20_antag_neg_0.21 / rad_plot_ar17_ag_pos_ar20_ag_neg_0.21 +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(
      legend.position = "bottom"
    )
)

if (!file.exists(here::here(figs, "AR_2017_positive_14_assay_2020_negative_radial_plots_combined.png"))) {
  ## Save combined radial plots
  ggsave(
    ar17_pos_ar20_neg_combine_plot,
    filename = "AR_2017_positive_14_assay_2020_negative_radial_plots_combined.png",
    path = here(figs),
    device = "png",
    height = 60,
    width = 30,
    units = "cm"
  )
}
## Radial plot for chemicals where 2017 AR model has +ve agonist prediction, and
## 14-assay 2020 AR model has -ve agonist prediction
##  - Only chemicals with 1+ AUC >0.21
# (rad_plot_ar17_ag_pos_ar20_ag_neg_gt_0.21 <- ar17_ag_pos_ar20_ag_neg_long %>% 
#     group_by(dtxsid) %>% 
#     filter(any(auc > 0.21)) %>% 
#     ggplot(aes(x = source2, y = auc, fill = source)) +
#     geom_col(position = "dodge") +
#     # Add horizontal line at median subset_AUC
#     geom_hline(yintercept = 0.1, linetype = 2, colour = "#222222") +
#     facet_wrap(facets = ~fct_reorder(dtxsid, auc)) + # Order facets by increasing AUC
#     coord_curvedpolar() +
#     scale_x_discrete(
#       labels = c(
#         "ar11_2017_agonist" = "AR11 2017\nAgonist",
#         "ar11_2017_antagonist" = "AR11 2017\nAntagonist",
#         "ar11sub_2020_agonist" = "AR11 2020\nAgonist",
#         "ar11sub_2020_antagonist" = "AR11 2020\nAntagonist",
#         "ar14_agonist" = "AR14 2020\nAgonist",
#         "ar14_antagonist" = "AR14 2020\nAntagonist"
#       )
#     ) +
#     scale_y_continuous(
#       name = "Model AUC",
#       limits = c(0, 1),
#       breaks = scales::pretty_breaks(n = 5),
#       oob = squish
#     ) +
#     scale_fill_manual(
#       name = "Source",
#       values = c(
#         "ar11_2017" = "#bf4321",
#         "ar11sub_2020" = "#b6c568",
#         "ar14" = "#92ccd8"
#       ),
#       labels = c(
#         "ar11_2017" = "AR11 2017",
#         "ar11sub_2020" = "AR11 2020",
#         "ar14" = "AR14 2020"
#       ),
#       aesthetics = c("fill")
#     ) +
#     ggthemes::theme_fivethirtyeight() +
#     theme(
#       axis.text.x = element_text(vjust = 0.6),
#       legend.background = element_blank(),
#       legend.key = element_blank(),
#       #legend.justification = c(1, 1),
#       #legend.position = c(0.9, 0.2),
#       legend.text = element_text(size = 10), # Reduce size of legend text
#       legend.key.size = unit(0.75, "lines"), # Reduce distance of lines in legend
#       legend.direction = "horizontal",
#       # panel.background = element_rect(fill = "transparent"),
#       # plot.background = element_rect(fill = "transparent"),
#       strip.text = element_text(size = 14),
#       text = element_text(family = "Inconsolata")
#     )
# )
# 
# if (!file.exists(here::here(figs, "AR_2017_Agonist_positive_14_assay_2020_Agonist_negative_radial_plots_gt_0_21.png"))) {
#   ## Save above radial plot
#   ggsave(
#     rad_plot_ar17_ag_pos_ar20_ag_neg_gt_0.21,
#     filename = "AR_2017_Agonist_positive_14_assay_2020_Agonist_negative_radial_plots_gt_0_21.png",
#     path = here(figs),
#     device = "png",
#     height = 25,
#     width = 25,
#     units = "cm"
#   )
# }


## Combine radial plots for chemicals where 2017 AR model has -ve antagonist prediction, and
## 2020 AR model has +ve antagonist prediction
# (ar17_pos_ar20_neg_combine_plot <- 
#     (rad_plot_ar17_antag_pos_ar20_antag_neg_0.21 | rad_plot_ar17_ag_pos_ar20_ag_neg_0.21) / 
#     (plot_spacer() | rad_plot_ar17_ag_pos_ar20_ag_neg_gt_0.21) +
#     # Left plot will be 2x as wide as right plot
#     plot_layout(heights = c(1.5, 1), guides = "collect") +
#     plot_annotation(tag_levels = "A") &
#     theme(
#       legend.position = "bottom",
#       legend.direction = "horizontal",
#       panel.background = element_rect(fill = "#ffffff"),
#       plot.background = element_rect(fill = "#ffffff"),
#       strip.background = element_rect(fill = "#ffffff")
#     )
# )
# 
# if (!file.exists(here::here(figs, "AR_2017_positive_14_assay_2020_negative_radial_plots_combined.png"))) {
#   ## Save combined radial plots
#   ggsave(
#     ar17_pos_ar20_neg_combine_plot,
#     filename = "AR_2017_positive_14_assay_2020_negative_radial_plots_combined.png",
#     path = here(figs),
#     device = "png",
#     height = 40,
#     width = 55,
#     units = "cm"
#   )
# }
