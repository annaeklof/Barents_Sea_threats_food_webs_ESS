library(dplyr)
library(ggplot2)
library(scales)
require(tidyverse)
require(fs)
library(glue)
library(stringr)


# function for plotting threats and parameters separately

plot_nw_diffs_joint <- function(ESS, ESS_no_net, fill_alpha = 0.7) {

  binwidth <- 0.1
  breaks   <- seq(0, 1, by = binwidth)
  nbins    <- length(breaks) - 1
  all_bins <- tibble(bin_id = 1:nbins)


  services <- unique(ESS$ESS)

  for (s in services) {
    plot_df <- ESS %>%
      full_join(ESS_no_net, by = intersect(names(ESS), names(ESS_no_net))) %>%
      filter(ESS == s) %>%
      mutate(service_diff = pmax(service_total_no_net - service_total, 0),
             bin_id = cut(service_diff, breaks = breaks, include.lowest = TRUE,
                          right = FALSE, labels = FALSE)) %>%
      count(threat, param_comb, bin_id, name = "n") %>%
      group_by(threat, param_comb) %>%
      complete(all_bins, fill = list(n = 0)) %>%
      ungroup() %>%
      mutate(bin_mid = (breaks[bin_id] + breaks[bin_id + 1]) / 2)

    # Share of the analyses of each threat (25 subregions x 5 combinations of
    # response functions = 125)
    plot_df <- plot_df %>%
      group_by(threat) %>%
      mutate(value = n / sum(n)) %>%
      ungroup()

    # The same y-axis, 0-100 %, in every panel and figure
    y_scale <- scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                                  expand = expansion(mult = c(0, .02)))

    wrapped_labels <- plot_df$param_comb %>% str_replace("   -   ", "\n") %>% unique()

    p <- ggplot(plot_df, aes(x = bin_mid, y = value, fill = param_comb)) +
      geom_col(width = binwidth, position = "stack",
               alpha = fill_alpha, colour = "grey50") +
      facet_wrap(~ threat, nrow = 2) +
      labs(x = "Additional service loss due to species interactions",
           y = "Share of analyses", fill = NULL) +
      scale_x_continuous(labels = percent_format(), limits = c(0, 1),
                         breaks = seq(0, 1, by = 0.25)) +
      y_scale +
      scale_fill_manual(
        name = NULL,
        values = c("Consumers: linear   -   ESS: robust"          = "#0072B2",
                   "Consumers: linear   -   ESS: vulnerable"      = "#D55E00",
                   "Consumers: linear   -   ESS: proportional"    = "#DDDDDD",
                   "Consumers: resilient   -   ESS: proportional" = "#56B4E9",
                   "Consumers: sensitive   -   ESS: proportional" = "#E69F00"),
        breaks = levels(plot_df$param_comb),
        labels = wrapped_labels
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      theme_bw(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "grey95"),
        axis.text.x = element_text(hjust = 1, vjust = 1),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.4), "cm"),
        legend.position = "bottom",
        legend.text.align = 0,
        legend.key.height = unit(0.5, "lines")
      )

    ggsave(glue("../../results/figures/network_effects/nw_diff_stacked_{s}.pdf"),
           plot = p, width = 8, height = 5.33)
  }
}

# modify data
source("../settings.R") # model version and results folder
magnitude_threats <- "var" # set to "equal" or "var"
webs <- list.files("../../data/sub_networks_full/")
webs <- map_chr(webs, ~ str_split(path_file(.x), pattern = ".txt")[[1]][1])

# main result file
ESS <-
  read_rds(paste0("../../results/", results_folder,
                  "/ESS_changes_", magnitude_threats, ".rds")) |>
  mutate(ESS_param = case_when(
    ESS_alpha == 1 & ESS_beta == 1 ~ "ESS: proportional",
    ESS_alpha == 1 & ESS_beta == 2 ~ "ESS: robust",
    ESS_alpha == 1 & ESS_beta == 5 ~ "ESS: highly robust",
    ESS_alpha == 2 & ESS_beta == 1 ~ "ESS: vulnerable",
    ESS_alpha == 5 & ESS_beta == 1 ~ "ESS: highly vulnerable",
    TRUE                           ~ ""
  )) |>
  mutate(ab_param = case_when(
    alpha == 1 & beta == 1 ~ "Consumers: linear",
    alpha == 1 & beta == 2 ~ "Consumers: sensitive",
    alpha == 1 & beta == 5 ~ "Consumers: highly sensitive",
    alpha == 2 & beta == 1 ~ "Consumers: resilient",
    alpha == 5 & beta == 1 ~ "Consumers: highly resilient",
    TRUE                   ~ ""
  )) |>
  unite("param_comb", c(ab_param, ESS_param), sep = "   -   ") |>
  filter(param_comb %in% c("Consumers: linear   -   ESS: robust",
                           "Consumers: linear   -   ESS: vulnerable",
                           "Consumers: resilient   -   ESS: proportional",
                           "Consumers: sensitive   -   ESS: proportional",
                           "Consumers: linear   -   ESS: proportional")) |>
  mutate(param_comb = fct_relevel(param_comb,
                                  "Consumers: linear   -   ESS: robust",
                                  "Consumers: resilient   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: proportional",
                                  "Consumers: sensitive   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: vulnerable")) |>
  # Human-readable ESS and threat names:
  mutate(ESS = case_match(
    ESS,
    "CO2_SEQ"  ~ "Carbon sequestration",
    "FOOD_PRO" ~ "Food provisioning",
    "HAB_PRO"  ~ "Habitat provisioning",
    "REC_TOU"  ~ "Recreation and tourism",
    .default   = ""
  )) |>
  mutate(threat = case_match(
    threat,
    "HAB_DES" ~ "Habitat destruction",
    "ICE_LOS" ~ "Ice loss & inc. water temp.",
    "OCE_ACI" ~ "Ocean acidification",
    "OIL_POL" ~ "Oil pollution",
    "PATH"    ~ "Pathogens",
    "POL"     ~ "Pollution & marine litter",
    "SEA_ABR" ~ "Seabed abrasion",
    "SEL_EXT" ~ "Selective species extraction",
    .default  = ""
  )) |>
  mutate(threat = fct_reorder(threat, desc(service_total))) |>
  mutate(ESS = fct_reorder(ESS, desc(service_total)))


ESS_no_net <-
  read_rds(paste0("../../results/", results_folder, "/ESS_changes_no_nw_",
                  magnitude_threats,".rds"))|>
  mutate(ESS_param = case_when(
    ESS_alpha == 1 & ESS_beta == 1 ~ "ESS: proportional",
    ESS_alpha == 1 & ESS_beta == 2 ~ "ESS: robust",
    ESS_alpha == 1 & ESS_beta == 5 ~ "ESS: highly robust",
    ESS_alpha == 2 & ESS_beta == 1 ~ "ESS: vulnerable",
    ESS_alpha == 5 & ESS_beta == 1 ~ "ESS: highly vulnerable",
    TRUE                           ~ ""
  )) |>
  mutate(ab_param = case_when(
    alpha == 1 & beta == 1 ~ "Consumers: linear",
    alpha == 1 & beta == 2 ~ "Consumers: sensitive",
    alpha == 1 & beta == 5 ~ "Consumers: highly sensitive",
    alpha == 2 & beta == 1 ~ "Consumers: resilient",
    alpha == 5 & beta == 1 ~ "Consumers: highly resilient",
    TRUE                   ~ ""
  )) |>
  unite("param_comb", c(ab_param, ESS_param), sep = "   -   ") |>
  filter(param_comb %in% c("Consumers: linear   -   ESS: robust",
                           "Consumers: linear   -   ESS: vulnerable",
                           "Consumers: resilient   -   ESS: proportional",
                           "Consumers: sensitive   -   ESS: proportional",
                           "Consumers: linear   -   ESS: proportional")) |>
  mutate(param_comb = fct_relevel(param_comb,
                                  "Consumers: linear   -   ESS: robust",
                                  "Consumers: resilient   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: proportional",
                                  "Consumers: sensitive   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: vulnerable")) |>
  # Human-readable ESS and threat names:
  mutate(ESS = case_match(
    ESS,
    "CO2_SEQ"  ~ "Carbon sequestration",
    "FOOD_PRO" ~ "Food provisioning",
    "HAB_PRO"  ~ "Habitat provisioning",
    "REC_TOU"  ~ "Recreation and tourism",
    .default   = ""
  )) |>
  mutate(threat = case_match(
    threat,
    "HAB_DES" ~ "Habitat destruction",
    "ICE_LOS" ~ "Ice loss & inc. water temp.",
    "OCE_ACI" ~ "Ocean acidification",
    "OIL_POL" ~ "Oil pollution",
    "PATH"    ~ "Pathogens",
    "POL"     ~ "Pollution & marine litter",
    "SEA_ABR" ~ "Seabed abrasion",
    "SEL_EXT" ~ "Selective species extraction",
    .default  = ""
  )) |>
  mutate(threat = fct_reorder(threat, desc(service_total))) |>
  mutate(ESS = fct_reorder(ESS, desc(service_total))) |>
  rename(service_total_no_net = service_total)

plot_nw_diffs_joint(ESS, ESS_no_net)

# write file for max and min differences
ESS_nw_diff  <- ESS %>% full_join(ESS_no_net) %>%
  filter(ESS_alpha == 1, ESS_beta == 1) %>%
  mutate(diff = 1-(service_total/service_total_no_net)) %>%
  group_by(ESS, threat) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE), sd_diff = sd(diff, na.rm = TRUE),
            max_diff = max(diff, na.rm = TRUE), min_diff = min(diff, na.rm = TRUE) )

write.csv2(ESS_nw_diff, paste0("../../results/", results_folder, "/ESS_network_diffs.csv"))
