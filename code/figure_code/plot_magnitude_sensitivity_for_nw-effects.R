# Plotting figure for showing how sensitive the results on the network effects to changes in threat magnitudes.

library(tidyverse)
library(fs)
library(glue)
library(scales)

# Colours and parameter combinations (same as in the base network-effects script)
param_colors_all <- c(
  "Consumers: linear   -   ESS: robust"          = "#0072B2",
  "Consumers: linear   -   ESS: vulnerable"      = "#D55E00",
  "Consumers: linear   -   ESS: proportional"    = "#DDDDDD",
  "Consumers: resilient   -   ESS: proportional" = "#56B4E9",
  "Consumers: sensitive   -   ESS: proportional" = "#E69F00"
)

selected_combos <- c(
  "Consumers: linear   -   ESS: robust",
  "Consumers: resilient   -   ESS: proportional",
  "Consumers: linear   -   ESS: proportional",
  "Consumers: sensitive   -   ESS: proportional",
  "Consumers: linear   -   ESS: vulnerable"
)

source("../settings.R") # model version and results folder
magnitude_threats <- "var"
wiggle_keep <- c(0.8, 0.9, 1, 1.1, 1.2)

read_wiggle <- function(file_stub) {
  dirs <- dir_ls(glue("../../results/{results_folder}/wiggle_magnitude/"),
                 type = "directory") |> as.character()
  dirs |>
    set_names(basename(dirs)) |>
    map(\(d) read_rds(file.path(d, glue("{file_stub}_{magnitude_threats}.rds")))) |>
    list_rbind(names_to = "wiggle_magnitude") |>
    mutate(wiggle_magnitude = as.numeric(wiggle_magnitude)) |>
    filter(wiggle_magnitude %in% wiggle_keep)
}


threat_order <- c("Pathogens", "Seabed abrasion", "Ice loss & inc. water temp.",
                  "Ocean acidification", "Selective species extraction",
                  "Pollution & marine litter", "Habitat destruction", "Oil pollution")
ESS_order <- c("Carbon sequestration", "Habitat provisioning",
               "Food provisioning", "Recreation and tourism")

prep_ESS <- function(df) {
  df |>
    mutate(ESS_param = case_when(
      ESS_alpha == 1 & ESS_beta == 1 ~ "ESS: proportional",
      ESS_alpha == 1 & ESS_beta == 2 ~ "ESS: robust",
      ESS_alpha == 2 & ESS_beta == 1 ~ "ESS: vulnerable",
      TRUE ~ ""),
      ab_param = case_when(
        alpha == 1 & beta == 1 ~ "Consumers: linear",
        alpha == 1 & beta == 2 ~ "Consumers: sensitive",
        alpha == 2 & beta == 1 ~ "Consumers: resilient",
        TRUE ~ "")) |>
    unite("param_comb", c(ab_param, ESS_param), sep = "   -   ") |>
    filter(param_comb %in% selected_combos) |>
    mutate(param_comb = factor(param_comb, levels = selected_combos),
           ESS = case_match(ESS,
                            "CO2_SEQ"  ~ "Carbon sequestration",
                            "FOOD_PRO" ~ "Food provisioning",
                            "HAB_PRO"  ~ "Habitat provisioning",
                            "REC_TOU"  ~ "Recreation and tourism") |> factor(levels = ESS_order),
           threat = case_match(threat,
                               "HAB_DES" ~ "Habitat destruction",
                               "ICE_LOS" ~ "Ice loss & inc. water temp.",
                               "OCE_ACI" ~ "Ocean acidification",
                               "OIL_POL" ~ "Oil pollution",
                               "PATH"    ~ "Pathogens",
                               "POL"     ~ "Pollution & marine litter",
                               "SEA_ABR" ~ "Seabed abrasion",
                               "SEL_EXT" ~ "Selective species extraction") |> factor(levels = threat_order))
}

ESS_w        <- read_wiggle("ESS_changes")       |> prep_ESS()
ESS_no_net_w <- read_wiggle("ESS_changes_no_nw") |> prep_ESS() |>
  rename(service_total_no_net = service_total)

nw_summary <- ESS_w |>
  inner_join(ESS_no_net_w, by = intersect(names(ESS_w), names(ESS_no_net_w))) |>
  mutate(service_diff = pmax(service_total_no_net - service_total, 0)) |>
  group_by(wiggle_magnitude, ESS, threat, param_comb) |>
  summarise(median = median(service_diff),
            q25    = quantile(service_diff, 0.25),
            q75    = quantile(service_diff, 0.75),
            n      = n(),
            .groups = "drop")

line_cols <- param_colors_all[selected_combos]
line_cols["Consumers: linear   -   ESS: proportional"] <- "#888888"

plot_nw_wiggle <- function(df) {
  ggplot(df, aes(x = wiggle_magnitude, y = median,
                 colour = param_comb, fill = param_comb)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~ threat, nrow = 2) +
    scale_y_continuous(name = "Additional service loss due to\nspecies interactions",
                       labels = percent_format(), limits = c(0, 1)) +
    scale_x_continuous(name = "Scaling factor for extinction probabilities",
                       breaks = wiggle_keep) +
    scale_colour_manual(name = NULL, values = line_cols,
                        labels = str_wrap(selected_combos, 18)) +
    scale_fill_manual(name = NULL, values = line_cols,
                      labels = str_wrap(selected_combos, 18)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_bw(base_size = 12) +
    theme(strip.background = element_rect(fill = "grey95"),
          legend.position = "bottom")
}

walk(ESS_order, \(s) {
  p <- nw_summary |> filter(ESS == s) |> plot_nw_wiggle()
  ggsave(glue("../../results/figures/network_effects/{magnitude_threats}_nw_diff_wiggle_{s}.pdf"),
         plot = p, width = 12, height = 7)
})