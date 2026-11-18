# =============================================================================
# plot_direct_vs_total.R
#
# Direct versus total loss of each ecosystem service, per threat.
# x: loss without species interactions (direct effects only)
# y: loss with species interactions (direct + indirect)
# The vertical distance to the 1:1 line is the network effect. Points are
# medians over the 25 subregions, bars the interquartile range, for linear
# consumers and the proportional ESS function.
# Also prints the share of the loss caused by interactions, and the ranking of
# threats with and without interactions (numbers for the text).
#
# Run from code/figure_code/. Output: results/figures/network_effects/.
# =============================================================================

library(tidyverse)

# Results to use (folder under results/)
results_folder <- "var_b0"

# Consumer response and ESS functions (alpha, beta)
consumer_ab <- c(1, 1)   # linear
ess_ab      <- c(1, 1)   # proportional

# Where to save the figure
out_dir <- "../../results/figures/network_effects"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Columns that identify one result
key_cols <- c("threat", "alpha", "beta", "web", "ESS", "ESS_alpha", "ESS_beta")

# Threats, in the same order and colours as in Fig. 1 and Fig. 3
threat_labels <- c(
  PATH    = "Pathogens",
  SEA_ABR = "Seabed abrasion",
  ICE_LOS = "Ice loss & increased water temp.",
  OCE_ACI = "Ocean acidification",
  SEL_EXT = "Selective species extraction",
  POL     = "Pollution & marine litter",
  HAB_DES = "Habitat destruction",
  OIL_POL = "Oil pollution"
)
threat_colors <- c("#E69F00", "#56B4E9", "#009E73", "#8C8C00",
                   "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Services: low-trophic first, then high-trophic, as in Fig. 1
ess_labels <- c(
  CO2_SEQ  = "Carbon sequestration",
  HAB_PRO  = "Habitat provisioning",
  FOOD_PRO = "Food provisioning",
  REC_TOU  = "Recreation and tourism"
)



# Load service retained with and without interactions
# Input:
# - folder: folder under results/
# Output:
# - Table with S_nw (retained with interactions) and S_no (without) for every
#   threat, web and function combination
load_results <- function(folder) {
  with_nw <- readRDS(file.path("../../results", folder, "ESS_changes_var.rds")) %>%
    select(all_of(key_cols), S_nw = service_total)
  without_nw <- readRDS(file.path("../../results", folder, "ESS_changes_no_nw_var.rds")) %>%
    select(all_of(key_cols), S_no = service_total)

  inner_join(with_nw, without_nw, by = key_cols)
}



# Data --------------------------------------------------------------------------

losses <- load_results(results_folder) %>%
  filter(alpha == consumer_ab[1], beta == consumer_ab[2]) %>%
  filter(ESS_alpha == ess_ab[1], ESS_beta == ess_ab[2]) %>%
  filter(ESS %in% names(ess_labels)) %>%
  mutate(direct = 1 - S_no) %>%
  mutate(total = 1 - S_nw)

summary <- losses %>%
  group_by(ESS, threat) %>%
  summarise(
    direct_m  = median(direct),
    direct_lo = quantile(direct, 0.25),
    direct_hi = quantile(direct, 0.75),
    total_m   = median(total),
    total_lo  = quantile(total, 0.25),
    total_hi  = quantile(total, 0.75),
    .groups = "drop"
  ) %>%
  mutate(threat = factor(threat_labels[threat], levels = threat_labels)) %>%
  mutate(ESS = factor(ess_labels[ESS], levels = ess_labels))


# Figures -----------------------------------------------------------------------

# Plot direct vs total loss, with reference lines for 1, 2 and 4 times the
# direct loss
# Input:
# - summary: table with medians and quartiles per service and threat
# - services: services (labels) to show, one panel each
# - x_max, y_max: axis limits
# Output:
# - ggplot object
plot_losses <- function(summary, services, x_max, y_max) {
  df <- summary %>%
    filter(ESS %in% services)

  # Reference lines y = k * x, each labelled where it leaves the panel
  ref_lines <- tibble(k = 1)  %>%
    mutate(x_end = pmin(x_max, y_max / k)) %>%
    mutate(y_end = k * x_end)

  ggplot(df, aes(x = direct_m, y = total_m, colour = threat)) +
    geom_segment(data = ref_lines, aes(x = 0, y = 0, xend = x_end, yend = y_end),
                 inherit.aes = FALSE, linetype = "dashed", colour = "grey60") +
    geom_errorbar(aes(ymin = total_lo, ymax = total_hi), width = 0, alpha = 0.6) +
    geom_errorbarh(aes(xmin = direct_lo, xmax = direct_hi), height = 0, alpha = 0.6) +
    geom_point(size = 4) +
    facet_wrap(~ ESS, nrow = 1) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max), clip = "off") +
    scale_colour_manual(values = threat_colors, name = NULL, drop = FALSE) +
    scale_x_continuous(name = "Service lost without species interactions (direct effects)",
                       labels = scales::label_percent()) +
    scale_y_continuous(name = "Service lost with species interactions",
                       labels = scales::label_percent()) +
    guides(colour = guide_legend(nrow = 3)) +
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.spacing.x  = unit(9, "mm"),
      strip.background = element_rect(fill = "grey95"),
      plot.margin      = margin(5, 20, 5, 5)
    )
}

# Main text: services provided by high-trophic-level species
p_main <- plot_losses(summary, c("Food provisioning", "Recreation and tourism"),
                      x_max = 0.6, y_max = 1)
ggsave(file.path(out_dir, "direct_vs_total_loss_main.pdf"), plot = p_main, width = 8, height = 5.5)

# SI: services provided by low-trophic-level species
p_si <- plot_losses(summary, c("Carbon sequestration", "Habitat provisioning"),
                    x_max = 0.6, y_max = 1)
ggsave(file.path(out_dir, "direct_vs_total_loss_SI.pdf"), plot = p_si, width = 8, height = 5.5)


# Numbers for the text

cat("\nShare of the loss caused by interactions (median over subregions):\n")
losses %>%
  filter(total > 0.01) %>%
  mutate(share = (total - direct) / total) %>%
  group_by(ESS, threat) %>%
  summarise(share = round(median(share), 2), .groups = "drop") %>%
  pivot_wider(names_from = ESS, values_from = share) %>%
  print()

cat("\nRanking of threats, most damaging first, without and with interactions:\n")
summary %>%
  group_by(ESS) %>%
  mutate(rank_direct = rank(-direct_m)) %>%
  mutate(rank_total = rank(-total_m)) %>%
  arrange(ESS, rank_total) %>%
  select(ESS, threat, rank_direct, rank_total) %>%
  print(n = 40)
