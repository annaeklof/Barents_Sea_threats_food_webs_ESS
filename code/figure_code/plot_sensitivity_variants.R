# =============================================================================
# plot_sensitivity_variants.R
#
# Compares the treatment of threat scores marked "Unknown"
# (run_sensitivity_analyses.R) with the main analysis: service retained and
# network effect per threat and service. Main analysis: an unknown factor is
# given score 1 (Minority/Minor); alternatives: 0 (as scored) and 2 (the
# intermediate level).
# The figure for linear consumers is Fig. S12 in the SI.
# Background variants can be added to  variants  below; they are then measured
# against their no-threat reference.
# Run from code/figure_code/. Output: results/figures/sensitivity/.
# =============================================================================

library(tidyverse)
library(patchwork)

# Which results to compare: folder under results/ and label in the figure
variants <- tribble(
  ~folder,         ~label,
  "var_b0_U0",     "Unknown = 0",
  "var_b0",        "Unknown = 1 (main analysis)",
  "var_b0_U2",     "Unknown = 2"
)

# Where to save the figures
out_dir <- "../../results/figures/sensitivity"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Columns that identify one result
key_cols <- c("threat", "alpha", "beta", "web", "ESS", "ESS_alpha", "ESS_beta")

# Labels used in the figures
threat_labels <- c(
  PATH    = "Pathogens",
  SEA_ABR = "Seabed abrasion",
  ICE_LOS = "Ice loss & water temp.",
  SEL_EXT = "Selective extraction",
  POL     = "Pollution & litter",
  OCE_ACI = "Ocean acidification",
  HAB_DES = "Habitat destruction",
  OIL_POL = "Oil pollution"
)
ess_labels <- c(
  CO2_SEQ  = "Carbon sequestration",
  HAB_PRO  = "Habitat provisioning",
  FOOD_PRO = "Food provisioning",
  REC_TOU  = "Recreation and tourism"
)
variant_colors <- c("#56B4E9", "black", "#0072B2", "#E69F00", "#D55E00")



# Load the results of one variant, with and without species interactions
# Input:
# - folder: folder under results/ with the variant's results
# - label: name of the variant in the figures
# Output:
# - Table with service retained and network effect for every threat, web and
#   combination of consumer and ESS functions. For the background variants both
#   are measured relative to the no-threat reference (threat "NONE").
load_variant <- function(folder, label) {
  with_nw <- readRDS(file.path("../../results", folder, "ESS_changes_var.rds")) %>%
    select(all_of(key_cols), S_nw = service_total)
  without_nw <- readRDS(file.path("../../results", folder, "ESS_changes_no_nw_var.rds")) %>%
    select(all_of(key_cols), S_no = service_total)

  results <- inner_join(with_nw, without_nw, by = key_cols)

  if ("NONE" %in% results$threat) {
    # Background variants: compare with the no-threat reference
    reference <- results %>%
      filter(threat == "NONE") %>%
      select(-threat) %>%
      rename(R_nw = S_nw, R_no = S_no)
    results <- results %>%
      filter(threat != "NONE") %>%
      inner_join(reference, by = setdiff(key_cols, "threat")) %>%
      mutate(retained = S_nw / R_nw) %>%
      mutate(nw = (1 - S_nw / R_nw) - (1 - S_no / R_no))
  } else {
    results <- results %>%
      mutate(retained = S_nw) %>%
      mutate(nw = S_no - S_nw)
  }

  results %>%
    mutate(variant = label)
}


# Median and interquartile range over the subregions, for one consumer function
# and the proportional ESS function
# Input:
# - results: table from load_variant() for all variants
# - a, b: alpha and beta of the consumer response function
# Output:
# - Table with median (_m) and quartiles (_lo, _hi) of retained and nw
summarise_variants <- function(results, a, b) {
  results %>%
    filter(alpha == a, beta == b, ESS_alpha == 1, ESS_beta == 1) %>%
    mutate(threat = factor(threat_labels[threat], levels = threat_labels)) %>%
    mutate(ESS = factor(ess_labels[ESS], levels = ess_labels)) %>%
    group_by(variant, ESS, threat) %>%
    summarise(
      retained_m  = median(retained),
      retained_lo = quantile(retained, 0.25),
      retained_hi = quantile(retained, 0.75),
      nw_m  = median(nw),
      nw_lo = quantile(nw, 0.25),
      nw_hi = quantile(nw, 0.75),
      .groups = "drop"
    )
}


# Plot one measure for all variants
# Input:
# - summary: table from summarise_variants()
# - measure: "retained" or "nw"
# - y_label, title: axis label and panel title
# Output:
# - ggplot object
plot_variants <- function(summary, measure, y_label, title) {
  ggplot(summary, aes(x = threat, y = .data[[paste0(measure, "_m")]], colour = variant)) +
    geom_hline(yintercept = 0, colour = "grey85") +
    geom_pointrange(aes(ymin = .data[[paste0(measure, "_lo")]],
                        ymax = .data[[paste0(measure, "_hi")]]),
                    position = position_dodge(width = 0.7), size = 0.4) +
    facet_wrap(~ ESS, ncol = 2, dir = "v") +
    scale_colour_manual(values = variant_colors, name = NULL) +
    scale_y_continuous(name = y_label, labels = scales::label_percent()) +
    labs(x = NULL, title = title) +
    guides(colour = guide_legend(nrow = 1)) +
    theme_bw(base_size = 13) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title      = element_text(size = 13, face = "bold")
    )
}



# Load all variants ------------------------------------------------------------

results <- pmap_dfr(variants, load_variant) %>%
  mutate(variant = factor(variant, levels = variants$label))


# One figure per consumer function ---------------------------------------------

consumer_functions <- tribble(
  ~a, ~b, ~name,
  1,  1,  "linear",
  1,  2,  "sensitive"
)

for (i in 1:nrow(consumer_functions)) {
  a    <- consumer_functions$a[i]
  b    <- consumer_functions$b[i]
  name <- consumer_functions$name[i]

  summary <- summarise_variants(results, a, b)

  p_retained <- plot_variants(summary, "retained", "Service retained",
                              paste0("A. Service retained (", name,
                                     " consumers, proportional ESS)"))
  p_nw <- plot_variants(summary, "nw", "Extra loss with interactions",
                        "B. Network effect")

  ggsave(file.path(out_dir, paste0("sensitivity_unknown_", name, ".pdf")),
         plot = p_retained / p_nw, width = 10, height = 13)

  # Numbers for the text: median network effect (percentage points)
  cat("\n=== ", name, " consumers, proportional ESS: median network effect (pp) ===\n")
  summary %>%
    filter(ESS %in% c("Food provisioning", "Recreation and tourism")) %>%
    mutate(nw = round(100 * nw_m)) %>%
    select(variant, ESS, threat, nw) %>%
    pivot_wider(names_from = variant, values_from = nw) %>%
    print(n = 20, width = 200)

  # Numbers for the text: median service retained (%), Recreation and tourism
  cat("\nMedian service retained (%), Recreation and tourism:\n")
  summary %>%
    filter(ESS == "Recreation and tourism") %>%
    mutate(retained = round(100 * retained_m)) %>%
    select(variant, threat, retained) %>%
    pivot_wider(names_from = variant, values_from = retained) %>%
    print(width = 200)
}


# Rank correlation of threats with the main analysis (for the text) --------------

cat("\nSpearman correlation with the main analysis, medians per threat:\n")
results %>%
  filter(alpha == 1, beta %in% c(1, 2), ESS_alpha == 1, ESS_beta == 1) %>%
  group_by(variant, beta, ESS, threat) %>%
  summarise(retained = median(retained), nw = median(nw), .groups = "drop") %>%
  group_by(beta, ESS, threat) %>%
  mutate(main_retained = retained[variant == variants$label[2]]) %>%
  mutate(main_nw = nw[variant == variants$label[2]]) %>%
  group_by(variant, beta, ESS) %>%
  summarise(rho_retained = cor(retained, main_retained, method = "spearman"),
            rho_nw = cor(nw, main_nw, method = "spearman"), .groups = "drop") %>%
  filter(variant != variants$label[2]) %>%
  print(n = 20)


# Ranking of threats, least to most damaging -----------------------------------
# (linear consumers, proportional ESS, mean retention over the four services)

cat("\nThreat ranking, least to most damaging (linear-proportional):\n")
results %>%
  filter(alpha == 1, beta == 1, ESS_alpha == 1, ESS_beta == 1) %>%
  group_by(variant, threat) %>%
  summarise(mean_retained = mean(retained), .groups = "drop") %>%
  group_by(variant) %>%
  arrange(desc(mean_retained), .by_group = TRUE) %>%
  summarise(order = paste(threat_labels[threat], collapse = " > ")) %>%
  print(width = 300)
