# =============================================================================
# make_ms_figures.R
#
# Creates all figures used in the manuscript and SI, and collects them in
# results/figures/manuscript_b0/ with names after their figure number.
#
# Run from code/figure_code/ (the same working directory as the figure
# scripts), e.g. in RStudio: Session > Set Working Directory > To Source File
# Location, then source("make_ms_figures.R").
#
# The figure scripts only read the simulation results (.rds) in results/,
# so the simulations (code/run_all_simulations.R) do not need to be re-run.
# Each script also produces other figures in its own folder under
# results/figures/; those are not part of the manuscript.
# =============================================================================

source("../settings.R") # model settings and results folder

# Run one figure script in its own environment, so that settings such as
# magnitude_threats in one script cannot leak into the next
# Input:
# - script: file name of the figure script (in code/figure_code/)
# Output:
# - none; the script saves its figures under results/figures/
run <- function(script) {
  message("\n==== Running ", script, " ====")
  source(script, local = new.env(), chdir = TRUE)
}

# -----------------------------------------------------------------------------
# 1. Run the figure scripts
# -----------------------------------------------------------------------------

# The output folders are not in git (empty folders can't be), and ggsave()
# does not create them, so create them first.
out_folders <- c("diff_subwebs", "sensitivity", "for_ms/var", "for_ms_SI/var",
                 "main_both_param_comb/var", "network_effects",
                 "species_props", "wiggle_comparison/var", "manuscript")
for (d in file.path("../../results/figures", out_folders)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Writes fw_props.csv, which stats_ESS-S_L_C.R reads (no manuscript figure)
run("plot_ESS_FW_props.R")

# Fig. 1
run("plot_ESS_both_param_comb.R")

# Fig. 2, Fig. 3
run("all_ms_figs.R")

# Fig. 4, Fig. S3, Fig. S4
run("plot_ESS_species_props.R")

# Fig. 5, Fig. S13 (direct vs total loss)
run("plot_direct_vs_total.R")

# Fig. 6, Fig. S5-S7
run("network_effects_main_figures.R")

# Fig. S2
run("plot_magnitude_sensitivity.R")

# Table S1
run("stats_ESS-S_L_C.R")

# Fig. S12 (treatment of Unknown threat scores)
run("plot_sensitivity_variants.R")

# Fig. S8-S11
run("plot_magnitude_sensitivity_for_nw-effects.R")

# -----------------------------------------------------------------------------
# 2. Collect the manuscript figures in one folder
# -----------------------------------------------------------------------------
# Left: name in results/figures/manuscript_b0/
# Right: file produced by the scripts above (relative to results/figures/)

fig_dir <- "../../results/figures/"
out_dir <- file.path(fig_dir, manuscript_folder)

ms_figures <- c(
  # Main text
  "Fig1_ESS_retention.pdf"                      = "main_both_param_comb/var/1.0_var_ESS_changes-all_webs.pdf",
  "Fig2_subregions.pdf"                         = "for_ms/fig2_spread-var-1-1-essparams-1-1.pdf",
  "Fig3_threat_magnitude.pdf"                   = "for_ms/fig3_magnitude_prop_with_oil.pdf",
  "Fig4_species_roles.pdf"                      = "species_props/species_roles_overlap-mean_all_webs.pdf",
  "Fig5_direct_vs_total_loss.pdf"               = "network_effects/direct_vs_total_loss_main.pdf",
  "Fig6_network_effects_Food_provisioning.pdf"  = "network_effects/nw_diff_stacked_Food provisioning.pdf",

  # SI (Fig. S1, the threat scoring table, is not made in R)
  "FigS2_sensitivity_threat_magnitude.pdf"      = "wiggle_comparison/var/var_ESS_wiggle_summary_all_webs.pdf",
  "TableS1_foodweb_properties.pdf"              = "diff_subwebs/table_stats_ESS-S-C-L.pdf",
  "FigS3_TL_service_providers.pdf"              = "species_props/ess_TL.pdf",
  "FigS4_TL_threatened_species.pdf"             = "species_props/threat_TL.pdf",
  "FigS5_network_effects_Recreation_and_tourism.pdf" = "network_effects/nw_diff_stacked_Recreation and tourism.pdf",
  "FigS6_network_effects_Carbon_sequestration.pdf" = "network_effects/nw_diff_stacked_Carbon sequestration.pdf",
  "FigS7_network_effects_Habitat_provisioning.pdf" = "network_effects/nw_diff_stacked_Habitat provisioning.pdf",
  "FigS8_network_effects_perturbed_Recreation_and_tourism.pdf" = "network_effects/var_nw_diff_wiggle_Recreation and tourism.pdf",
  "FigS9_network_effects_perturbed_Food_provisioning.pdf"      = "network_effects/var_nw_diff_wiggle_Food provisioning.pdf",
  "FigS10_network_effects_perturbed_Carbon_sequestration.pdf"  = "network_effects/var_nw_diff_wiggle_Carbon sequestration.pdf",
  "FigS11_network_effects_perturbed_Habitat_provisioning.pdf"  = "network_effects/var_nw_diff_wiggle_Habitat provisioning.pdf",
  "FigS12_sensitivity_unknown_scores.pdf"       = "sensitivity/sensitivity_unknown_linear.pdf",
  "FigS13_direct_vs_total_loss.pdf"             = "network_effects/direct_vs_total_loss_SI.pdf"
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

sources <- file.path(fig_dir, ms_figures)
missing <- ms_figures[!file.exists(sources)]
if (length(missing) > 0) {
  stop("These figures were not created:\n  ", paste(missing, collapse = "\n  "))
}

file.copy(sources, file.path(out_dir, names(ms_figures)), overwrite = TRUE)
message("\nDone: ", length(ms_figures), " figures copied to ", normalizePath(out_dir))
