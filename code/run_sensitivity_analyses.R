# =============================================================================
# run_sensitivity_analyses.R
#
# Sensitivity analysis of the treatment of factors scored "Unknown" (see
# settings.R), run once at the unperturbed threat magnitudes:
# - unknown_level = 0 and 2: factors scored "Unknown" kept as scored in the
#   data (0) or given the middle level, instead of the lowest level (1, main
#   analysis)
#
# Each variant runs in its own R process, so the settings cannot leak between
# runs. Results go to results/var_b0_<variant>/. Run from code/. To run only
# one variant, set  variants  before sourcing this script.
# =============================================================================

# The variants: the setting to change for each run
if (!exists("variants")) {
  variants <- c(
    "unknown_level <- 0",
    "unknown_level <- 2"
  )
}

for (v in variants) {
  message("\n==== ", v, " ====")

  # R code for one run: set the variant, run the simulation, then move the
  # results from wiggle_magnitude/1.0/ to the top of the variant's folder
  commands <- c(
    v,
    "mag_change <- 1.0",
    "source('run_one_job.R')",
    "main_dir <- paste0('../results/', results_folder, '/')",
    "run_dir <- paste0(main_dir, 'wiggle_magnitude/1.0/')",
    "file.copy(list.files(run_dir, full.names = TRUE), main_dir, overwrite = TRUE)",
    "unlink(paste0(main_dir, 'wiggle_magnitude'), recursive = TRUE)"
  )

  # Run it in a new R process
  status <- system2("Rscript", c("-e", shQuote(paste(commands, collapse = "; "))))
  if (status != 0) {
    stop("Run failed: ", v)
  }
}

message("\nDone.")
