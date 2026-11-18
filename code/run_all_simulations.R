# =============================================================================
# run_all_simulations.R
#
# Runs run_one_job.R for every level of the sensitivity analysis (all
# extinction probabilities multiplied by 0.8, 0.9, 1.0, 1.1 and 1.2) and copies
# the unperturbed results (1.0) to the top of the results folder, where the
# figure scripts read the main results.
#
# Run from code/. Takes several hours with nreps = 10000 (set in
# run_one_job.R).
# =============================================================================

source("settings.R") # results folder

# Levels of the sensitivity analysis
mag_changes <- c(0.8, 0.9, 1.0, 1.1, 1.2)

for (m in mag_changes) {
  message("\n==== mag_change = ", m, " ====")
  # Run each level in its own environment, so nothing is left over between runs
  env <- new.env()
  env$mag_change <- m
  sys.source("run_one_job.R", envir = env)
}

# Main results = the unperturbed run
main_dir <- paste0("../results/", results_folder, "/")
files <- list.files(paste0(main_dir, "wiggle_magnitude/1.0/"), full.names = TRUE)
file.copy(files, main_dir, overwrite = TRUE)

message("\nDone. Main results copied to ", main_dir)
