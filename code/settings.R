# =============================================================================
# settings.R: extinction probabilities and shared reading of the threat data
#
# Sourced by run_one_job.R and by the figure scripts, so that the extinction
# probabilities are defined in one place only.
#
# Threat scores in data/threats_ecosystem_services_by_species.xlsx (column
# Magnitude) are scope + severity + 1, i.e. 1-7, where "Unknown" counts as 0
# for a factor; column Unknown_factors gives the number of the two factors
# that are unknown (0-2). A species affected by a threat with score s gets
# extinction probability s/10; species not affected get 0. In the main
# analysis, each
# factor scored "Unknown" is given the lowest level (Minority / Minor), so
# Unknown + Unknown gives 0.3, the same as the weakest known assessment, and
# probabilities range 0.3-0.7.
#
# Sensitivity analysis (set before sourcing this file):
# unknown_level  Level given to each factor scored "Unknown": 1 = the lowest
#                level (Minority / Minor, main analysis), 0 = as scored in the
#                data (Unknown + Unknown gives 0.1), 2 = the middle level
#                (Majority / Moderate, Unknown + Unknown gives 0.5).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})



# Settings ---------------------------------------------------------------------

# Default value, used unless it is set before this file is sourced
if (!exists("unknown_level")) {
  unknown_level <- 1
}

# Extinction probability of species not affected by a threat
baseline <- 0


# Result folders ---------------------------------------------------------------

# Suffix for the sensitivity analyses, e.g. "_U0"
variant <- ""
if (unknown_level != 1) {
  variant <- paste0("_U", unknown_level)
}

# Where the results and manuscript figures are stored
results_folder    <- paste0("var_b0", variant)
manuscript_folder <- paste0("manuscript_b0", variant)
cont_folder       <- results_folder



# Functions --------------------------------------------------------------------

# Extinction probability caused by a threat
# Input:
# - s: threat score (scope + severity + 1)
# Output:
# - extinction probability
threat_prob <- function(s) {
  s / 10
}


# Extinction probability for every species and threat
# Ice loss and increased water temperature are merged (maximum), as are
# pollution and marine litter; vessel collisions and acoustic disturbance are
# not analysed.
# Input:
# - xlsx: path to threats_ecosystem_services_by_species.xlsx
# Output:
# - Table with one row per species (taxon) and one column per threat:
#   extinction probability caused by the threat
read_threats <- function(xlsx) {
  threats <- read_excel(xlsx, sheet = "Threats", .name_repair = "unique_quiet")

  # Give each factor scored "Unknown" a level (column Unknown_factors: number
  # of the two factors, scope and severity, that are unknown)
  threats <- threats %>%
    mutate(Magnitude = Magnitude + unknown_level * Unknown_factors)

  threats %>%
    dplyr::select(c("Taxon", "Threat", "Magnitude")) %>%
    mutate(Magnitude = threat_prob(Magnitude)) %>%
    pivot_wider(names_from = Threat, values_from = Magnitude, values_fill = 0) %>%
    mutate(across(where(is.numeric), ~ .x + baseline)) %>%
    rename(taxon = Taxon) %>%
    rowwise() %>%
    mutate(ICE_LOS = max(c_across(c(ICE_LOS, INC_WAT)))) %>%
    mutate(POL = max(c_across(c(MAR_LIT, POL)))) %>%
    select(-c(VES_COL, ACO_DIS, INC_WAT, MAR_LIT))
}


# Which species are directly affected by each threat
# Input:
# - threats: table from read_threats()
# Output:
# - Same table with 1 if the species is directly affected, 0 otherwise
affected <- function(threats) {
  threats %>%
    mutate(across(where(is.numeric), ~ as.numeric(.x > baseline)))
}
