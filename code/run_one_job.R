# Run one job of threats-ESS analysis

# Parameter values

library(tidyverse)
library(readxl)

#=== Change parameters here ========================================
# Each setting can also be set before sourcing this script (see
# run_all_simulations.R); the values below are the defaults.

source("settings.R") # extinction probabilities, results folder

# Consumer response functions (alpha, beta of the beta distribution)
if (!exists("ab_params")) ab_params <- tibble(
  alpha = c(1, 1, 1, 2, 5),
  beta  = c(5, 2, 1, 1, 1)
)
if (!exists("nreps")) nreps <- 10000 # Number of iterations, set to 10.000 for real runs
magnitude_threats <- "var" # used in the result file names
# Sensitivity analysis: all extinction probabilities are multiplied by mag_change
if (!exists("mag_change")) mag_change <- 1.0

webs <- list.files("../data/sub_networks_full/")

out_dir <- paste0("../results/", results_folder, "/wiggle_magnitude/",
                  formatC(mag_change, format = "f", digits = 1), "/")

# Quick test: one subregion, linear consumers and 100 iterations, to check that
# the simulation runs (a few minutes). Results go to results/quick_test/ and
# are not used by the figures.
if (!exists("quick_test")) quick_test <- FALSE
if (quick_test) {
  webs <- webs[1]
  ab_params <- tibble(alpha = 1, beta = 1)
  nreps <- 100
  out_dir <- "../results/quick_test/"
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#============================================================

# Extinction probability per species and threat (defined in settings.R)
threats <- read_threats("../data/threats_ecosystem_services_by_species.xlsx")

threats <- threats %>%
  mutate(across(where(is.numeric), ~ . * mag_change))

services <- read_excel("../data/threats_ecosystem_services_by_species.xlsx", sheet = "Ecosystem Services") %>% 
  dplyr::select(c("ES", "Taxon")) %>% 
  mutate("Connected" = 1) %>% 
  pivot_wider(names_from = ES, values_from = Connected, values_fill = 0) %>% 
  rename(taxon = Taxon)

persistence <- tibble()

for(p in 1: length(ab_params$alpha)){
    alpha <- ab_params$alpha[p]
    beta <- ab_params$beta[p]
    for(web in webs){
  print(paste0("web:",web, ", alpha:", alpha, ", beta:", beta))

webname <- str_replace(web, ".txt", "") 

# Load data files
edges <- tibble(read.csv2(paste0("../data/sub_networks_full/",web))) %>% 
  dplyr::select(c("consumer", "resource"))

# Code for threat analyses ---
  # Functions for analyzing how different threats affect persistence of different species 

source("threat_functions.R") # functions for analyzing how different threats affect persistence of different species 
persistence_subnw <- threat_analysis(edges, threats, alpha, beta, nreps, webname) # used in get_ESS.R 
persistence <- bind_rows(persistence, persistence_subnw)
    }
  }

# ´persistence´gives the likelihoods for being extant for all species depending on threat  realized 
outfile_persistence <- paste0(out_dir, "persistence_results_", magnitude_threats,".rds")
write_rds(persistence, outfile_persistence, compress = "xz") 


# Code for ESS analyses ---
  # Functions for analyzing how the changes in species persistence affect ecosystem services. 
source("get_ESS.R")  
ESS <- ESS_analysis(persistence, services)
outfile_ESS <- paste0(out_dir, "ESS_changes_", magnitude_threats,".rds")
write_rds(ESS, outfile_ESS, compress = "xz")

# result files 
persistence <- read_rds(paste0(out_dir, "persistence_results_",magnitude_threats,".rds"))
ESS <- read_rds(paste0(out_dir, "ESS_changes_",magnitude_threats,".rds"))

# ----- Without species interactions -----
source("threat_functions_no_nw.R")
persistence_no_nw <- tibble() 

for(p in 1: length(ab_params$alpha)){
  alpha <- ab_params$alpha[p]
  beta <- ab_params$beta[p]
  for(web in webs){
    print(paste0("no nw, web:",web, ", alpha:", alpha, ", beta:", beta))
  edges <- tibble(read.csv2(paste0("../data/sub_networks_full/",web))) %>%  
    dplyr::select(c("consumer", "resource"))
       webname <- str_replace(web, ".txt", "") 
       persistence_no_nw_subnw <- threat_analysis_no_nw(edges, threats, alpha, beta, nreps, webname) 
       persistence_no_nw <- bind_rows(persistence_no_nw, persistence_no_nw_subnw)
    }
}
outfile_persistence_no_nw <- paste0(out_dir, "persistence_results_no_nw_",magnitude_threats,".rds")
write_rds(persistence_no_nw, outfile_persistence_no_nw, compress = "xz")

source("get_ESS_no_nw.R")  

ESS_no_nw <- ESS_analysis_no_nw(persistence_no_nw, services)
outfile_ESS <- paste0(out_dir, "ESS_changes_no_nw_",magnitude_threats,".rds")
write_rds(ESS_no_nw, outfile_ESS, compress = "xz")

ESS_no_nw <- read_rds(paste0(out_dir, "ESS_changes_no_nw_",magnitude_threats,".rds"))


