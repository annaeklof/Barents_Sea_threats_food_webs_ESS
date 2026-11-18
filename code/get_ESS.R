require(tidyverse)

ESS_analysis <- function(persistence, services) {
# Load table of marginal persistence probabilities
persistence <- persistence 

# Load table of ecosystem services (ESS)
services <- services 

# Table of parameters, used in beta function for summarizing the effects
# of species loss on ecosystem services
#ESS_params <- tibble(ESS_alpha = c(1), ESS_beta = c(1))
ESS_params <- tibble(ESS_alpha = c(1, 1, 1, 2, 5), ESS_beta = c(1, 2, 5, 1, 1))
# Table to store new (reduced) ecosystem service provision values in
ESS <- tibble()

# Looping through each combination of parameters in ESS summary function.
# We loop (instead of join), and gradually add rows to the table ESS, to
# reduce the otherwise excessive memory requirements.
for (r in 1:nrow(ESS_params)) { 
  # Join persistence and ecosystem service tables, by taxon
  ESS <- left_join(persistence, services, by = "taxon") %>% 
    # Normalize table for ESS; those come from column 8-11
    pivot_longer(cols = 8:11, names_to = "ESS", values_to = "provider") %>% 
    replace_na(list(provider = 0)) %>% 
    # Restrict table to only those taxa which actually provide the service
    filter(provider == 1) %>%
    group_by(threat, alpha, beta, nreps, web, ESS) %>%
    # mutate(test = mean(persist))    %>%
    # service_total: take average persistence, and apply a beta function
    summarise(service_total = pbeta(mean(persist), ESS_params$ESS_alpha[r],
                                    ESS_params$ESS_beta[r])) %>%
    ungroup() %>%
    # Add the two parameters of the ESS summary function as new columns
    mutate(ESS_alpha = ESS_params$ESS_alpha[r],
           ESS_beta = ESS_params$ESS_beta[r]) %>%
    # Attach result to master data frame
    bind_rows(ESS)
}
return(ESS)
}



# write_rds(ESS, outfile_ESS, compress = "xz")



