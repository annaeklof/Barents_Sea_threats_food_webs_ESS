# Figure code updated for Barents Sea

require(tidyverse)
require(forcats)
require(fs)
require(ggplot2)
library(readxl)
library(pals)


# Plot ecosystem services
# Input:
# - ESS: table of ecosystem services (compiled by get_ESS.R)
# - w: the (sub)web to for which to plot the ESS (without path or extension)
# - a: alpha parameter for Bayesian network
# - b: beta parameter for Bayesian network
# - ESS_sum: method of summation of ESS loss ("ind" or "tot")
# Output:
# - A figure with projected loss of ecosystem services for each threat
plot_ESS_FW <- function(ESS_FW, a, b, a_ess, b_ess) {
  ESS_count <- ESS_FW %>% # for sorting based on average fraction of service providers for each service
    select(ESS_count, ESS, S, web) %>%
    distinct() %>%
    mutate(ESS_S = ESS_count/S) %>%
    group_by(ESS) %>%
    summarise(mean = mean(ESS_S), sd = sd(ESS_S)) %>%
    arrange(desc(mean))
  
 test <- ESS_FW %>%
    filter(alpha == a, beta == b, ESS_alpha == a_ess, ESS_beta ==  b_ess) %>% 
   filter(threat != "OIL_POL") %>% # Oil pollution is on a different scale so messes things up
   select(threat, web, service_total, ESS, S, L, C, ESS_count) %>%
   distinct() %>%
   right_join(ESS_count) %>%
   group_by(ESS) %>%
   arrange(mean) %>%
   mutate(ESS = case_match(
     ESS,
     "CO2_SEQ"  ~ "Carbon sequestration",
     "FOOD_PRO" ~ "Food provisioning",
     "HAB_PRO"  ~ "Habitat provisioning",
     "REC_TOU"  ~ "Recreation and tourism",
     .default   = ""
   ))

 
 # --- Plot 1: original boxplot ---
 p1 <- test %>%
   ggplot(aes(x = fct_reorder(threat, desc(mean)), y = service_total)) +
   geom_boxplot(coef = 1.5) +
   geom_point(
     aes(fill = web),
     position = position_jitter(width = 0.5, height = 0),
     shape = 21, color = "black", size = 2, alpha = 0.7,
     show.legend = FALSE
   ) +
   scale_fill_manual(values = unname(pals::polychrome(25))) +
   scale_y_continuous(name = "Fraction of service retained",
                      limits = NULL, labels = scales::label_percent()) +
   labs(x = "Threat", y = "Fraction of service retained") +
   facet_wrap(~ ESS, nrow = 2, scales = "free") +
   theme_bw() +
   scale_x_discrete(labels = c(
     "HAB_DES" = "Habitat des.",
     "ICE_LOS" = "Ice loss",
     "OCE_ACI" = "Ocean acid.",
     "OIL_POL" = "Oil poll.",
     "PATH"    = "Pathogenes",
     "POL"     = "Pollution",
     "SEA_ABR" = "Seabed abrasion",
     "SEL_EXT" = "Selective ext.")) +
   theme(
     axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
     axis.text.y  = element_text(size = 10),
     axis.title.x = element_text(size = 12),
     axis.title.y = element_text(size = 12),
     strip.text   = element_text(size = 12)
   )
 
 # --- Plot 2: rank consistency ---
 rank_data <- test %>%
   ungroup() %>%
   group_by(threat, web, ESS) %>%
   summarise(service_mean = mean(service_total), .groups = "drop") %>%
   group_by(threat, ESS) %>%
   mutate(rank = rank(service_mean)) %>%
   group_by(web, ESS) %>%
   summarise(mean_rank = mean(rank), sd_rank = sd(rank), .groups = "drop")
 
 p2 <- rank_data %>%
   ggplot(aes(x = mean_rank, y = fct_reorder(web, mean_rank))) +
   geom_pointrange(aes(xmin = mean_rank - sd_rank,
                       xmax = mean_rank + sd_rank,
                       color = sd_rank)) +
   scale_color_viridis_c(name = "Rank variability\n(SD)", direction = 1) +
   facet_wrap(~ ESS, nrow = 2) +
   labs(x = "Mean rank across threats",
        y = "Subweb") +
   theme_bw() +
   theme(
     axis.text  = element_text(size = 14),
     axis.title = element_text(size = 16),
     strip.text = element_text(size = 16))
 
 p3 <- test %>%
   ungroup() %>%
   group_by(web, ESS, L, S, C) %>%
   summarise(service_mean = mean(service_total), .groups = "drop") %>%
   pivot_longer(cols = c(L, S, C),
                names_to  = "network_property",
                values_to = "value") %>%
   ggplot(aes(x = value, y = service_mean)) +
   geom_point(aes(color = web), size = 3, alpha = 0.8, show.legend = FALSE) +
   geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.7) +
   scale_color_manual(values = unname(pals::polychrome(25))) +
   scale_y_continuous(labels = scales::label_percent()) +
   facet_grid(ESS ~ network_property, scales = "free") +
   labs(x = NULL, y = "Mean fraction of service retained\n(averaged across threats)") +
   theme_bw() +
   theme(strip.text   = element_text(size = 12),
         axis.text    = element_text(size = 11),
         axis.title   = element_text(size = 13))
 
 p4 <- test %>%
   ungroup() %>%
   group_by(web, ESS, L, S, C, ESS_count) %>%
   summarise(service_mean = mean(service_total), .groups = "drop") %>%
   mutate(ESS_fraction = ESS_count / S) %>%
   ggplot(aes(x = ESS_fraction, y = service_mean)) +
   geom_point(aes(color = web), size = 3, show.legend = FALSE) +
   geom_smooth(method = "lm", se = TRUE, color = "black") +
   scale_color_manual(values = unname(pals::polychrome(25))) +
   scale_y_continuous(labels = scales::label_percent()) +
   scale_x_continuous(labels = scales::label_percent()) +
   facet_wrap(~ ESS, scales = "free") +
   labs(x = "Fraction of species providing service (ESS_count / S)",
        y = "Mean fraction of service retained") +
   theme_bw()
 
 return(list(boxplot = p1, rankplot = p2, fw_props = p3, fracESSprov = p4))
}

#------------------------------------------------

source("../settings.R") # model version and results folder
magnitude_threats <- "var" # set to "equal" or "var"

ESS <-
  read_rds(paste0("../../results/", results_folder,
                  "/ESS_changes_", magnitude_threats, ".rds")) %>%
  mutate(threat = fct_reorder(threat, desc(service_total))) |>
  mutate(ESS = fct_reorder(ESS, desc(service_total)))


# Create table with all results from ESS losses and FW properties

webs <- list.files("../../data/sub_networks_full/")
# Extract web & add web properties 
webs <- map_chr(webs, ~ str_split(path_file(.x), pattern = ".txt")[[1]][1])
# needed for general properties

threats <- read_threats("../../data/threats_ecosystem_services_by_species.xlsx") # see settings.R

threatlist  <- affected(threats) # 1 = directly affected by the threat
  

ESSlist  <- read_excel("../../data/threats_ecosystem_services_by_species.xlsx", sheet = "Ecosystem Services") %>% 
  dplyr::select(c("ES", "Taxon")) %>% 
  mutate("Connected" = 1) %>% 
  pivot_wider(names_from = ES, values_from = Connected, values_fill = 0) %>% 
  rename(taxon = Taxon)


for (web in webs) { # For each web:
  FW <- tibble(read.csv2(paste("../../data/sub_networks_full/",web,".txt", sep=""))) %>%
    mutate(subweb = web) %>%
    mutate(L = length(consumer)) %>%
    pivot_longer(cols = c(consumer, resource), values_to = "taxon") %>%
    distinct(subweb, L, taxon) %>%
    mutate(S = n()) %>%
    ungroup() %>%
    mutate(C = L/S^2) %>%
    inner_join(threatlist, by = "taxon") %>% # add the list of which species are affected by which threat
    mutate(OIL_POL   = sum(OIL_POL)) %>%
    mutate(SEA_ABR = sum(SEA_ABR)) %>%
    mutate(HAB_DES = sum(HAB_DES)) %>%
    mutate(SEL_EXT = sum(SEL_EXT)) %>%
    mutate(POL = sum(POL)) %>%
    mutate(ICE_LOS = sum(ICE_LOS)) %>%
    mutate(OCE_ACI = sum(OCE_ACI)) %>%
    mutate(PATH = sum(PATH)) %>%
    inner_join(ESSlist, by = "taxon") %>%
    mutate(FOOD_PRO = sum(FOOD_PRO)) %>%
    mutate(REC_TOU = sum(REC_TOU)) %>%
    mutate(HAB_PRO = sum(HAB_PRO)) %>%
    mutate(CO2_SEQ = sum(CO2_SEQ)) %>%
    select(-c(taxon)) %>%
    distinct()
  if(web == webs[1]){
    FWs <- NULL
  }
  FWs <- rbind(FW, FWs)
}
FWs <- FWs %>%
  rename(web = subweb)
# Combine ESS table and FWs table

long_FW <- FWs %>%
  pivot_longer(!c(web, S, L, C, OIL_POL, SEA_ABR, HAB_DES, SEL_EXT, POL, ICE_LOS, OCE_ACI, PATH), names_to = "services", values_to = "ESS_count")
longer_FW <- long_FW %>%
  pivot_longer(!c(web, S, L, C, services, ESS_count), names_to = "threat", values_to = "threat_count") %>%
  rename(ESS = services)

write.csv2(longer_FW, "../../results/figures/diff_subwebs/fw_props.csv", row.names = FALSE)

ESS_FW <- ESS %>%
  inner_join(longer_FW, by = c("web", "threat", "ESS")) 

# Plot from main df ESS_FW

a <- 1
b <- 1
a_ess <- 1
b_ess <- 1

magnitude_threats <- "var"

plots <- plot_ESS_FW(ESS_FW, a, b, a_ess, b_ess)

fname_boxplot <- paste0("../../results/figures/diff_subwebs/freescale_ESS-FW-spread-",
                        magnitude_threats, "-", a, "-", b, "-essparams-", a_ess, "-", b_ess, ".pdf")

fname_rankplot <- paste0("../../results/figures/diff_subwebs/freescale_ESS-FW-rank-",
                         magnitude_threats, "-", a, "-", b, "-essparams-", a_ess, "-", b_ess, ".pdf")

fname_fwpropsplot <- paste0("../../results/figures/diff_subwebs/ESS_fw_props",
                            magnitude_threats, "-", a, "-", b, "-essparams-", a_ess, "-", b_ess, ".pdf")

fname_fracESSprov <- paste0("../../results/figures/diff_subwebs/frac_ESS_props",
                            magnitude_threats, "-", a, "-", b, "-essparams-", a_ess, "-", b_ess, ".pdf")

ggsave(fname_boxplot, plot = plots$boxplot, width = 12, height = 14)
ggsave(fname_rankplot, plot = plots$rankplot, width = 10, height = 12)
ggsave(fname_fwpropsplot, plot = plots$fw_props, width = 10, height = 12)
ggsave(fname_fracESSprov, plot = plots$fracESSprov, width = 10, height = 10)

  
  

