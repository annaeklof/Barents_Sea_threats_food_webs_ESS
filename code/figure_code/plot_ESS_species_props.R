require(tidyverse)
require(dplyr)
library(readxl)
require(forcats)
require(fs)
require(ggplot2)
require(viridis)
library(RColorBrewer)


# Plot ecosystem services
# Input:
# - ESS: table of ecosystem services (compiled by get_ESS.R)
# - w: the (sub)web to for which to plot the ESS (without path or extension)
# - tf: file with threat information (without path or extension)
# - a: alpha parameter for Bayesian network
# - b: beta parameter for Bayesian network
# - ESS_sum: method of summation of ESS loss ("ind" or "tot")
# Output:
# - A figure with projected loss of ecosystem services for each threat

plot_TL <- function(FW_sp, cols, col_order, nrow = 2, dir = "h") {
  breaks <- seq(0.75, 6.25, by = 0.5)
  mids   <- head(breaks, -1) + 0.25

  d <- FW_sp |>
    pivot_longer(all_of(cols), names_to = "group") |>
    filter(value == 1) |>
    distinct(group, taxon, .keep_all = TRUE) |>
    mutate(bin = cut(TL, breaks = breaks, labels = FALSE)) |>
    count(group, bin) |>
    group_by(group) |>
    mutate(prop  = n / sum(n),
           label = paste0(group, " (n = ", sum(n), ")")) |>
    ungroup() |>
    mutate(TL_mid = mids[bin],
           group  = factor(group, levels = col_order)) |>
    arrange(group) |>
    mutate(label = factor(label, levels = unique(label)))

  ggplot(d, aes(x = TL_mid, y = prop)) +
    geom_col(width = 0.5, colour = "darkblue", fill = "darkblue", alpha = 0.2) +
    scale_x_continuous(name = "Trophic level", breaks = 1:6, limits = c(0.75, 6.25)) +
    scale_y_continuous(name = "Proportion of species", labels = scales::label_percent()) +
    facet_wrap(~ label, nrow = nrow, dir = dir) +
    theme_bw() +
    theme(axis.text  = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 14),
          strip.text = element_text(size = 12))
}

#------------------------------------------------------
plot_overlap_species <- function(FW_sp, threats, ess) {
  new <- NULL
  for(t in threats){
    for(e in ess){
  sp_overlap <- FW_sp %>%
      select(web, taxon, t, e) %>%
    mutate(ess = e, threat = t) %>%
    unite("overlap", t:e, sep = "_") %>%
    mutate(overlap = case_when(overlap == '1_0' ~ 'threatened',
                               overlap == '0_1' ~ 'ESS provider',
                               overlap == '1_1' ~ 'both',
                               overlap == '0_0' ~ 'none'))

    new <- rbind(sp_overlap, new)
    }
  }
    OL<- new %>%
    #drop_na(overlap) %>%
    ggplot(aes(web, fill = overlap)) +
    geom_bar() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_x_discrete(labels=seq(1:33)) +
    labs(x="Web", y="Number of species") +
      facet_grid(ess  ~ threat) %>%
    return()

}

#------------------------------------------------------
plot_overlap_one_web <- function(FW_sp, threats, ess, w) {
  new <- NULL

  role_colors <- c(
    "Threatened"   = "#D55E00",
    "ESS provider" = "#009E73",
    "Both"         = "#7B3F99",
    "Neither"      = "#B0B0B0"
  )

  overlap_levels <- c("Neither", "Threatened", "ESS provider", "Both")

  for(t in threats){
    for(e in ess){
      sp_overlap <- FW_sp %>%
        filter(web == w) %>%
        select(web, taxon, t, e) %>%
        mutate(ess = e, threat = t) %>%
        unite("overlap", t:e, sep = "_") %>%
        mutate(overlap = case_when(overlap == '1_0' ~ 'Threatened',
                                   overlap == '0_1' ~ 'ESS provider',
                                   overlap == '1_1' ~ 'Both',
                                   overlap == '0_0' ~ 'Neither'))

      new <- rbind(sp_overlap, new)
    }
  }

  new <- new %>%
    mutate(overlap = factor(overlap, levels = overlap_levels))

  ess_order <- c("Carbon sequestration",
                 "Habitat provisioning",
                 "Food provisioning",
                 "Recreation and tourism")

  level_order <- ess_order

  threat_order <- c("Pathogens",
                    "Seabed abrasion",
                    "Ice loss & inc. water temp.",
                    "Ocean acidification",
                    "Selective sp. extraction",
                    "Pollution & marine litter",
                    "Habitat destruction",
                    "Oil pollution")

  OL <- new %>%
    mutate(threat = factor(threat, levels = threat_order)) %>%
    ggplot(aes(x = factor(ess, levels = level_order), fill = overlap)) +
    geom_bar() +
    scale_fill_manual(values = role_colors) +
    theme_bw() +
    theme(axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1, size = 12),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_text(size=12),
      legend.text = element_text(size=12)) +
    theme(strip.text = element_text(colour = 'black', size=13)) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
    labs(x="Ecosystem service", y="Number of species",  fill = "Species role(s)", size = 15) +
    facet_wrap(~ threat, nrow = 2)

  return(list(plot = OL, data = new))
}
#------------------------------------------------
source("../settings.R") # model version and results folder
magnitude_threats <- "var" # set to "equal" or "var"

#ESS <- read_rds(paste0("../../results/", results_folder,"/ESS_changes_",magnitude_threats,".rds")) # Main result file
ESS <-
  read_rds(paste0("../../results/", results_folder,
                  "/ESS_changes_", magnitude_threats, ".rds")) |>
  mutate(ESS_param = case_when(
    ESS_alpha == 1 & ESS_beta == 1 ~ "ESS: proportional",
    ESS_alpha == 1 & ESS_beta == 2 ~ "ESS: robust",
    ESS_alpha == 1 & ESS_beta == 5 ~ "ESS: highly robust",
    ESS_alpha == 2 & ESS_beta == 1 ~ "ESS: vulnerable",
    ESS_alpha == 5 & ESS_beta == 1 ~ "ESS: highly vulnerable",
    TRUE                           ~ ""
  )) |>
  mutate(ab_param = case_when(
    alpha == 1 & beta == 1 ~ "Consumers: linear",
    alpha == 1 & beta == 2 ~ "Consumers: sensitive",
    alpha == 1 & beta == 5 ~ "Consumers: highly sensitive",
    alpha == 2 & beta == 1 ~ "Consumers: resilient",
    alpha == 5 & beta == 1 ~ "Consumers: highly resilient",
    TRUE                   ~ ""
  )) |>
  unite("param_comb", c(ab_param, ESS_param), sep = "   -   ") |>
  filter(param_comb %in% c("Consumers: linear   -   ESS: robust",
                           "Consumers: linear   -   ESS: vulnerable",
                           "Consumers: resilient   -   ESS: proportional",
                           "Consumers: sensitive   -   ESS: proportional",
                           "Consumers: linear   -   ESS: proportional")) |>
  mutate(param_comb = fct_relevel(param_comb,
                                  "Consumers: linear   -   ESS: robust",
                                  "Consumers: resilient   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: proportional",
                                  "Consumers: sensitive   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: vulnerable")) |>
  # Human-readable ESS and threat names:
  mutate(ESS = case_match(
    ESS,
    "CO2_SEQ"  ~ "Carbon sequestration",
    "FOOD_PRO" ~ "Food provisioning",
    "HAB_PRO"  ~ "Habitat provisioning",
    "REC_TOU"  ~ "Recreation and tourism",
    .default   = ""
  )) |>
  mutate(threat = case_match(
    threat,
    "PATH"    ~ "Pathogens",
    "SEA_ABR" ~ "Seabed abrasion",
    "ICE_LOS" ~ "Ice loss & inc. water temp.",
    "OCE_ACI" ~ "Ocean acidification",
    "SEL_EXT" ~ "Selective sp. extraction",
    "POL"     ~ "Pollution & marine litter",
    "HAB_DES" ~ "Habitat destruction",
    "OIL_POL" ~ "Oil pollution",
    .default  = ""
  )) |>
  mutate(threat = fct_reorder(threat, desc(service_total))) |>
  mutate(ESS = fct_reorder(ESS, desc(service_total)))

# Create table with all results from ESS losses and species properties

webs <- list.files("../../data/sub_networks_full/")
# Extract web & add web properties
webs <- map_chr(webs, ~ str_split(path_file(.x), pattern = ".txt")[[1]][1])
# needed for general properties
threats <- read_threats("../../data/threats_ecosystem_services_by_species.xlsx") %>% # see settings.R
  affected() %>% # 1 = directly affected by the threat
  rename(
    "Oil pollution" = OIL_POL,
    "Habitat destruction" = HAB_DES,
    "Ice loss & inc. water temp." = ICE_LOS,
    "Ocean acidification" = OCE_ACI,
    "Pathogens" =  PATH,
    "Pollution & marine litter" =   POL,
    "Seabed abrasion" =  SEA_ABR,
    "Selective sp. extraction" = SEL_EXT)

threatlist <- threats

ESSlist  <- read_excel("../../data/threats_ecosystem_services_by_species.xlsx", sheet = "Ecosystem Services") %>%
  dplyr::select(c("ES", "Taxon")) %>%
  mutate("Connected" = 1) %>%
  pivot_wider(names_from = ES, values_from = Connected, values_fill = 0) %>%
  rename(taxon = Taxon)%>%
  rename(
    "Carbon sequestration" = CO2_SEQ,
    "Food provisioning" =  FOOD_PRO,
    "Habitat provisioning" = HAB_PRO,
    "Recreation and tourism" = REC_TOU)

sp_props <- tibble(read.csv2("../../data/species_props.csv", sep = ";")) %>%
  rename(taxon = TROPHOSPECIES) %>%
  mutate(TL = as.numeric(TL))

for (web in webs) { # For each web:
  TLS <- sp_props %>% filter(WEB == web) %>% select(taxon, TL, WEB)

  FW <- tibble(read.csv2(paste("../../data/sub_networks_full/",web,".txt", sep=""))) %>%
    mutate(subweb = web) %>%
    mutate(L = length(consumer))

  edges <- FW[,1:2]
  taxon <- unique(as.vector(as.matrix(edges)))


  FW <- FW %>%
    select(-c(consumer, resource)) %>%
    distinct() %>%
    mutate(S = length(taxon)) %>%
    mutate(C = L/S^2) %>%
    bind_cols(taxon) %>%
    rename(taxon = ...5) %>%

   inner_join(TLS, by = "taxon") %>%
    left_join(threatlist, by = "taxon") %>% # add the list of which species are affected by which threat
    left_join(ESSlist, by = "taxon") %>%
    mutate(across(all_of(names(ESSlist)[-1]), ~ replace_na(.x, 0)))


  if(web == webs[1]){
    FW_sp <- NULL
  }
  FW_sp <- rbind(FW, FW_sp)
}

FW_sp <- FW_sp %>%
  rename(web = subweb)
# choose which parameter combinations to plot
a <- 1
b <- 1
a_ess <- 1
b_ess <- 1

ess <- names(ESSlist[2:5])

threats <- names(threatlist[2:length(threatlist)])

# Panel order (also used further down; defined here so the script runs on its own)
ess_order <- c("Carbon sequestration", "Habitat provisioning",
               "Food provisioning", "Recreation and tourism")
threat_order <- c("Pathogens", "Seabed abrasion", "Ice loss & inc. water temp.",
                  "Ocean acidification", "Selective sp. extraction",
                  "Pollution & marine litter", "Habitat destruction", "Oil pollution")

fname <- paste0("../../results/figures/species_props/threat_TL-",
                a, "-", b, "-ESSsum-", a_ess, "-", b_ess,".pdf")
p_ess <- plot_TL(FW_sp, ess, ess_order, nrow = 2, dir = "v")
ggsave("../../results/figures/species_props/ess_TL.pdf",
       plot = p_ess, width = 9, height = 7)

p_threat <- plot_TL(FW_sp, threats, threat_order, nrow = 2)
ggsave("../../results/figures/species_props/threat_TL.pdf",
       plot = p_threat, width = 14, height = 7)


fname <- paste0("../../results/figures/species_props/species_roles_overlap-all_webs-",
                a, "-", b, "-ESSsum-", a_ess, "-", b_ess,".pdf")
plot_overlap_species(FW_sp, threats, ess) # Create figure
ggsave(fname, width = 18, height = 20) # Save plot

# for one web
fname <- paste0("../../results/figures/species_props/species_roles_one_web-",
                a, "-", b, "-ESSsum-", a_ess, "-", b_ess,".pdf")
w <- webs[2]

overlap_result <- plot_overlap_one_web(FW_sp, threats, ess, w)

overlap_result$plot   # the ggplot object
ggsave(fname, plot = overlap_result$plot, width = 10, height = 8)


# --- Overlap across all sub-webs ------------------------------------------

build_overlap_data <- function(FW_sp, threats, ess) {
  map(threats, \(t) map(ess, \(e)
                        FW_sp |>
                          select(web, taxon, all_of(c(t, e))) |>
                          mutate(across(all_of(c(t, e)), \(x) replace_na(x, 0)),
                                 threat = t, ess = e,
                                 overlap = case_when(.data[[t]] == 1 & .data[[e]] == 1 ~ "Both",
                                                     .data[[t]] == 1                   ~ "Threatened",
                                                     .data[[e]] == 1                   ~ "ESS provider",
                                                     TRUE                              ~ "Neither")) |>
                          select(web, taxon, threat, ess, overlap)
  ) |> list_rbind()) |> list_rbind()
}

overlap_all <- build_overlap_data(FW_sp, threats, ess)

# Proportion of species in each role, per web
overlap_frac <- overlap_all |>
  count(web, threat, ess, overlap) |>
  complete(nesting(web, threat, ess), overlap, fill = list(n = 0)) |>
  group_by(web, threat, ess) |>
  mutate(frac = n / sum(n)) |>
  ungroup()

# Main figure: mean proportions across sub-webs
role_colors <- c("Threatened" = "#D55E00", "ESS provider" = "#009E73",
                 "Both" = "#7B3F99", "Neither" = "#B0B0B0")
ess_order <- c("Carbon sequestration", "Habitat provisioning",
               "Food provisioning", "Recreation and tourism")
threat_order <- c("Pathogens", "Seabed abrasion", "Ice loss & inc. water temp.",
                  "Ocean acidification", "Selective sp. extraction",
                  "Pollution & marine litter", "Habitat destruction", "Oil pollution")

p_overlap <- overlap_frac |>
  group_by(threat, ess, overlap) |>
  summarise(mean_frac = mean(frac), .groups = "drop") |>
  mutate(overlap = factor(overlap, levels = c("Neither", "Threatened", "ESS provider", "Both")),
         ess     = factor(ess, levels = ess_order),
         threat  = factor(threat, levels = threat_order)) |>
  ggplot(aes(x = ess, y = mean_frac, fill = overlap)) +
  geom_col(alpha = 0.9, , colour = "grey50", linewidth = 0.2) +
  scale_fill_manual(values = role_colors) +
  scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~ threat, nrow = 2) +
  labs(x = "Ecosystem service", y = "Mean proportion of species",
       fill = "Species role(s)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title   = element_text(size = 14),
        legend.title   = element_text(size = 14),
        legend.position = "bottom",
        strip.text  = element_text(size = 13),
        legend.text = element_text(size = 12))

ggsave("../../results/figures/species_props/species_roles_overlap-mean_all_webs.pdf",
       plot = p_overlap, width = 10, height = 8)

# SI table: proportion of species with both roles, across sub-webs
fmt <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",")

overlap_role_table <- overlap_frac |>
  group_by(threat, ess, overlap) |>
  summarise(median_pct = 100 * median(frac),
            min_pct    = 100 * min(frac),
            max_pct    = 100 * max(frac),
            .groups = "drop") |>
  mutate(value   = paste0(fmt(median_pct), " (", fmt(min_pct), "-", fmt(max_pct), ")"),
         overlap = factor(overlap, levels = c("Threatened", "ESS provider", "Both", "Neither")),
         threat  = factor(threat, levels = threat_order),
         ess     = factor(ess, levels = ess_order)) |>
  select(threat, ess, overlap, value) |>
  arrange(overlap) |>
  pivot_wider(names_from = overlap, values_from = value) |>
  arrange(threat, ess) |>
  rename(Threat = threat, `Ecosystem service` = ess)

write.csv2(overlap_role_table,
           "../../results/figures/species_props/overlap_roles_all_webs.csv",
           row.names = FALSE)



