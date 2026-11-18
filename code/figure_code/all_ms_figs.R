# ============================================================
# Barents Sea manuscript — consolidated figure script
# Produces: Fig 1 (+SI), Fig 2, Fig 3, Fig 5 (+SI)
# ============================================================

library(tidyverse)
library(forcats)
library(fs)
library(ggplot2)
library(readxl)
library(pals)
library(scales)

source("../settings.R") # model version and results folder
magnitude_threats <- "var"

# ============================================================
# 0. SHARED PALETTE / LABELS / ORDERS  (palette.R content)
# ============================================================

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#8C8C00",
               "#0072B2", "#D55E00", "#CC79A7", "#999999")

fig1_threat_order <- c("Pathogens", "Seabed abrasion", "Ice loss & increased water temp.",
                       "Ocean acidification", "Selective species extraction",
                       "Pollution & marine litter", "Habitat destruction", "Oil pollution")

fig5_threat_order <- c("PATH", "SEA_ABR", "ICE_LOS", "OCE_ACI",
                       "SEL_EXT", "POL", "HAB_DES", "OIL_POL")

ess_order <- c("Carbon sequestration", "Habitat provisioning",
               "Food provisioning", "Recreation and tourism")

threat_colors_full   <- setNames(okabe_ito, fig1_threat_order)
threat_colors_abbrev <- setNames(okabe_ito, fig5_threat_order)

role_colors <- c(
  "Threatened"   = "#D55E00",
  "ESS provider" = "#009E73",
  "Both"         = "#CC79A7",
  "Neither"      = "#999999"
)

sensitivity_colors <- c(
  "Consumers: resilient   -   ESS: proportional" = "#0072B2",
  "Consumers: linear   -   ESS: proportional"    = "#999999",
  "Consumers: sensitive   -   ESS: proportional" = "#D55E00"
)

threat_lookup_labels <- c(
  "HAB_DES" = "Habitat destruction", "ICE_LOS" = "Ice loss & inc. water temp.",
  "OCE_ACI" = "Ocean acidification", "OIL_POL" = "Oil pollution",
  "PATH"    = "Pathogens", "POL" = "Pollution & marine litter",
  "SEA_ABR" = "Seabed abrasion", "SEL_EXT" = "Selective sp. extraction"
)
threat_lookup_labels <- threat_lookup_labels[fig5_threat_order]

ess_labels <- c("CO2_SEQ" = "Carbon sequestration", "FOOD_PRO" = "Food provisioning",
                "HAB_PRO" = "Habitat provisioning", "REC_TOU" = "Recreation and tourism")

fig1_threat_labels_x <- c(
  "Habitat destruction" = "Habitat des.", "Ice loss & increased water temp." = "Ice loss",
  "Ocean acidification" = "Ocean acid.", "Oil pollution" = "Oil poll.",
  "Pathogens" = "Pathogens", "Pollution & marine litter" = "Pollution",
  "Seabed abrasion" = "Seabed abrasion", "Selective species extraction" = "Selective ext."
)

# ============================================================
# 1. FIGURE 1 — Service retained by threat, colored by consumer sensitivity
#    (main text: linear ESS only; SI: robust/vulnerable ESS)
# ============================================================

plot_ESS <- function(ESS, param_combs_to_show, fill_values) {
  ESS %>%
    filter(param_comb %in% param_combs_to_show) %>%
    ggplot(aes(x = threat, y = service_total, fill = param_comb)) +
    geom_point(size = 3.5, alpha = 0.8, shape = 21, color = "black") +
    scale_y_continuous(name = "Service retained (%)", limits = c(0, 1),
                       breaks = c(0, 0.5, 1), labels = scales::label_percent()) +
    scale_x_discrete(name = NULL, labels = fig1_threat_labels_x) +
    scale_fill_manual(name = NULL, values = fill_values) +
    facet_wrap(~ ESS, ncol = 2, scales = "fixed", dir = "v") +
    guides(fill = guide_legend(ncol = 1)) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
      axis.text.y  = element_text(size = 12),
      axis.title.y = element_text(size = 14),
      strip.text   = element_text(size = 13),
      legend.text  = element_text(size = 12),
      plot.margin  = unit(c(0.1, 0.1, 0.1, 0.4), "cm"),
      legend.position = "bottom"
    )
}

ESS <-
  read_rds(paste0("../../results/", results_folder,
                  "/wiggle_magnitude/1.0/ESS_changes_", magnitude_threats, ".rds")) |>
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
    "HAB_DES" ~ "Habitat destruction",
    "ICE_LOS" ~ "Ice loss & increased water temp.",
    "OCE_ACI" ~ "Ocean acidification",
    "OIL_POL" ~ "Oil pollution",
    "PATH"    ~ "Pathogens",
    "POL"     ~ "Pollution & marine litter",
    "SEA_ABR" ~ "Seabed abrasion",
    "SEL_EXT" ~ "Selective species extraction",
    .default  = ""
  )) |>
  mutate(threat = factor(threat, levels = fig1_threat_order)) |>
  mutate(ESS = factor(ESS, levels = ess_order))

# --- MAIN TEXT: linear ESS, 3 consumer sensitivity levels ---
main_combs <- c("Consumers: resilient   -   ESS: proportional",
                "Consumers: linear   -   ESS: proportional",
                "Consumers: sensitive   -   ESS: proportional")

# --- SI: robust / vulnerable ESS (linear consumer response held fixed for clarity) ---
si_combs <- c("Consumers: linear   -   ESS: robust",
              "Consumers: linear   -   ESS: proportional",
              "Consumers: linear   -   ESS: vulnerable")

si_fill_values <- c(
  "Consumers: linear   -   ESS: robust"       = "#0072B2",
  "Consumers: linear   -   ESS: proportional" = "#999999",
  "Consumers: linear   -   ESS: vulnerable"   = "#D55E00"
)

list.files("../../data/sub_networks_full/") |>
  str_remove(".txt") |>
  walk(\(w) {
    ess_w <- ESS |> filter(web == w)

    # main text version
    fname_main <- paste0("../../results/figures/for_ms/", magnitude_threats,
                         "/", magnitude_threats, "_ESS_changes-", w, ".pdf")
    p_main <- plot_ESS(ess_w, main_combs, sensitivity_colors)
    ggsave(fname_main, plot = p_main, width = 8, height = 10)

    # SI version (ESS function sensitivity)
    fname_si <- paste0("../../results/figures/for_ms_SI/", magnitude_threats,
                       "/", magnitude_threats, "_ESS_changes_SI_essfunc-", w, ".pdf")
    p_si <- plot_ESS(ess_w, si_combs, si_fill_values)
    ggsave(fname_si, plot = p_si, width = 8, height = 10)
  })

# ============================================================
# 2. FIGURE 2 — Service retention spread across subregions, by threat
# ============================================================


# --- 2a. Build food-web properties + threat/service counts per subweb ---

threats_wide <- read_threats("../../data/threats_ecosystem_services_by_species.xlsx") # see settings.R

threatlist <- affected(threats_wide) # 1 = directly affected by the threat

ESSlist <- read_excel("../../data/threats_ecosystem_services_by_species.xlsx", sheet = "Ecosystem Services") %>%
  dplyr::select(c("ES", "Taxon")) %>%
  mutate("Connected" = 1) %>%
  pivot_wider(names_from = ES, values_from = Connected, values_fill = 0) %>%
  rename(taxon = Taxon)

webs <- list.files("../../data/sub_networks_full/") %>%
  map_chr(~ str_split(path_file(.x), pattern = ".txt")[[1]][1])

FWs <- NULL
for (web in webs) {
  FW <- tibble(read.csv2(paste("../../data/sub_networks_full/", web, ".txt", sep = ""))) %>%
    mutate(subweb = web) %>%
    mutate(L = length(consumer)) %>%
    pivot_longer(cols = c(consumer, resource), values_to = "taxon") %>%
    distinct(subweb, L, taxon) %>%
    mutate(S = n()) %>%
    ungroup() %>%
    mutate(C = L / S^2) %>%
    inner_join(threatlist, by = "taxon") %>%
    mutate(OIL_POL = sum(OIL_POL)) %>%
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
  FWs <- rbind(FW, FWs)
}
FWs <- FWs %>% rename(web = subweb)

long_FW <- FWs %>%
  pivot_longer(!c(web, S, L, C, OIL_POL, SEA_ABR, HAB_DES, SEL_EXT, POL, ICE_LOS, OCE_ACI, PATH),
               names_to = "services", values_to = "ESS_count")

longer_FW <- long_FW %>%
  pivot_longer(!c(web, S, L, C, services, ESS_count),
               names_to = "threat", values_to = "threat_count") %>%
  rename(ESS = services)

#dir_create("../../results/figures/diff_subwebs")
#write.csv2(longer_FW, "../../results/figures/diff_subwebs/fw_props.csv", row.names = FALSE)

# --- 2b. Combine with ESS results ---

ESS_FW <- ESS %>%
  mutate(threat_abbrev = case_match(
    as.character(threat),
    "Habitat destruction"                ~ "HAB_DES",
    "Ice loss & increased water temp."   ~ "ICE_LOS",
    "Ocean acidification"                ~ "OCE_ACI",
    "Oil pollution"                      ~ "OIL_POL",
    "Pathogens"                          ~ "PATH",
    "Pollution & marine litter"          ~ "POL",
    "Seabed abrasion"                    ~ "SEA_ABR",
    "Selective species extraction"       ~ "SEL_EXT",
    .default = as.character(threat)
  )) %>%
  mutate(ESS_abbrev = case_match(
    as.character(ESS),
    "Carbon sequestration"    ~ "CO2_SEQ",
    "Food provisioning"       ~ "FOOD_PRO",
    "Habitat provisioning"    ~ "HAB_PRO",
    "Recreation and tourism"  ~ "REC_TOU",
    .default = as.character(ESS)
  )) %>%
  inner_join(longer_FW, by = c("web", "threat_abbrev" = "threat", "ESS_abbrev" = "ESS"))

# --- 2c. Plot ---

plot_fig2 <- function(ESS_FW, a, b, a_ess, b_ess) {
  test <- ESS_FW %>%
    filter(alpha == a, beta == b, ESS_alpha == a_ess, ESS_beta == b_ess) %>%
    filter(threat != "Oil pollution") %>%
    select(threat, web, service_total, ESS) %>%
    distinct() %>%
    group_by(threat, ESS) %>%
    mutate(mean = mean(service_total)) %>%
    ungroup() %>%
    mutate(ESS = factor(ESS, levels = ess_order))

  test %>%
    ggplot(aes(x = fct_reorder(threat, desc(mean)), y = service_total)) +
    geom_boxplot(coef = 10, outlier.shape = NA, color = "#0072B2",
                 fill = "#0072B2", alpha = 0.2) +
    geom_point(
      position = position_jitter(width = 0.2, height = 0, seed = 54321),
      shape = 21, fill = "#0072B2", color = "#0072B2", size = 2, alpha = 0.4
    ) +
    scale_y_continuous(name = "Fraction of service retained",
                       limits = NULL, labels = scales::label_percent()) +
    labs(x = "Threat") +
    facet_wrap(~ ESS, nrow = 2, scales = "free", dir = "v") +
    theme_bw(base_size = 14) +
    scale_x_discrete(labels = fig1_threat_labels_x) +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 9))
}

p_fig2 <- plot_fig2(ESS_FW, a = 1, b = 1, a_ess = 1, b_ess = 1)
fname_fig2 <- paste0("../../results/figures/for_ms/fig2_spread-",
                     magnitude_threats, "-1-1-essparams-1-1.pdf")
ggsave(fname_fig2, plot = p_fig2, width = 8, height = 7.5)

# ============================================================
# 3. FIGURE 3 — Threat magnitude vs. proportion of species affected
# ============================================================

threats_raw <- read_threats("../../data/threats_ecosystem_services_by_species.xlsx") %>% # see settings.R
  rename(
    "Habitat destruction"          = HAB_DES,
    "Ice loss & increased water temp." = ICE_LOS,
    "Ocean acidification"          = OCE_ACI,
    "Oil pollution"                = OIL_POL,
    "Pathogens"                    = PATH,
    "Pollution & marine litter"    = POL,
    "Seabed abrasion"              = SEA_ABR,
    "Selective species extraction" = SEL_EXT
  ) %>%
  pivot_longer(!c(taxon), names_to = "threat", values_to = "threat_magnitude")

sp_props <- tibble(read.csv2("../../data/species_props.csv", sep = ";")) %>%
  rename(taxon = TROPHOSPECIES) %>%
  mutate(TL = as.numeric(TL)) %>%
  select(!c(Group, ABBREVIATION, Turnover))

fw_props_s <- longer_FW %>%
  select(web, S) %>%
  rename(WEB = web) %>%
  distinct()

magnitudes <- threats_raw %>%
  full_join(sp_props, relationship = "many-to-many") %>%
  filter(threat_magnitude > baseline) %>% # directly affected species only
  mutate(TL_round = round(TL))

pdat <- magnitudes %>%
  select(threat, threat_magnitude, WEB) %>%
  #filter(threat != "Oil pollution") %>%
  filter(WEB != "Metaweb") %>%
  group_by(WEB, threat) %>%
  mutate(N = n()) %>%
  inner_join(fw_props_s, by = "WEB") %>%
  mutate(
    prop     = N / S,
    mean_mag = mean(threat_magnitude, na.rm = TRUE)
  ) %>%
  select(!threat_magnitude) %>%
  distinct() %>%
  ungroup() %>%
  mutate(threat = factor(threat, levels = fig1_threat_order))

ell_ok <- pdat %>%
  group_by(threat) %>%
  # an ellipse needs variation in both directions (ocean acidification has the
  # same mean magnitude, 0.5, in every subregion)
  filter(n() >= 3, sd(prop, na.rm = TRUE) > 0, sd(mean_mag, na.rm = TRUE) > 1e-9) %>%
  ungroup()

p_fig3 <- ggplot(pdat, aes(x = prop, y = mean_mag)) +
  geom_point(aes(colour = threat), size = 2) +
  scale_color_manual(values = threat_colors_full, name = "Threat") +
  stat_ellipse(
    data = ell_ok,
    aes(fill = threat, group = threat),
    geom = "polygon", alpha = 0.3, colour = NA
  ) +
  scale_fill_manual(values = threat_colors_full, guide = "none") +
  theme_bw() +
  scale_x_continuous(name = "Proportion of species affected") +
  scale_y_continuous(name = "Magnitude of threat") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    axis.text.x = element_text(vjust = 0.9, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12)
  )


ggsave("../../results/figures/for_ms/fig3_magnitude_prop_with_oil.pdf",
       plot = p_fig3, width = 8, height = 6)

# ============================================================
# 4. FIGURE 4 — Species role overlap (Threatened / Provider / Both / Neither)
# ============================================================

# Uses FW_sp, built the same way as your original Fig 4 pipeline
# (species x subweb, with threat/service 0/1 indicator columns, left_join + NA-fill applied)

# --- 4a. Build FW_sp (species properties + threat/service indicators per subweb) ---

TLS_all <- sp_props %>% select(taxon, TL, WEB)

FW_sp <- NULL
for (web in webs) {
  TLS <- TLS_all %>% filter(WEB == web) %>% select(taxon, TL, WEB)

  FW_raw <- tibble(read.csv2(paste("../../data/sub_networks_full/", web, ".txt", sep = ""))) %>%
    mutate(subweb = web) %>%
    mutate(L = length(consumer))

  edges <- FW_raw[, 1:2]
  taxon <- unique(as.vector(as.matrix(edges)))

  FW_single <- FW_raw %>%
    select(-c(consumer, resource)) %>%
    distinct() %>%
    mutate(S = length(taxon)) %>%
    mutate(C = L / S^2) %>%
    bind_cols(taxon) %>%
    rename(taxon = ...5) %>%
    inner_join(TLS, by = "taxon") %>%
    left_join(threatlist, by = "taxon") %>%
    mutate(across(all_of(names(threatlist)[-1]), ~ replace_na(.x, 0))) %>%
    left_join(ESSlist, by = "taxon") %>%
    mutate(across(all_of(names(ESSlist)[-1]), ~ replace_na(.x, 0)))

  FW_sp <- rbind(FW_single, FW_sp)
}

FW_sp <- FW_sp %>% rename(web = subweb)

# --- 4b. Plot function ---

plot_overlap_one_web <- function(FW_sp, threats_cols, ess_cols, w) {
  new <- NULL
  overlap_levels <- c("Neither", "Threatened", "ESS provider", "Both")

  for (t in threats_cols) {
    for (e in ess_cols) {
      sp_overlap <- FW_sp %>%
        filter(web == w) %>%
        select(web, taxon, all_of(t), all_of(e)) %>%
        mutate(ess = e, threat = t) %>%
        unite("overlap", all_of(t):all_of(e), sep = "_") %>%
        mutate(overlap = case_when(overlap == '1_0' ~ 'Threatened',
                                   overlap == '0_1' ~ 'ESS provider',
                                   overlap == '1_1' ~ 'Both',
                                   overlap == '0_0' ~ 'Neither'))
      new <- rbind(sp_overlap, new)
    }
  }

  new <- new %>% mutate(overlap = factor(overlap, levels = overlap_levels))

  threat_order_abbrev <- fig5_threat_order  # same order as Fig 5
  ess_order_abbrev    <- c("CO2_SEQ", "HAB_PRO", "FOOD_PRO", "REC_TOU")

  OL <- new %>%
    mutate(threat = factor(threat, levels = threat_order_abbrev)) %>%
    ggplot(aes(x = factor(ess, levels = ess_order_abbrev), fill = overlap)) +
    geom_bar() +
    scale_fill_manual(values = role_colors) +
    scale_x_discrete(labels = ess_labels) +
    theme_bw() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
      plot.margin  = unit(c(1, 1, 1, 1), "cm"),
      axis.text.y  = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      strip.text   = element_text(colour = 'black', size = 13),
      plot.title   = element_text(hjust = 0.5)
    ) +
    labs(x = "Ecosystem service", y = "Number of species", fill = "Species role(s)") +
    facet_wrap(~ factor(threat, levels = threat_order_abbrev), nrow = 2,
               labeller = as_labeller(threat_lookup_labels))

  return(list(plot = OL, data = new))
}

# --- 4c. Generate figure for one representative subregion ---

threats_abbrev_cols <- names(threatlist)[-1]
ess_abbrev_cols     <- names(ESSlist)[-1]

w_fig4 <- webs[21]  # representative subregion — adjust if needed

overlap_result <- plot_overlap_one_web(FW_sp, threats_abbrev_cols, ess_abbrev_cols, w_fig4)

fname_fig4 <- paste0("../../results/figures/for_ms/fig4_species_roles-", w_fig4, ".pdf")
ggsave(fname_fig4, plot = overlap_result$plot, width = 12, height = 8)

# --- 4d. Quantitative overlap summary (for text) ---

overlap_summary <- overlap_result$data %>%
  group_by(threat, ess) %>%
  summarise(
    n_total      = n(),
    n_both       = sum(overlap == "Both"),
    pct_both     = 100 * n_both / n_total,
    .groups = "drop"
  )

overlap_summary %>%
  filter(threat != "OIL_POL") %>%
  summarise(median_pct_both = median(pct_both), min_pct_both = min(pct_both), max_pct_both = max(pct_both))

overlap_summary %>%
  filter(threat == "OIL_POL")

# ============================================================
# 5. FIGURE 5 — Network amplification vs. consumer sensitivity (k)
#    (main text: linear/proportional ESS; SI: robust/vulnerable ESS)
# ============================================================

nw_data    <- readRDS(paste0("../../results/", cont_folder, "/ESS_changes_var.rds"))
no_nw_data <- readRDS(paste0("../../results/", cont_folder, "/ESS_changes_no_nw_var.rds"))

plot_fig5 <- function(nw_data, no_nw_data, EA, EB) {
  nw_avg <- nw_data %>%
    filter(ESS_alpha == EA, ESS_beta == EB) %>%
    mutate(k = beta / alpha) %>%
    group_by(threat, ESS, k) %>%
    summarise(retained_nw = mean(service_total), .groups = "drop")

  no_nw_avg <- no_nw_data %>%
    filter(ESS_alpha == EA, ESS_beta == EB) %>%
    group_by(threat, ESS) %>%
    summarise(retained_no_nw = mean(service_total), .groups = "drop")

  amp_data <- nw_avg %>%
    left_join(no_nw_avg, by = c("threat", "ESS")) %>%
    mutate(threat = factor(threat, levels = fig5_threat_order)) %>%
    mutate(amplification = (retained_no_nw - retained_nw) * 100)

  ggplot(amp_data, aes(x = k, y = amplification, color = threat)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    scale_color_manual(values = threat_colors_abbrev, labels = threat_lookup_labels) +
    facet_wrap(~ ESS, ncol = 2, labeller = as_labeller(ess_labels)) +
    labs(
      x = "Consumer sensitivity (k = beta / alpha)",
      y = "Additional service loss due to network effects (percentage points)",
      color = "Threat"
    ) +
    theme_bw()
}

# ============================================================
# 6. SI — Structural overlap vs. outcome (species/edge overlap
#    vs. retention difference, and vs. amplification difference)
# ============================================================

# --- 6a. Species and edge sets per web ---

web_files <- list.files("../../data/sub_networks_full/", full.names = TRUE)
web_names_overlap <- str_remove(basename(web_files), ".txt")

species_sets <- map(web_files, ~ {
  edges <- read.csv2(.x)
  unique(c(edges$consumer, edges$resource))
}) %>% setNames(web_names_overlap)

edge_sets <- map(web_files, ~ {
  edges <- read.csv2(.x)
  paste(edges$consumer, edges$resource, sep = "__")
}) %>% setNames(web_names_overlap)

jaccard <- function(a, b) length(intersect(a, b)) / length(union(a, b))

pairs <- combn(web_names_overlap, 2, simplify = FALSE)

pairwise_struct <- map_dfr(pairs, function(p) {
  tibble(
    web1 = p[1],
    web2 = p[2],
    species_sim = jaccard(species_sets[[p[1]]], species_sets[[p[2]]]),
    edge_sim    = jaccard(edge_sets[[p[1]]], edge_sets[[p[2]]])
  )
})

# --- 6b. Retention difference version (uses `test`, built in Fig 2 section) ---

pairwise_outcome <- function(test, t, e) {
  vals <- test %>%
    filter(threat == t, ESS == e) %>%
    select(web, service_total) %>%
    distinct()

  map_dfr(pairs, function(p) {
    v1 <- vals$service_total[vals$web == p[1]]
    v2 <- vals$service_total[vals$web == p[2]]
    if (length(v1) == 0 || length(v2) == 0) return(NULL)
    tibble(web1 = p[1], web2 = p[2], threat = t, ESS = e,
           outcome_diff = abs(v1 - v2))
  })
}

test_overlap <- ESS_FW %>%
  filter(alpha == 1, beta == 1, ESS_alpha == 1, ESS_beta == 1) %>%
  select(threat, web, service_total, ESS) %>%
  distinct()

outcome_all <- expand_grid(threat = unique(test_overlap$threat), ESS = unique(test_overlap$ESS)) %>%
  pmap_dfr(~ pairwise_outcome(test_overlap, ..1, ..2))

plot_data_retention <- outcome_all %>%
  left_join(pairwise_struct, by = c("web1", "web2"))

# --- 6c. Amplification difference version (uses nw_data/no_nw_data from Fig 5) ---

amp_by_web <- nw_data %>%
  filter(ESS_alpha == 1, ESS_beta == 1, alpha == 1, beta == 1) %>%
  select(threat, web, ESS, retained_nw = service_total) %>%
  left_join(
    no_nw_data %>%
      filter(ESS_alpha == 1, ESS_beta == 1) %>%
      select(threat, web, ESS, retained_no_nw = service_total),
    by = c("threat", "web", "ESS")
  ) %>%
  mutate(amplification = (retained_no_nw - retained_nw) * 100) %>%
  mutate(threat = case_match(
    threat,
    "HAB_DES" ~ "Habitat destruction",
    "ICE_LOS" ~ "Ice loss & increased water temp.",
    "OCE_ACI" ~ "Ocean acidification",
    "OIL_POL" ~ "Oil pollution",
    "PATH"    ~ "Pathogens",
    "POL"     ~ "Pollution & marine litter",
    "SEA_ABR" ~ "Seabed abrasion",
    "SEL_EXT" ~ "Selective species extraction",
    .default  = threat
  )) %>%
  mutate(ESS = case_match(
    ESS,
    "CO2_SEQ"  ~ "Carbon sequestration",
    "FOOD_PRO" ~ "Food provisioning",
    "HAB_PRO"  ~ "Habitat provisioning",
    "REC_TOU"  ~ "Recreation and tourism",
    .default   = ESS
  ))

pairwise_amplification <- function(amp_by_web, t, e) {
  vals <- amp_by_web %>%
    filter(threat == t, ESS == e) %>%
    select(web, amplification) %>%
    distinct()

  map_dfr(pairs, function(p) {
    v1 <- vals$amplification[vals$web == p[1]]
    v2 <- vals$amplification[vals$web == p[2]]
    if (length(v1) == 0 || length(v2) == 0) return(NULL)
    tibble(web1 = p[1], web2 = p[2], threat = t, ESS = e,
           amp_diff = abs(v1 - v2))
  })
}

amp_outcome_all <- expand_grid(threat = unique(amp_by_web$threat), ESS = unique(amp_by_web$ESS)) %>%
  pmap_dfr(~ pairwise_amplification(amp_by_web, ..1, ..2))

plot_data_amp <- amp_outcome_all %>%
  left_join(pairwise_struct, by = c("web1", "web2"))

# --- 6d. Shared plot function ---

plot_overlap_vs <- function(data, ess_name, y_var, x_var, y_label, x_label) {
  data %>%
    filter(ESS == ess_name) %>%
    mutate(threat = factor(threat, levels = fig1_threat_order)) %>%
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.3, size = 1, color = "grey30") +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6) +
    facet_wrap(~ threat, nrow = 2) +
    labs(x = x_label, y = y_label, title = ess_name) +
    theme_bw() +
    theme(strip.text = element_text(size = 11), axis.text = element_text(size = 9))
}

# --- 6e. Generate all 16 SI figures (4 services x 2 outcomes x 2 overlap measures) ---

dir_create("../../results/figures/for_ms_SI")

overlap_specs <- expand_grid(
  outcome = c("retention", "amplification"),
  overlap = c("species", "edge")
)

for (i in seq_len(nrow(overlap_specs))) {
  outcome <- overlap_specs$outcome[i]
  overlap <- overlap_specs$overlap[i]

  data    <- if (outcome == "retention") plot_data_retention else plot_data_amp
  y_var   <- if (outcome == "retention") "outcome_diff" else "amp_diff"
  y_label <- if (outcome == "retention") "Difference in service retention between subregion pairs"
  else "Difference in network amplification between subregion pairs"
  x_var   <- if (overlap == "species") "species_sim" else "edge_sim"
  x_label <- if (overlap == "species") "Species overlap between subregion pairs (Jaccard similarity)"
  else "Interaction overlap between subregion pairs (Jaccard similarity)"

  for (ess_name in ess_order) {
    p <- plot_overlap_vs(data, ess_name, y_var, x_var, y_label, x_label)
    fname <- paste0("../../results/figures/for_ms_SI/", overlap, "_overlap_vs_", outcome, "-",
                    gsub(" ", "_", ess_name), ".pdf")
    ggsave(fname, plot = p, width = 12, height = 6)
  }
}

# --- MAIN TEXT: linear/proportional ESS ---
p_fig5_main <- plot_fig5(nw_data, no_nw_data, EA = 1, EB = 1)
ggsave("../../results/figures/for_ms/fig5_amplification_main.pdf",
       plot = p_fig5_main, width = 10, height = 8)

# --- SI: robust ESS (ESS_alpha=1, ESS_beta=2) ---
p_fig5_robust <- plot_fig5(nw_data, no_nw_data, EA = 1, EB = 2)
ggsave("../../results/figures/for_ms_SI/fig5_amplification_SI_robust.pdf",
       plot = p_fig5_robust, width = 10, height = 8)

# --- SI: vulnerable ESS (ESS_alpha=2, ESS_beta=1) ---
p_fig5_vulnerable <- plot_fig5(nw_data, no_nw_data, EA = 2, EB = 1)
ggsave("../../results/figures/for_ms_SI/fig5_amplification_SI_vulnerable.pdf",
       plot = p_fig5_vulnerable, width = 10, height = 8)
