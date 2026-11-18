require(tidyverse)
require(fs)



# Plot ecosystem services
# Input:
# - ESS: table of ecosystem services, for one given (sub-)web
# Output:
# - A figure with projected loss of ecosystem services for each threat
plot_ESS <- function(ESS) {
  ggplot(ESS, aes(x = threat, y = service_total, fill = param_comb)) +
    geom_point(size = 3.5, alpha = 0.7, shape = 21, color = "black") +
    scale_y_continuous(name = "Service retained (%)", limits = c(0, 1),
                       breaks = c(0, 0.5, 1), labels = scales::label_percent()) +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(
      name = NULL,
      values = c(
                "Consumers: linear   -   ESS: robust"         = "#0072B2",
                 "Consumers: linear   -   ESS: vulnerable"     = "#D55E00",
                "Consumers: linear   -   ESS: proportional"    = "#DDDDDD",
                "Consumers: resilient   -   ESS: proportional" = "#56B4E9",
                "Consumers: sensitive   -   ESS: proportional" = "#E69F00"
                 # "Consumers: resilient" = "#2166AC",
                 # "Consumers: linear"     = "#B0B0B0",
                 # "Consumers: sensitive"  = "#B2182B"
                 )
    ) +
    facet_wrap(~ ESS, nrow = 2, scales = "fixed",  dir = "v") +
    guides(fill = guide_legend(ncol = 1)) +
    theme_bw() +
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



# Which parameters to plot:
source("../settings.R") # model version and results folder
magnitude_threats <- "var" # set to "equal" or "var"
wiggle_magnitude <- "1.0"

# Load main ecosystem service result file and massage it for plotting:
ESS <-
  read_rds(paste0("../../results/", results_folder,
                  "/wiggle_magnitude/",wiggle_magnitude,"/ESS_changes_", magnitude_threats, ".rds"))|>
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
                                  "Consumers: linear   -   ESS: vulnerable", 
                                  "Consumers: resilient   -   ESS: proportional",
                                  "Consumers: linear   -   ESS: proportional",
                                  "Consumers: sensitive   -   ESS: proportional"
                                  )) |>
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
  mutate(threat = fct_reorder(threat, desc(service_total))) |>
  mutate(ESS = fct_reorder(ESS, desc(service_total)))



# For each web, create and save a plot:
list.files("../../data/sub_networks_full/") |> # network files 
  str_remove(".txt") |> # remove extension (sub21.txt -> sub21, and so on)
  map(\(w) { # for each web, create and save plot with specific file name
    fname <- paste0("../../results/figures/main_both_param_comb/", magnitude_threats,
                    "/",wiggle_magnitude,"_",magnitude_threats, "_ESS_changes-", w, ".pdf")
    ESS |> filter(web == w) |> plot_ESS()
    ggsave(fname, width = 8, height = 10)
  })

# ---------------------------------------------------------------------------
# Summary figure over all sub-webs: median and IQR across webs
# ---------------------------------------------------------------------------

# Colours: same fills as plot_ESS, but darker grey for the
# linear-proportional error bar so it is visible on white
summary_fill <- c(
  "Consumers: linear   -   ESS: robust"          = "#0072B2",
  "Consumers: linear   -   ESS: vulnerable"      = "#D55E00",
  "Consumers: linear   -   ESS: proportional"    = "#DDDDDD",
  "Consumers: resilient   -   ESS: proportional" = "#56B4E9",
  "Consumers: sensitive   -   ESS: proportional" = "#E69F00"
)
summary_line <- summary_fill
summary_line["Consumers: linear   -   ESS: proportional"] <- "#888888"

plot_ESS_summary <- function(ESS_summary) {
  dodge <- position_dodge(width = 0.0)
  ggplot(ESS_summary, aes(x = threat, y = median, fill = param_comb)) +
    geom_linerange(aes(ymin = q25, ymax = q75, colour = param_comb),
                   position = dodge, linewidth = 0.6, show.legend = FALSE) +
    geom_point(size = 3, alpha = 0.8, shape = 21, colour = "black", position = dodge) +
    scale_y_continuous(name = "Service retained (median, IQR)", limits = c(0, 1),
                       breaks = c(0, 0.5, 1), labels = scales::label_percent()) +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(name = NULL, values = summary_fill) +
    scale_colour_manual(values = summary_line) +
    facet_wrap(~ ESS, nrow = 2, scales = "fixed", dir = "v") +
    guides(fill = guide_legend(ncol = 1)) +
    theme_bw() +
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

ESS_summary <- ESS |>
  mutate(ESS = fct_relevel(ESS, "Carbon sequestration", "Habitat provisioning",
                           "Food provisioning", "Recreation and tourism")) |>
  group_by(ESS, threat, param_comb) |>
  summarise(median = median(service_total),
            q25    = quantile(service_total, 0.25),
            q75    = quantile(service_total, 0.75),
            n      = n(),
            .groups = "drop")

plot_ESS_summary(ESS_summary)
ggsave(paste0("../../results/figures/main_both_param_comb/", magnitude_threats,
              "/", wiggle_magnitude, "_", magnitude_threats, "_ESS_changes-all_webs.pdf"),
       width = 8, height = 10)
