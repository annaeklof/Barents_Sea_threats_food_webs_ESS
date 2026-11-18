# Figure for sensitivity in the thereat magnitudes

source("../settings.R") # model version and results folder
magnitude_threats <- "var"

# Get akll wiggle_magnitude-folders 
wiggle_dirs <- dir_ls(paste0("../../results/", results_folder, "/wiggle_magnitude/"),
                      type = "directory") |>
  as.character()

ESS_raw <- wiggle_dirs |>
  set_names(basename(wiggle_dirs)) |>
  map(\(d) read_rds(file.path(d, paste0("ESS_changes_", magnitude_threats, ".rds")))) |>
  list_rbind(names_to = "wiggle_magnitude") |>
  mutate(wiggle_magnitude = as.numeric(wiggle_magnitude))

prep_ESS <- function(df) {
  df |>
    mutate(ESS_param = case_when(
      ESS_alpha == 1 & ESS_beta == 1 ~ "ESS: proportional",
      ESS_alpha == 1 & ESS_beta == 2 ~ "ESS: robust",
      ESS_alpha == 1 & ESS_beta == 5 ~ "ESS: highly robust",
      ESS_alpha == 2 & ESS_beta == 1 ~ "ESS: vulnerable",
      ESS_alpha == 5 & ESS_beta == 1 ~ "ESS: highly vulnerable",
      TRUE ~ ""
    )) |>
    mutate(ab_param = case_when(
      alpha == 1 & beta == 1 ~ "Consumers: linear",
      alpha == 1 & beta == 2 ~ "Consumers: sensitive",
      alpha == 1 & beta == 5 ~ "Consumers: highly sensitive",
      alpha == 2 & beta == 1 ~ "Consumers: resilient",
      alpha == 5 & beta == 1 ~ "Consumers: highly resilient",
      TRUE ~ ""
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
                                    "Consumers: sensitive   -   ESS: proportional")) |>
    mutate(ESS = case_match(ESS,
                            "CO2_SEQ"  ~ "Carbon sequestration",
                            "FOOD_PRO" ~ "Food provisioning",
                            "HAB_PRO"  ~ "Habitat provisioning",
                            "REC_TOU"  ~ "Recreation and tourism",
                            .default = "")) |>
    mutate(threat = case_match(threat,
                               "HAB_DES" ~ "Habitat destruction",
                               "ICE_LOS" ~ "Ice loss & increased water temp.",
                               "OCE_ACI" ~ "Ocean acidification",
                               "OIL_POL" ~ "Oil pollution",
                               "PATH"    ~ "Pathogens",
                               "POL"     ~ "Pollution & marine litter",
                               "SEA_ABR" ~ "Seabed abrasion",
                               "SEL_EXT" ~ "Selective species extraction",
                               .default = "")) |>
    mutate(threat = fct_reorder(threat, desc(service_total)),
           ESS    = fct_reorder(ESS, desc(service_total)))
}

ESS_all <- prep_ESS(ESS_raw)

# Which to plot
ESS_all <- ESS_all |>
  filter(wiggle_magnitude %in% c(0.8, 0.9, 1.0, 1.1, 1.2))


plot_wiggle_threats <- function(df) {
  ggplot(df, aes(x = wiggle_magnitude, y = service_total,
                 colour = threat, group = threat)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_y_continuous(name = "Service retained (%)", limits = c(0, 1),
                       labels = scales::label_percent()) +
    scale_x_continuous(name = "Wiggle magnitude") +
    scale_colour_brewer(name = NULL, palette = "Dark2") +
    facet_wrap(~ ESS, nrow = 2, dir = "v") +
    theme_bw() +
    theme(legend.position = "right")
}


outdir <- paste0("../../results/figures/wiggle_comparison/", magnitude_threats, "/")
dir_create(outdir)

list.files("../../data/sub_networks_full/") |>
  str_remove(".txt") |>
  walk(\(w) {
    d <- ESS_all |> filter(web == w)
    
    
    # plotting
    d |>
      filter(param_comb == "Consumers: linear   -   ESS: proportional") |>
      plot_wiggle_threats()
    ggsave(paste0(outdir, "consumer_linear_",magnitude_threats, "_ESS_wiggle_threats-", w, ".pdf"),
           width = 10, height = 7)
  })

wiggle_summary <- ESS_all |>
  filter(param_comb == "Consumers: linear   -   ESS: proportional",
         wiggle_magnitude %in% c(0.8, 0.9, 1, 1.1, 1.2)) |>
  group_by(wiggle_magnitude, ESS, threat) |>
  summarise(median = median(service_total),
            q25    = quantile(service_total, 0.25),
            q75    = quantile(service_total, 0.75),
            n      = n(),
            .groups = "drop")

ggplot(wiggle_summary, aes(x = wiggle_magnitude, y = median,
                           colour = threat, fill = threat)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(name = "Service retained (median, IQR)", limits = c(0, 1),
                     breaks = c(0, 0.5, 1), labels = scales::label_percent()) +
  scale_x_continuous(name = "Scaling factor for extinction probabilities", breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  scale_colour_brewer(name = NULL, palette = "Dark2") +
  scale_fill_brewer(name = NULL, palette = "Dark2") +
  facet_wrap(~ ESS, nrow = 2, dir = "v") +
  theme_bw() +
  theme(legend.position = "right")

ggsave(glue("../../results/figures/wiggle_comparison/",magnitude_threats,"/{magnitude_threats}_ESS_wiggle_summary_all_webs.pdf"),
       width = 10, height = 7)

