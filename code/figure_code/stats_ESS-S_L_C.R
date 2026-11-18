# =============================================================
# fig_SX_fwprops_table.R
# Univariate associations between food-web properties (S, L, C)
# and fraction of ecosystem service retained, per threat.
# Produces: table_SX.pdf  (base R only — no extra packages)
# =============================================================

library(tidyverse)  # only for data wrangling; broom for tidy()

# --- Parameters (match your main analysis) ---
a     <- 1
b     <- 1
a_ess <- 1
b_ess <- 1

# --- Load data ---
source("../settings.R") # model version and results folder
magnitude_threats <- "var"
ESS <- read_rds(paste0("../../results/", results_folder,
                       "/ESS_changes_", magnitude_threats, ".rds")) %>%
  mutate(threat = fct_reorder(threat, desc(service_total))) |>
  mutate(ESS = fct_reorder(ESS, desc(service_total)))

longer_FW <- read.csv2("../../results/figures/diff_subwebs/fw_props.csv")

ESS_FW <- ESS %>%
  inner_join(longer_FW, by = c("web", "threat", "ESS"))

# --- Filter to chosen parameters, relabel ESS ---
ESS_FW_filt <- ESS_FW %>%
  filter(alpha == a, beta == b, ESS_alpha == a_ess, ESS_beta == b_ess) %>%
 # filter(threat != "OIL_POL") %>%
  mutate(ESS = case_match(
    ESS,
    "CO2_SEQ"  ~ "Carbon sequestration",
    "FOOD_PRO" ~ "Food provisioning",
    "HAB_PRO"  ~ "Habitat provisioning",
    "REC_TOU"  ~ "Recreation and tourism",
    .default   = ESS
  ))

# --- Run univariate models: S, L, C separately, per threat x ESS ---

run_one <- function(df, threat_val, ess_val, var) {
  d <- df %>% filter(threat == threat_val, ESS == ess_val)
  f <- as.formula(paste("service_total ~ scale(", var, ")"))
  m <- lm(f, data = d)
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(threat = threat_val, ESS = ess_val, predictor = var)
}

combos <- ESS_FW_filt %>% select(threat, ESS) %>% distinct()

stat_univariate <- purrr::pmap_dfr(
  combos,
  function(threat, ESS) {
    bind_rows(
      run_one(ESS_FW_filt, threat, ESS, "S"),
      run_one(ESS_FW_filt, threat, ESS, "L"),
      run_one(ESS_FW_filt, threat, ESS, "C")
    )
  }
) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  select(threat, ESS, predictor, estimate, p.value, sig) %>%
  arrange(threat, ESS, predictor)

# --- Reshape to wide: one row per threat x ESS, columns S/L/C ---

threat_labels <- c(
  HAB_DES = "Habitat destruction"  ,
  ICE_LOS = "Ice loss & increased water temp.",
  OCE_ACI = "Ocean acidification",
  PATH    = "Pathogens" ,
  POL     = "Pollution & marine litter"  ,
  SEA_ABR = "Seabed abrasion"  ,
  SEL_EXT = "Selective species extraction",
  OIL_POL = "Oil pollution"  
)


wide_table <- stat_univariate %>%
  mutate(
    threat = recode(threat, !!!threat_labels),
    cell = sprintf("%.3f", round(estimate, 3) + 0)
  ) %>%
  select(threat, ESS, predictor, cell) %>%
  pivot_wider(names_from = predictor, values_from = cell) %>%
  select(threat, ESS, S, L, C) %>%
  mutate(threat = factor(threat, levels = c("Pathogens", "Seabed abrasion",
                                            "Ice loss & increased water temp.",
                                            "Ocean acidification", "Selective species extraction",
                                            "Pollution & marine litter", "Habitat destruction",
                                            "Oil pollution")),
         ESS = factor(ESS, levels = c("Carbon sequestration", "Habitat provisioning",
                                      "Food provisioning", "Recreation and tourism"))) %>%
  arrange(threat, ESS)

# =============================================================
# Draw table with base R graphics and save as PDF
# =============================================================

draw_table_pdf <- function(df, out_path,
                           col_headers = c("Threat", "Ecosystem service", "S", "L", "C"),
                           col_widths  = c(0.22, 0.30, 0.16, 0.16, 0.16),
                           row_height  = 0.28,
                           font_size   = 9,
                           header_font_size = 9.5) {
  
  n_rows <- nrow(df) + 1  # +1 for header
  page_height <- 1 + n_rows * row_height  # inches, rough
  page_width  <- 6.5  # inches
  
  pdf(out_path, width = page_width, height = page_height)
  par(mar = c(0, 0, 0, 0))
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xaxs = "i", yaxs = "i", bty = "n", xaxt = "n", yaxt = "n")
  
  x_starts <- c(0, cumsum(col_widths))
  y_top    <- 1
  row_h    <- y_top / n_rows
  
  # --- Header ---
  y0 <- y_top - row_h
  y1 <- y_top
  for (j in seq_along(col_headers)) {
    xc <- (x_starts[j] + x_starts[j + 1]) / 2
    hj <- if (j <= 2) 0.5 else 0.5  # centered; change to 0 for left-align on col 1-2
    text(xc, (y0 + y1) / 2, col_headers[j], font = 2, cex = header_font_size / 10)
  }
  segments(0, y0, 1, y0, lwd = 1.2)  # rule under header
  segments(0, y1, 1, y1, lwd = 1.5)  # top rule
  
  # --- Body rows, with merged "threat" cells (only print threat on first row of each group) ---
  threats_rle <- rle(as.character(df$threat))
  row_idx <- 1
  y_cursor <- y1 - row_h
  
  for (k in seq_along(threats_rle$values)) {
    th <- threats_rle$values[k]
    n_sub <- threats_rle$lengths[k]
    group_y_top <- y_cursor
    
    for (i in seq_len(n_sub)) {
      row <- df[row_idx, ]
      yc <- y_cursor - row_h / 2
      
      # Threat label: only on first row of group, vertically centered on group
      if (i == 1) {
        group_yc <- group_y_top - (n_sub * row_h) / 2
        text((x_starts[1] + x_starts[2]) / 2, group_yc,
             paste(strwrap(th, width = 18), collapse = "\n"),
             cex = font_size / 10, font = 1)
      }
      
      text((x_starts[2] + x_starts[3]) / 2, yc, row$ESS, cex = font_size / 10, adj = 0.5)
      text((x_starts[3] + x_starts[4]) / 2, yc, row$S,   cex = font_size / 10)
      text((x_starts[4] + x_starts[5]) / 2, yc, row$L,   cex = font_size / 10)
      text((x_starts[5] + x_starts[6]) / 2, yc, row$C,   cex = font_size / 10)
      
      y_cursor <- y_cursor - row_h
      row_idx  <- row_idx + 1
    }
    # rule between threat groups
    segments(0, y_cursor, 1, y_cursor, lwd = 0.6, col = "grey60")
  }
  
  segments(0, y_cursor, 1, y_cursor, lwd = 1.5)  # bottom rule
  
  dev.off()
}

out_dir <- "../../results/figures/diff_subwebs/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

draw_table_pdf(wide_table, file.path(out_dir, "table_stats_ESS-S-C-L.pdf"))

message("Done: ", file.path(out_dir, "table_SX.pdf"))