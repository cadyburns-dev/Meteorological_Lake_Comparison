

# calc_metrics is used inside metrics_vs_ref.
# Ensure vars match the column names in each dataset; adjust if your any column is named differently across met data.
# If any dataset lacks a variable, remove it from vars or handle with tryCatch.


# 04_analysis_plotting.R
# Purpose: Create  comparison plots + rolling diagnostics
# Inputs: processed daily CSVs + metrics_vs_ref() from 03_analysis_helpers.R
# Outputs: plots (for Quarto + optional saving)

library(tidyverse)
library(slider)
library(plotly)

# --- Source helpers (must exist relative to project root) ---
# run source below if not using quarto file
source("scripts/01_metrics_helpers.R")
source("scripts/03_analysis_helpers.R")

# -----------------------------
# Load processed daily data
# -----------------------------
# when coding with different data sets change the csv file to the new created files
era5     <- read_csv("data/processed/rotorua_era5_daily.csv", show_col_types = FALSE)
buoy     <- read_csv("data/processed/rotorua_buoy_daily.csv", show_col_types = FALSE)
ap1770   <- read_csv("data/processed/rotorua_airport_1770_daily.csv", show_col_types = FALSE)
twn40177 <- read_csv("data/processed/rotorua_town_40177_daily.csv", show_col_types = FALSE)
vcs_on   <- read_csv("data/processed/rotorua_vcs_on_daily.csv", show_col_types = FALSE)

#----------------------------------------
# Creating additional functions for plotting and later use in quarto
#----------------------------------------

# Enforce Date type (prevents join issues)
to_date <- function(df) { df$Date <- as.Date(df$Date); df }
era5     <- to_date(era5)
buoy     <- to_date(buoy)
ap1770   <- to_date(ap1770)
twn40177 <- to_date(twn40177)
vcs_on   <- to_date(vcs_on)
# -----------------------------
# Reference + targets
# -----------------------------
ref_df <- ap1770

targets_list <- list(
  ERA5       = era5,
  Buoy       = buoy,
  Town_40177 = twn40177,
  VCS_On     = vcs_on
)

target_colors <- c(
  Airport_1770 = "black",
  ERA5         = "#d95f02",
  Buoy         = "#7570b3",
  Town_40177   = "#f0c400",
  VCS_On       = "#1b9e77"
)

vars <- c("Temp_C", "Precip_mm", "Wind_Spd_ms", "RadSWD_Wm2")

# -----------------------------
# Metrics table (ready for Quarto)
# where ref_df refers to the reference site we choose e.g Airport - 01.metrics_helpers sets this up
# Where target_dr refers to the other datasets we are comparing e.g ERA5, Buoy, VCSN, Town
# -----------------------------
metrics_all <- purrr::imap_dfr(
  targets_list,
  ~ metrics_vs_ref(
    ref_df      = ref_df,
    target_df   = .x,
    target_name = .y,
    vars = vars,
    wet_threshold_mm = 1,
    windy_top_pct    = 0.10
  )
)

#print(metrics_all)

# ============================================================
# Plot helpers
# ============================================================

# -----------------------------
# Helper: overlap-aligned long table
# -----------------------------
# Why: comparisons must be on the SAME dates where the reference exists (honest overlap).
make_long_overlap <- function(ref_df, targets_list, var, ref_name = "Airport_1770") {

  ref_keep <- ref_df |>
    select(Date, value = all_of(var)) |>
    mutate(source = ref_name)

  targets_keep <- purrr::imap_dfr(
    targets_list,
    ~ .x |>
      select(Date, value = all_of(var)) |>
      mutate(source = .y)
  )

  df <- bind_rows(ref_keep, targets_keep) |>
    drop_na(value)

  ref_dates <- ref_keep |>
    drop_na(value) |>
    distinct(Date)

  df |> semi_join(ref_dates, by = "Date")
}

# -----------------------------
# Time series: line (Temp/Wind/Rad)
# Why: time series shows seasonal structure, drift, and event alignment across datasets.
# Note: precip is "event-like" and zero-inflated, so bars (hydrograph) are clearer than lines. = event aware precip analysis
# -----------------------------
plot_timeseries_line <- function(ref_df, targets_list, var, ref_name = "Airport_1770") {
  df <- make_long_overlap(ref_df, targets_list, var, ref_name = ref_name)

  ggplot(df, aes(Date, value, color = source)) +
    geom_line(alpha = 0.85) +
    facet_wrap(~ source, ncol = 1, scales = "fixed") +
    scale_color_manual(values = target_colors) +
    labs(
      title = paste0(var, " over time (aligned to reference overlap)"),
      x = NULL, y = var
    ) +
    theme_bw() +
    theme(legend.position = "none")
}

# -----------------------------
# Precip hydrograph (bars) + optional 7-day rolling sum
# -----------------------------
plot_precip_hydrograph <- function(ref_df, targets_list, ref_name = "Airport_1770", show_7day_sum = TRUE) {

  df <- make_long_overlap(ref_df, targets_list, "Precip_mm", ref_name = ref_name)

  p <- ggplot(df, aes(Date, value, fill = source)) +
    geom_col(alpha = 0.55) +
    facet_wrap(~ source, ncol = 1, scales = "fixed") +
    scale_fill_manual(values = target_colors) +
    labs(
      title = "Precipitation hydrograph (daily totals; overlap with reference)",
      x = NULL, y = "Precip (mm/day)"
    ) +
    theme_bw() +
    theme(legend.position = "none")

  # OPTIONAL 7-day sum is useful for storm periods but is on a different scale than daily totals.
    #  7-day rolling sum line per dataset to show storm periods more clearly
  # Allows to see week long totals over the daily bars
  if (isTRUE(show_7day_sum)) {
    df7 <- df |>
      arrange(source, Date) |>
      group_by(source) |>
      mutate(sum7 = slider::slide_dbl(value, ~ sum(.x, na.rm = TRUE), .before = 6, .complete = TRUE)) |>
      ungroup()

    p <- p +
      geom_line(data = df7, aes(Date, sum7, color = source), inherit.aes = FALSE, alpha = 0.85) +
      scale_color_manual(values = target_colors)
  }

  p
}

# -----------------------------
# Distributions

#  distribution = density for temp/wind/ran but hystogram for precip


# Why: distributions show bias in variability (e.g., too many moderate values, missing extremes).
# For precip: include wet-day-only to reduce distortion from many zero-rain days.
# Option to scale the ) rain days so they are ot inflaed -> Code below ->
#ggplot(df, aes(value, fill = source)) +
 # geom_histogram(bins = 40, alpha = 0.5, position = "identity") +
 # scale_x_continuous(trans = "log1p") +
 # labs(x = "Precip (mm/day) [log1p scale]")
# -----------------------------
plot_distribution <- function(ref_df, targets_list, var, ref_name = "Airport_1770") {
  df <- make_long_overlap(ref_df, targets_list, var, ref_name = ref_name)

  if (var == "Precip_mm") {
    ggplot(df, aes(value, fill = source)) +
      geom_histogram(bins = 40, alpha = 0.5, position = "identity") +
      scale_fill_manual(values = target_colors) +
      facet_wrap(~ source, ncol = 1, scales = "fixed") +
      labs(
        title = "Precip distribution (all days; zero-inflated)",
        x = "Precip (mm/day)", y = "Count"
      ) +
      theme_bw()
  } else {
    ggplot(df, aes(value, color = source)) +
      geom_density(linewidth = 1) +
      scale_color_manual(values = target_colors) +
      labs(
        title = paste0(var, " density (overlap with reference)"),
        x = var, y = "Density"
      ) +
      theme_bw()
  }
}

plot_precip_wetday_distribution <- function(ref_df, targets_list, threshold_mm = 1, ref_name = "Airport_1770") {
  df <- make_long_overlap(ref_df, targets_list, "Precip_mm", ref_name = ref_name)

  # Wet days defined using reference only (critical for fair event evaluation)
  ref_wet_dates <- ref_df |>
    select(Date, Precip_mm) |>
    drop_na(Precip_mm) |>
    filter(Precip_mm > threshold_mm) |>
    distinct(Date)

  df_wet <- df |> semi_join(ref_wet_dates, by = "Date")

  ggplot(df_wet, aes(value, fill = source)) +
    geom_histogram(bins = 40, alpha = 0.5, position = "identity") +
    scale_fill_manual(values = target_colors) +
    facet_wrap(~ source, ncol = 1, scales = "fixed") +
    labs(
      title = paste0("Precip distribution on wet days only (ref > ", threshold_mm, " mm)"),
      x = "Precip (mm/day)", y = "Count"
    ) +
    theme_bw()+
    theme(legend.position = "none")
}

plot_precip_cdf <- function(ref_df, targets_list, ref_name = "Airport_1770") {
  df <- make_long_overlap(ref_df, targets_list, "Precip_mm", ref_name = ref_name)

  ggplot(df, aes(value, color = source)) +
    stat_ecdf(geom = "step", pad = FALSE, linewidth = 1) +
    scale_color_manual(values = target_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Cumulative frequency of daily precipitation",
      x = "Precip (mm/day)",
      y = "Cumulative probability"
    ) +
    theme_bw()
}

# -----------------------------
# Scatter vs reference: faceted (one figure, ncol configurable)
# Why: shows point-by-point agreement; regression highlights slope/intercept bias.



 # not nessesary - single simple regression plots
# scatter vs reference wth regression line and 1:1
plot_scatter_vs_ref_single <- function(ref_df, target_df, target_name, var) {
  joined <- inner_join(
    ref_df |> select(Date, ref = all_of(var)),
    target_df |> select(Date, tgt = all_of(var)),
    by = "Date"
  ) |> drop_na(ref, tgt)
  
  ggplot(joined, aes(ref, tgt)) +
    geom_point(alpha = 0.35) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = paste0(var, ": ", target_name, " vs Airport_1770"),
      x = "Reference (Airport_1770)",
      y = paste0(target_name)
    ) +
    theme_bw()
}
# -----------------------------
plot_scatter_faceted <- function(ref_df, targets_list, var, ref_name = "Airport_1770", ncol = 2) {

  paired <- purrr::imap_dfr(
    targets_list,
    ~ inner_join(
      ref_df |> select(Date, ref = all_of(var)),
      .x     |> select(Date, tgt = all_of(var)),
      by = "Date"
    ) |>
      drop_na(ref, tgt) |>
      mutate(source = .y)
  )

  stats_df <- paired |>
    group_by(source) |>
    summarise(
      n_val    = n(),
      cor_val  = if (n_val > 1) cor(ref, tgt) else NA_real_,
      rmse_val = sqrt(mean((tgt - ref)^2)),
      bias_val = mean(tgt - ref),
      label = paste0(
        "n = ", n_val,
        "\nR = ", ifelse(is.na(cor_val), "NA", sprintf("%.3f", cor_val)),
        "\nRMSE = ", sprintf("%.3f", rmse_val),
        "\nBias = ", sprintf("%.3f", bias_val)
      ),
      .groups = "drop"
    )

  ggplot(paired, aes(ref, tgt)) +
    geom_point(aes(color = source), alpha = 0.35) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", se = TRUE) +
    geom_text(
      data = stats_df,
      aes(x = Inf, y = -Inf, label = label),
      inherit.aes = FALSE,
      hjust = 1.1, vjust = -0.2,
      color = "black" # fixed annotation color
    ) +
    facet_wrap(~ source, ncol = ncol, scales = "fixed") +
    scale_color_manual(values = target_colors) +
    coord_equal() +
    labs(
      title = paste0(var, ": datasets vs ", ref_name),
      x = paste0("Reference (", ref_name, ")"),
      y = paste0("Target (", var, ")")
    ) +
    theme_bw() +
    theme(legend.position = "none")
}

# -----------------------------
# Rolling diagnostics: MAE, RMSE, Bias, Correlation
# RMSE and MAE over time (rolling window)
# Why: a single global RMSE hides seasonal drift and regime changes.
# Rolling metrics show when/where datasets diverge.
# Adds rolling correlation to capture timing/structure agreement.
# -----------------------------
rolling_diagnostics <- function(ref_df, target_df, var, window_days = 30) {

  joined <- inner_join(
    ref_df    |> select(Date, ref = all_of(var)),
    target_df |> select(Date, tgt = all_of(var)),
    by = "Date"
  ) |>
    arrange(Date) |>
    drop_na(ref, tgt) |>
    mutate(err = tgt - ref)

  joined |>
    mutate(
      mae  = slider::slide_dbl(abs(err), mean, .before = window_days - 1, .complete = TRUE),
      rmse = slider::slide_dbl(err^2, ~ sqrt(mean(.x)), .before = window_days - 1, .complete = TRUE),
      bias = slider::slide_dbl(err, mean, .before = window_days - 1, .complete = TRUE),
      cor  = slider::slide2_dbl(
        ref, tgt,
        ~ if (sum(is.finite(.x) & is.finite(.y)) >= 3) cor(.x, .y, use = "complete.obs") else NA_real_,
        .before = window_days - 1,
        .complete = TRUE
      )
    ) |>
    select(Date, mae, rmse, bias, cor)
}

plot_rolling_diagnostics <- function(ref_df, target_df, target_name, var, window_days = 30) {

  df <- rolling_diagnostics(ref_df, target_df, var, window_days) |>
    pivot_longer(c(mae, rmse, bias, cor), names_to = "metric", values_to = "value")


  ggplot(df, aes(Date, value)) +
    geom_line() +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    labs(
      title = paste0("Rolling ", window_days, "-day diagnostics: ", target_name, " vs Airport_1770 (", var, ")"),
      x = NULL, y = NULL
    ) +
    theme_bw()
}


# ============================================================
# Example calls (keep these minimal for script testing)
# ============================================================

#print(plot_timeseries_line(ref_df, targets_list, "Temp_C"))
#print(plot_timeseries_line(ref_df, targets_list, "Wind_Spd_ms"))
# only plot timeseries is enough data avalable
# p_ts_rad  <- plot_timeseries_line(ref_df, targets_list, "RadSWD_Wm2")

#print(plot_precip_hydrograph(ref_df, targets_list, show_7day_sum = TRUE))
#p_hydro can be shown either with a running weekly total (show_7day_sum = TRUE or not (=FALSE)
#be sure to know the difference because the output plot is very different
#print(plot_precip_cdf(ref_df, targets_list))
#print(plot_precip_wetday_distribution(ref_df, targets_list, threshold_mm = 1))

#print(plot_scatter_faceted(ref_df, targets_list, "Temp_C", ncol = 2))
#print(plot_scatter_faceted(ref_df, targets_list, "Wind_Spd_ms", ncol = 2))
#print(plot_scatter_faceted(ref_df, targets_list, "Precip_mm", ncol = 2))

#print(plot_rolling_diagnostics(ref_df, era5, "ERA5", "Temp_C", window_days = 30))




# ============================================================
# EXTRA ANALYSIS: Event agreement + seasonal bias + climatology
# Paste below your existing plotting code (no changes above)
# ============================================================

library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# -----------------------------
# 1) EVENT AGREEMENT (hits/misses/false alarms)
# -----------------------------
# Why: correlation can look "good" while the model is wrong on events (rain/wind).
# This section quantifies:
#   - Hits: ref event & target event
#   - Misses: ref event & target no event
#   - False alarms: ref no event & target event
#   - Correct negatives: neither event
# Plus skill scores:
#   POD (prob detection), FAR (false alarm ratio), CSI (critical success index), Bias score.

#To understand more the free and open access book (Camper 3 "Binary events by Ian B. Mason Outlines the working) -> Forecast verification: a practitioner's guide in atmospheric science editors = Ian T Jolliffe and David B. Stephenson

event_skill <- function(ref_df, target_df, var, threshold,
                        direction = c(">", ">=", "<", "<=")) {
  direction <- match.arg(direction)

  joined <- inner_join(
    ref_df    |> select(Date, ref = all_of(var)),
    target_df |> select(Date, tgt = all_of(var)),
    by = "Date"
  ) |>
    drop_na(ref, tgt)

  ref_event <- switch(
    direction,
    ">"  = joined$ref >  threshold,
    ">=" = joined$ref >= threshold,
    "<"  = joined$ref <  threshold,
    "<=" = joined$ref <= threshold
  )

  tgt_event <- switch(
    direction,
    ">"  = joined$tgt >  threshold,
    ">=" = joined$tgt >= threshold,
    "<"  = joined$tgt <  threshold,
    "<=" = joined$tgt <= threshold
  )

  hits  <- sum(ref_event & tgt_event, na.rm = TRUE)
  miss  <- sum(ref_event & !tgt_event, na.rm = TRUE)
  fa    <- sum(!ref_event & tgt_event, na.rm = TRUE)
  cn    <- sum(!ref_event & !tgt_event, na.rm = TRUE)

  pod <- if ((hits + miss) > 0) hits / (hits + miss) else NA_real_
  far <- if ((hits + fa)   > 0) fa   / (hits + fa)   else NA_real_
  csi <- if ((hits + miss + fa) > 0) hits / (hits + miss + fa) else NA_real_
  bias_score <- if ((hits + miss) > 0) (hits + fa) / (hits + miss) else NA_real_



  tibble(
    n = nrow(joined),
    hits = hits, misses = miss, false_alarms = fa, correct_neg = cn,
    POD = pod, FAR = far, CSI = csi, bias_score = bias_score
  )
}

event_skill_all_targets <- function(ref_df, targets_list, var, threshold,
                                   direction = ">=",
                                   ref_name = "Airport_1770") {
  imap_dfr(targets_list, ~ {
    event_skill(ref_df, .x, var = var, threshold = threshold, direction = direction) |>
      mutate(target = .y, var = var, threshold = threshold, ref = ref_name, .before = 1)
  })
}

# --- Plot: stacked counts of event outcomes per target ---
plot_event_outcomes <- function(skill_tbl, title = NULL) {
  long <- skill_tbl |>
    select(target, hits, misses, false_alarms, correct_neg) |>
    pivot_longer(-target, names_to = "outcome", values_to = "count")

  ggplot(long, aes(target, count, fill = outcome)) +
    geom_col() +
    coord_flip() +
    labs(
      title = title %||% "Event agreement outcomes",
      x = NULL, y = "Count (days)", fill = NULL
    ) +
    theme_bw()
}

# --- Plot: POD/FAR/CSI per target (quick skill comparison) ---
plot_event_skill_scores <- function(skill_tbl, title = NULL) {
  long <- skill_tbl |>
    select(target, POD, FAR, CSI, bias_score) |>
    pivot_longer(-target, names_to = "metric", values_to = "value")

  ggplot(long, aes(target, value)) +
    geom_col() +
    facet_wrap(~ metric, ncol = 2, scales = "free_y") +
    coord_flip() +
    labs(
      title = title %||% "Event skill scores",
      x = NULL, y = NULL
    ) +
    theme_bw()
}

# --- Magnitude when disagreement occurs (precip example) ---
# "Ref dry but target wet": how much rain do they report?
plot_false_alarm_magnitude <- function(ref_df, targets_list,
                                      threshold_mm = 1,
                                      ref_name = "Airport_1770") {

  # Build paired dataset
  paired <- imap_dfr(
    targets_list,
    ~ inner_join(
      ref_df |> select(Date, ref = Precip_mm),
      .x     |> select(Date, tgt = Precip_mm),
      by = "Date"
    ) |>
      drop_na(ref, tgt) |>
      mutate(target = .y)
  )

  fa <- paired |>
    filter(ref <= threshold_mm, tgt > threshold_mm)

  ggplot(fa, aes(tgt, fill = target)) +
    geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
    scale_x_continuous(trans = "log1p") +
    scale_fill_manual(values = target_colors) +
    facet_wrap(~ target, ncol = 1, scales = "fixed") +
    labs(
      title = paste0("False alarms magnitude: ref ≤ ", threshold_mm, " mm but target > ", threshold_mm, " mm"),
      x = "Target precip (mm/day) [log1p]", y = "Count", fill = "Dataset"
    ) +
    theme_bw()
}

# --- Example calls (precip threshold default = 1 mm; easy to change) ---
precip_event_threshold <- 1  # change to 1 or 5 if you want sensitivity tests later

skill_precip <- event_skill_all_targets(
  ref_df, targets_list,
  var = "Precip_mm",
  threshold = precip_event_threshold,
  direction = ">=")

#print(skill_precip)
#print(plot_event_outcomes(skill_precip,
#  title = paste0("Precip events (≥ ", precip_event_threshold, " mm/day): outcomes vs reference")))
#print(plot_event_skill_scores(skill_precip,
#  title = paste0("Precip events (≥ ", precip_event_threshold, " mm/day): skill vs reference")))
#print(plot_false_alarm_magnitude(ref_df, targets_list, threshold_mm = precip_event_threshold))


# -----------------------------
# 2) MONTHLY CLIMATOLOGY OVERLAY (mean + IQR)
# -----------------------------
# Why: shows systematic seasonal bias in mean AND variability.
# Uses overlap with reference dates to ensure fair comparison.

make_monthly_climatology <- function(ref_df, targets_list, var,
                                     ref_name = "Airport_1770", ribbon = 0.15) {
  df <- make_long_overlap(ref_df, targets_list, var, ref_name = ref_name) |>
    mutate(
      month = month(Date),
      month_lab = factor(month.abb[month], levels = month.abb)
    )

  df |>
    group_by(source, month, month_lab) |>
    summarise(
      n = n(),
      mean = mean(value, na.rm = TRUE),
      q25  = quantile(value, 0.25, na.rm = TRUE, type = 7),
      q75  = quantile(value, 0.75, na.rm = TRUE, type = 7),
      .groups = "drop"
    )
}

plot_monthly_climatology <- function(ref_df, targets_list, var,
                                     ref_name = "Airport_1770",
                                     ribbon_alpha = 0.15) {

  clim <- make_monthly_climatology(ref_df, targets_list, var, ref_name)

  # Make one consistent palette including the reference
  pal <- c(setNames("black", ref_name), target_colors)

  ggplot(clim, aes(month_lab, mean, group = source, color = source)) +
    # ribbon uses fill but we HIDE its legend so only one legend remains
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = source),
                alpha = ribbon_alpha, color = NA, show.legend = FALSE) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(
      title = paste0("Monthly climatology: ", var, " (mean +/- IQR)"),
      x = NULL, y = var,
      color = "Dataset"
    ) +
    theme_bw() +
    theme(legend.position = "right")
}

# --- Example calls ---
#print(plot_monthly_climatology(ref_df, targets_list, "Temp_C"))
#print(plot_monthly_climatology(ref_df, targets_list, "Wind_Spd_ms"))
# Radiation only if you have decent overlap:
# print(plot_monthly_climatology(ref_df, targets_list, "RadSWD_Wm2"))


# -----------------------------
# 3) SEASON DEFINITIONS (NZ / Southern Hemisphere) with editable month sets
# -----------------------------
# Default SH seasons:
#   Summer = DJF (Dec-Jan-Feb)
#   Autumn = MAM
#   Winter = JJA
#   Spring = SON
#
# For "NZ warm vs cool season" style, can set:
#   e.g warm = Nov-April, cool = May-Oct
#
# You can change these month vectors without touching anything else.

season_defs_default <- list(
  summer = c(12, 1, 2),
  autumn = c(3, 4, 5),
  winter = c(6, 7, 8),
  spring = c(9, 10, 11)
)

season_defs_warmcool <- list(
  warm = c(11, 12, 1, 2, 3, 4),
  cool = c(5, 6, 7, 8, 9, 10)
)

assign_season <- function(date, season_defs = season_defs_default) {
  m <- month(date)
  out <- rep(NA_character_, length(m))
  for (nm in names(season_defs)) {
    out[m %in% season_defs[[nm]]] <- nm
  }
  factor(out, levels = names(season_defs))
}


# -----------------------------
# 4) SEASONAL METRICS (calc_metrics_* within each season)
# -----------------------------
# Why: claims "dataset X performs best in winter for wind" (defensible).

seasonal_metrics_vs_ref <- function(ref_df, target_df, target_name, var,
                                    season_defs = season_defs_default,
                                    wet_threshold_mm = 1,
                                    ref_name = "Airport_1770") {

  joined <- inner_join(
    ref_df    |> select(Date, ref = all_of(var), ref_precip = Precip_mm),
    target_df |> select(Date, tgt = all_of(var)),
    by = "Date"
  ) |>
    drop_na(ref, tgt) |>
    mutate(season = assign_season(Date, season_defs))

  # Optional: for precip you might want wet-only seasonal metrics
  # (zero inflation again). We keep both all-days and wet-days seasonal for precip.
  if (var == "Precip_mm") {
    out_all <- joined |>
      group_by(season) |>
      summarise(
        calc_metrics_all_days(ref, tgt),
        .groups = "drop"
      ) |>
      mutate(subset = "all_days")

    out_wet <- joined |>
      filter(ref > wet_threshold_mm) |>
      group_by(season) |>
      summarise(
        calc_metrics_all_days(ref, tgt),
        .groups = "drop"
      ) |>
      mutate(subset = paste0("wet_days_ref>", wet_threshold_mm, "mm"))

    bind_rows(out_all, out_wet) |>
      mutate(target = target_name, var = var, .before = 1)
  } else {
    joined |>
      group_by(season) |>
      summarise(
        calc_metrics_all_days(ref, tgt),
        .groups = "drop"
      ) |>
      mutate(target = target_name, var = var, subset = "all_days", .before = 1)
  }
}

seasonal_metrics_all_targets <- function(ref_df, targets_list, var,
                                        season_defs = season_defs_default,
                                        wet_threshold_mm = 1) {
  imap_dfr(targets_list, ~ seasonal_metrics_vs_ref(
    ref_df = ref_df,
    target_df = .x,
    target_name = .y,
    var = var,
    season_defs = season_defs,
    wet_threshold_mm = wet_threshold_mm
  ))
}

# --- Plot: seasonal bias (mean(target - ref)) for summer vs winter (or all seasons) ---
plot_seasonal_bias <- function(ref_df, targets_list, var,
                              season_defs = season_defs_default,
                              ref_name = "Airport_1770") {

  paired <- imap_dfr(
    targets_list,
    ~ inner_join(
      ref_df |> select(Date, ref = all_of(var)),
      .x     |> select(Date, tgt = all_of(var)),
      by = "Date"
    ) |>
      drop_na(ref, tgt) |>
      mutate(target = .y, season = assign_season(Date, season_defs),
             bias = tgt - ref)
  )

  summ <- paired |>
    group_by(target, season) |>
    summarise(
      n = n(),
      bias_mean = mean(bias, na.rm = TRUE),
      bias_q25  = quantile(bias, 0.25, na.rm = TRUE),
      bias_q75  = quantile(bias, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(summ, aes(season, bias_mean, fill = target)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(
      aes(ymin = bias_q25, ymax = bias_q75),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    scale_fill_manual(values = target_colors) +
    labs(
      title = paste0("Seasonal bias: ", var, " (target - reference)"),
      x = NULL, y = "Bias", fill = "Dataset"
    ) +
    theme_bw()
}

# --- Plot: seasonal scatter (faceted by season + target) ---
plot_seasonal_scatter <- function(ref_df, targets_list, var,
                                 season_defs = season_defs_default,
                                 ref_name = "Airport_1770") {

  paired <- imap_dfr(
    targets_list,
    ~ inner_join(
      ref_df |> select(Date, ref = all_of(var)),
      .x     |> select(Date, tgt = all_of(var)),
      by = "Date"
    ) |>
      drop_na(ref, tgt) |>
      mutate(target = .y, season = assign_season(Date, season_defs))
  )

  ggplot(paired, aes(ref, tgt)) +
    geom_point(alpha = 0.25) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(season ~ target) +
    coord_equal() +
    labs(
      title = paste0("Seasonal scatter: ", var, " (datasets vs reference)"),
      x = paste0("Reference (", ref_name, ")"),
      y = "Target"
    ) +
    theme_bw() +
    theme(legend.position = "none")
}

# --- Example calls: pick one season definition ---
season_defs <- season_defs_default
# season_defs <- season_defs_warmcool  # <- switch to warm/cool quickly

# Seasonal bias plots
#print(plot_seasonal_bias(ref_df, targets_list, "Temp_C", season_defs = season_defs))
#print(plot_seasonal_bias(ref_df, targets_list, "Wind_Spd_ms", season_defs = season_defs))

# Seasonal metrics tables (useful to print in Quarto later)
#season_metrics_wind <- seasonal_metrics_all_targets(ref_df, targets_list, "Wind_Spd_ms", season_defs = season_defs)
#season_metrics_precip <- seasonal_metrics_all_targets(ref_df, targets_list, "Precip_mm", season_defs = season_defs, wet_threshold_mm = precip_event_threshold)

#print(season_metrics_wind)
#print(season_metrics_precip)

# Seasonal scatter (big figure, but very informative)
#print(plot_seasonal_scatter(ref_df, targets_list, "Wind_Spd_ms", season_defs = season_defs))
# print(plot_seasonal_scatter(ref_df, targets_list, "Precip_mm", season_defs = season_defs))  # can be heavy; optional


