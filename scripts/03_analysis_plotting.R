

# calc_metrics is used inside metrics_vs_ref.
# Ensure vars match the column names in each dataset; adjust if your any column is named differently across met data.
# If any dataset lacks a variable, remove it from vars or handle with tryCatch.


# 03_analysis_plotting.R
# Purpose: Create publication-quality comparison plots + rolling diagnostics
# Inputs: processed daily CSVs + metrics_vs_ref() from 02_analysis_helpers.R
# Outputs: plots (for Quarto + optional saving)

library(tidyverse)
library(slider)

# --- Source helpers (must exist relative to project root) ---
source("scripts/00_metrics_helpers.R")
source("scripts/02_analysis_helpers.R")

# -----------------------------
# Load processed daily data
# -----------------------------
era5     <- read_csv("data/processed/rotorua_era5_daily.csv", show_col_types = FALSE)
buoy     <- read_csv("data/processed/rotorua_buoy_daily.csv", show_col_types = FALSE)
ap1770   <- read_csv("data/processed/rotorua_airport_1770_daily.csv", show_col_types = FALSE)
twn40177 <- read_csv("data/processed/rotorua_town_40177_daily.csv", show_col_types = FALSE)
vcs_on   <- read_csv("data/processed/rotorua_vcs_on_daily.csv", show_col_types = FALSE)

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

print(metrics_all)

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

#  distribution = density for temp/wind/ran but hustogram for precip


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
# Fix 1: coord_equal() ensures 1:1 line is visually meaningful.
# Fix 2: annotate summary stats so the figure is self-contained.


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

print(plot_timeseries_line(ref_df, targets_list, "Temp_C"))
print(plot_timeseries_line(ref_df, targets_list, "Wind_Spd_ms"))
# only plot timeseries is enough data avalable
# p_ts_rad  <- plot_timeseries_line(ref_df, targets_list, "RadSWD_Wm2")

print(plot_precip_hydrograph(ref_df, targets_list, show_7day_sum = TRUE))
#p_hydro can be shown either with a running weekly total (show_7day_sum = TRUE or not (=FALSE)
#be sure to know the difference because the output plot is very different
print(plot_precip_cdf(ref_df, targets_list))
print(plot_precip_wetday_distribution(ref_df, targets_list, threshold_mm = 1))

print(plot_scatter_faceted(ref_df, targets_list, "Temp_C", ncol = 2))
print(plot_scatter_faceted(ref_df, targets_list, "Wind_Spd_ms", ncol = 2))
print(plot_scatter_faceted(ref_df, targets_list, "Precip_mm", ncol = 2))

print(plot_rolling_diagnostics(ref_df, era5, "ERA5", "Temp_C", window_days = 30))









#+#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# to do later --- add another script for seaons?? or maybe not use?
filter_season <- function(date, season = c("summer", "winter")) { ... }
# this will show something so we can see when it preforms best e.g (but not acurat would be) “ERA5 performs best in winter for wind, but buoy performs best during summer convective rainfall.”
