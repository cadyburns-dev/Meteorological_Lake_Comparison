

library(slider)


# --- Source helpers
source("scripts/00_metrics_helpers.R")
source("scripts/02_analysis_helpers.R")




# calc_metrics is used inside metrics_vs_ref.
# Ensure vars match the column names in each dataset; adjust if your any column is named differently across met data.
# If any dataset lacks a variable, remove it from vars or handle with tryCatch.



# load processed daily data
era5     <- read_csv("data/processed/rotorua_era5_daily.csv", show_col_types = FALSE)
buoy     <- read_csv("data/processed/rotorua_buoy_daily.csv", show_col_types = FALSE)
ap1770   <- read_csv("data/processed/rotorua_airport_1770_daily.csv", show_col_types = FALSE)
twn40177 <- read_csv("data/processed/rotorua_town_40177_daily.csv", show_col_types = FALSE)
vcs_on   <- read_csv("data/processed/rotorua_vcs_on_daily.csv", show_col_types = FALSE)


# ---- enforce Date class (prevents join issues) ----
to_date <- function(df) { df$Date <- as.Date(df$Date); df }
era5     <- to_date(era5)
buoy     <- to_date(buoy)
ap1770   <- to_date(ap1770)
twn40177 <- to_date(twn40177)
vcs_on   <- to_date(vcs_on)

# ---- reference + targets ----
ref_df <- ap1770  # explicit reference
targets_list <- list(
  ERA5       = era5,
  Buoy       = buoy,
  Town_40177 = twn40177,
  VCS_On     = vcs_on
)

target_colors <- c(
  ERA5          = "#d95f02",  # orange
  Buoy          = "#7570b3",  # purple
  Town_40177 = "#f0c400",  # deep yellow
  VCS_On        = "#1b9e77"   # green
)

vars <- c("Temp_C", "Precip_mm", "Wind_Spd_ms", "RadSWD_Wm2")

# ---- metrics table (tidy, ready for Quarto) ----
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


# helper come plot consistance 
make_long_overlap <- function(ref_df, targets_list, var, ref_name = "Airport_1770") {
  
  ref_keep <- ref_df |>
    select(Date, value = all_of(var)) |>
    mutate(source = ref_name)
  
  targets_keep <- imap_dfr(targets_list, ~ .x |>
                             select(Date, value = all_of(var)) |>
                             mutate(source = .y))
  
  df <- bind_rows(ref_keep, targets_keep) |>
    drop_na(value)
  
  # keep only dates present in the reference (so overlap is honest)
  ref_dates <- ref_keep |>
    drop_na(value) |>
    distinct(Date)
  
  df |>
    semi_join(ref_dates, by = "Date")
}


# time serice overlay
# -----------------------------
# Plot helpers: time series
# -----------------------------
# Why: time series shows seasonal structure, drift, and event alignment across datasets.
# Note: precip is "event-like" and zero-inflated, so bars (hydrograph) are clearer than lines. = event aware precip analysis

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
  
  # keep only dates where reference exists -> honest overlap
  ref_dates <- ref_keep |>
    drop_na(value) |>
    distinct(Date)
  
  df |>
    semi_join(ref_dates, by = "Date")
}

plot_timeseries_line <- function(ref_df, targets_list, var) {
  df <- make_long_overlap(ref_df, targets_list, var)
  
  ggplot(df, aes(Date, value, color = source)) +
    geom_line(alpha = 0.8) +
    labs(
      title = paste0(var, " over time (aligned to reference overlap)"),
      x = NULL, y = var, color = "Dataset"
    ) +
    theme_bw()
}

plot_precip_hydrograph <- function(ref_df, targets_list, var = "Precip_mm", show_7day_sum = TRUE) {
  df <- make_long_overlap(ref_df, targets_list, var)
  
  p <- ggplot(df, aes(Date, value, fill = source)) +
    geom_col(alpha = 0.55, position = "identity") +
    labs(
      title = "Precipitation hydrograph (daily totals; overlap with reference)",
      x = NULL, y = "Precip (mm/day)", fill = "Dataset"
    ) +
    theme_bw()
  
  # Optional: 7-day rolling sum line per dataset to show storm periods more clearly
  if (isTRUE(show_7day_sum)) {
    df7 <- df |>
      arrange(source, Date) |>
      group_by(source) |>
      mutate(sum7 = slider::slide_dbl(value, sum, .before = 6, .complete = TRUE, na.rm = TRUE)) |>
      ungroup()
    
    p <- p +
      geom_line(data = df7, aes(Date, sum7, color = source), inherit.aes = FALSE, alpha = 0.8) +
      guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))
  }
  
  p
}

# --- Example calls ---
p_ts_temp <- plot_timeseries_line(ref_df, targets_list, "Temp_C")
p_ts_wind <- plot_timeseries_line(ref_df, targets_list, "Wind_Spd_ms")
# only do radiation if overlap is adequate
# p_ts_rad  <- plot_timeseries_line(ref_df, targets_list, "RadSWD_Wm2")

p_hydro <- plot_precip_hydrograph(ref_df, targets_list, "Precip_mm", show_7day_sum = TRUE)

print(p_ts_temp); print(p_ts_wind); print(p_hydro)



# Frequency distribution = density for temp/wind/ran but hustogram for precip
# -----------------------------
# Plot helpers: distributions
# -----------------------------
# Why: distributions show bias in variability (e.g., too many moderate values, missing extremes).
# For precip: include wet-day-only to reduce distortion from many zero-rain days.

plot_distribution <- function(ref_df, targets_list, var) {
  df <- make_long_overlap(ref_df, targets_list, var)
  
  if (var == "Precip_mm") {
    ggplot(df, aes(value, fill = source)) +
      geom_histogram(bins = 40, alpha = 0.5, position = "identity") +
      labs(
        title = paste0(var, " distribution (all days; overlap with reference)"),
        x = var, y = "Count", fill = "Dataset"
      ) +
      theme_bw()
  } else {
    ggplot(df, aes(value, color = source)) +
      geom_density() +
      labs(
        title = paste0(var, " density (overlap with reference)"),
        x = var, y = "Density", color = "Dataset"
      ) +
      theme_bw()
  }
}

plot_precip_wetday_distribution <- function(ref_df, targets_list, threshold_mm = 1) {
  df <- make_long_overlap(ref_df, targets_list, "Precip_mm")
  
  # define wet days from the REFERENCE only
  ref_wet_dates <- ref_df |>
    select(Date, Precip_mm) |>
    drop_na(Precip_mm) |>
    filter(Precip_mm > threshold_mm) |>
    distinct(Date)
  
  df_wet <- df |>
    semi_join(ref_wet_dates, by = "Date")
  
  ggplot(df_wet, aes(value, fill = source)) +
    geom_histogram(bins = 40, alpha = 0.5, position = "identity") +
    labs(
      title = paste0("Precip distribution on wet days only (ref > ", threshold_mm, " mm)"),
      x = "Precip (mm/day)", y = "Count", fill = "Dataset"
    ) +
    theme_bw()
}

# --- Example calls ---
print(plot_distribution(ref_df, targets_list, "Temp_C"))
print(plot_distribution(ref_df, targets_list, "Wind_Spd_ms"))
print(plot_distribution(ref_df, targets_list, "Precip_mm"))
print(plot_precip_wetday_distribution(ref_df, targets_list, threshold_mm = 1))


# scatter vs reference wth regression line and 1:1
# -----------------------------
# Plot helpers: scatter vs reference
# -----------------------------
# Why: shows point-by-point agreement; regression highlights slope/intercept bias.
# Fix 1: coord_equal() ensures 1:1 line is visually meaningful.
# Fix 2: annotate summary stats so the figure is self-contained.

plot_scatter_vs_ref <- function(ref_df, target_df, target_name, var) {
  
  joined <- inner_join(
    ref_df   |> select(Date, ref = all_of(var)),
    target_df |> select(Date, tgt = all_of(var)),
    by = "Date"
  ) |> drop_na(ref, tgt)
  
  # summary stats for annotation
  n_val    <- nrow(joined)
  cor_val  <- if (n_val > 1) cor(joined$ref, joined$tgt) else NA_real_
  rmse_val <- sqrt(mean((joined$tgt - joined$ref)^2))
  bias_val <- mean(joined$tgt - joined$ref)
  
  ann <- paste0(
    "n = ", n_val,
    "\nR = ", ifelse(is.na(cor_val), "NA", sprintf("%.3f", cor_val)),
    "\nRMSE = ", sprintf("%.3f", rmse_val),
    "\nBias = ", sprintf("%.3f", bias_val)
  )

  ggplot(joined, aes(ref, tgt)) +
    geom_point(alpha = 0.35) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", se = TRUE) +
    coord_equal() +
    annotate("text", x = Inf, y = -Inf, label = ann, hjust = 1.1, vjust = -0.2) +
    labs(
      title = paste0(var, ": ", target_name, " vs Airport_1770"),
      x = "Reference (Airport_1770)",
      y = paste0(target_name)
    ) +
    theme_bw()
}

# --- Example calls ---
print(plot_scatter_vs_ref(ref_df, era5,   "ERA5",   "Temp_C"))
print(plot_scatter_vs_ref(ref_df, buoy,   "Buoy",   "Wind_Spd_ms"))
print(plot_scatter_vs_ref(ref_df, vcs_on, "VCS_On", "Precip_mm"))




# RMSE and MAE over time (rolling window)
# -----------------------------
# Rolling diagnostics
# -----------------------------
# Why: a single global RMSE hides seasonal drift and regime changes.
# Rolling metrics show when/where datasets diverge.
# Adds rolling correlation to capture timing/structure agreement.

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
      
      # rolling correlation must be computed on paired ref/tgt windows
      cor  = slider::slide2_dbl(
        ref, tgt,
        ~ if (sum(is.finite(.x) & is.finite(.y)) >= 3) cor(.x, .y, use = "complete.obs") else NA_real_,
        .before = window_days - 1,
        .complete = TRUE
      )
    ) |>
    select(Date, mae, rmse, cor)
}

plot_rolling_diagnostics <- function(ref_df, target_df, target_name, var, window_days = 30) {
  
  df <- rolling_diagnostics(ref_df, target_df, var, window_days) |>
    pivot_longer(c(mae, rmse, cor), names_to = "metric", values_to = "value")
  
  ggplot(df, aes(Date, value)) +
    geom_line() +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    labs(
      title = paste0("Rolling ", window_days, "-day diagnostics: ", target_name, " vs Airport_1770 (", var, ")"),
      x = NULL, y = NULL
    ) +
    theme_bw()
}




# Need slider package
# renv::install("slider") if missing
library(slider)
# --- Example calls (change window_days freely) ---
print(plot_rolling_diagnostics(ref_df, era5,   "ERA5",   "Temp_C",      window_days = 30))
print(plot_rolling_diagnostics(ref_df, vcs_on, "VCS_On", "Wind_Spd_ms",  window_days = 60))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+# old plots blow review later date 

#----------------------------------------
  #2.  Scatter plot Variable, target vs reference (airport)
#----------------------------------------
#----------------------------------------
  #3.  Time series overlay (All met data)
#----------------------------------------
#----------------------------------------
# Time series plots
#----------------------------------------



#----------------------------------------
   #  Distribution plots (density or Histagram??) - per source
#----------------------------------------
#----------------------------------------
  #DELETE# Scatter plots: one variable at a time, all targets together vs 1770
#----------------------------------------
  targets_list <- list(
     ERA5          = era5,
    Buoy          = buoy,
      Town_40177 = twn40177,
      VCS_On        = vcs_on 
    )

  var_colors <- c(
      Temp_C      = "#ff6fb7",  # pink
      Wind_Spd_ms = "#8c6fff",  # purple
      RadSWD_Wm2  = "#f0c400",  # deep yellow
      Precip_mm   = "#7ec8ff"   # light blue
    )

  plot_var_all_sites <- function(var) {
      color_var <- var_colors[[var]]
    
        df_all <- bind_rows(lapply(names(targets_list), function(nm) {
            joined <- inner_join(ap1770, targets_list[[nm]], by = "Date", suffix = c("_ref", "_tgt"))
            tibble(
                target = nm,
                ref    = joined[[paste0(var, "_ref")]],
                tgt    = joined[[paste0(var, "_tgt")]]
              ) |> drop_na(ref, tgt)
          }))
      
        ggplot(df_all, aes(x = ref, y = tgt, shape = target)) +
            geom_point(color = color_var, alpha = 0.5) +
            geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
            labs(
                title = paste0(var, " targets vs Airport 1770"),
                x = "Airport 1770",
                y = "Target",
                shape = "Target site"
              ) +
            theme_bw()
      }

  # Example plots
p_temp   <- plot_var_all_sites("Temp_C")
p_wind   <- plot_var_all_sites("Wind_Spd_ms")
p_rad    <- plot_var_all_sites("RadSWD_Wm2")
p_precip <- plot_var_all_sites("Precip_mm")


print(p_temp)
print(p_wind)
print(p_rad)
print(p_precip)

  # Print or ggsave as needed, e.g.:
  
#----------------------------------------
  # Faceted Scatter plot: all targets together, facets by variable (one figure
#----------------------------------------

  # One figure, one variable (Temp_C), facets by target (vs 1770)
  targets_list <- list(
      ERA5          = era5,
      Buoy          = buoy,
      Town_40177 = twn40177,
      VCS_On        = vcs_on
    )

  target_colors <- c(
      ERA5          = "#d95f02",  # pink
      Buoy          = "#7570b3",  # purple
      Town_40177 = "#f0c400",  # deep yellow
      VCS_On        = "#1b9e77"   # gray
  )

  plot_var_by_target <- function(var) {
      df_all <- bind_rows(lapply(names(targets_list), function(nm) {
          joined <- inner_join(ap1770, targets_list[[nm]], by = "Date", suffix = c("_ref", "_tgt"))
          tibble(
              target = nm,
              ref    = joined[[paste0(var, "_ref")]],
              tgt    = joined[[paste0(var, "_tgt")]]
            ) 
        })) |> drop_na(ref, tgt)
      
        ggplot(df_all, aes(x = ref, y = tgt, color = target, group = target)) +
          geom_point(alpha = 0.5) +
          geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
          facet_wrap(~ target, scales = "free", ncol = 1) + #ncol = 1 brings each plot on its own row
          scale_color_manual(values = target_colors) +
          labs(
              title = paste0(var, " Met data vs Airport"),
              x =  paste0("Airport 1770,",var),
              y = paste0("Met data,", var),
              color = "Meterological data"
            ) +
          theme_bw()
  }

  f_temp_C <- plot_var_by_target("Temp_C")
  f_wind_ms <- plot_var_by_target("Wind_Spd_ms")
  f_rad_Wm2 <- plot_var_by_target("RadSWD_Wm2")
  f_rain_mm <- plot_var_by_target("Precip_mm")
print(f_temp_C)
print(f_wind_ms)
print(f_rad_Wm2)
print(f_rain_mm)


# save individually -> single saves |>
save_fig(f_temp_C,  "Temp_C_facet_scatter.png")
save_fig(f_wind_ms, "Wind_ms_facet_scatter.png")
save_fig(f_rad_Wm2, "RAD_Wm2_facet_scatter.png")
save_fig(f_rain_mm, "Rain_mm_facet_scatter.png")




#---------------------------------------
  # Quick scatter plots vs 1770 for each target and variable
#----------------------------------------
plot_vs_ref <- function(target_df, target_name) {
  inner_join(ap1770, target_df, by = "Date", suffix = c("_ref", "_tgt")) |>
    pivot_longer(cols = ends_with("_ref"), names_to = "var_ref", values_to = "ref") |>
    mutate(var = str_remove(var_ref, "_ref")) |>
    left_join(
      pivot_longer(target_df, cols = all_of(vars), names_to = "var", values_to = "tgt"),
      by = c("Date", "var")
    ) |>
    filter(var %in% vars) |>
    ggplot(aes(x = ref, y = tgt)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(~ var, scales = "free") +
    labs(
      title = paste("Target:", target_name, "vs Town 1770"),
      x = "1770 reference",
      y = target_name
    ) +
    theme_bw()
}

p_era5    <- plot_vs_ref(era5, "ERA5")
p_buoy    <- plot_vs_ref(buoy, "Buoy")
p_40177   <- plot_vs_ref(twn40177, "Town 40177")
p_vcson   <- plot_vs_ref(vcs_on, "VCS On")

print(p_era5)
print(p_buoy)
print(p_40177)
print(p_vcson)


# NOTE TO SELF - when happy with figs makecodeto save figs to file as i manually have ben doing it also to get shading on trend line you can add model fedality, to it, make line in front and and shade
message("scatter plots complete")
#----------------------------------------
  # Correlation plots X= date var  independent
#----------------------------------------

targets_list <- list(
  ERA5          = era5,
  Buoy          = buoy,
  Town_40177 = twn40177,
  VCS_On        = vcs_on
)

target_colors <- c(
  ERA5          = "#d95f02",  # pink
  Buoy          = "#7570b3",  # purple
  Town_40177 = "#f0c400",  # deep yellow
  VCS_On        = "#1b9e77"   # gray
)

plot_var_by_target <- function(var) {
  df_all <- bind_rows(lapply(names(targets_list), function(nm) {
    joined <- inner_join(ap1770, targets_list[[nm]], by = "Date", suffix = c("_ref", "_tgt"))
    tibble(
      target = nm,
      ref    = joined[[paste0(var, "_ref")]],
      tgt    = joined[[paste0(var, "_tgt")]]
    ) 
  })) |> drop_na(ref, tgt)
  
  ggplot(df_all, aes(x = ref, y = tgt, color = target, group = target)) +
    geom_line(alpha = 0.5) +
    facet_wrap(~ target, scales = "free", ncol = 1) + #ncol = 1 brings each plot on its own row
    scale_color_manual(values = target_colors) +
    labs(
      title = paste0(var, " Corolation"),
      x = "Date",
      y = "Site",
      color = "Meterological data"
    ) +
    theme_bw()
}

p_temp_C <- plot_var_by_target("Temp_C")
p_wind_ms <- plot_var_by_target("Wind_Spd_ms")
p_rad_Wm2 <- plot_var_by_target("RadSWD_Wm2")
p_rain_mm <- plot_var_by_target("Precip_mm")  # Note zero-inflated data can be misleading in correlation graph
print(p_temp_C)
print(p_wind_ms)
print(p_rad_Wm2)
print(p_rain_mm)

#Save figures to outputs -> figures using function created
# loop over many plots - to save all at once 
plots <- list(
  Temp_C_Correlation = p_temp_C,
  Wind_ms_Correlation = p_wind_ms,
  RAD_Wm2_Correlation  = p_rad_Wm2,
  Rain_mm_Correlation = p_rain_mm
)

purrr::iwalk(plots, ~ save_fig(.x, paste0(.y, ".png")))

# Or if you prefer to save individually -> single saves |>
#save_fig(p_temp_C,  "Temp_C_Correlation.png")
#save_fig(p_wind_ms, "Wind_ms_Correlation.png")
#save_fig(p_rad_Wm2, "RAD_Wm2_Correlation.png")
#save_fig(p_rain_mm, "Rain_mm_Correlation.png")

#----------------------------------------
  # Plots all stations together
#----------------------------------------
era5    <- read_csv("data/processed/rotorua_era5_daily.csv", show_col_types = FALSE)
buoy    <- read_csv("data/processed/rotorua_buoy_daily.csv", show_col_types = FALSE)
ap1770  <- read_csv("data/processed/rotorua_airport_1770_daily.csv", show_col_types = FALSE)
twn40177 <- read_csv("data/processed/rotorua_town_40177_daily.csv", show_col_types = FALSE)
vcs_on <- read_csv("data/processed/rotorua_vcs_on_daily.csv", show_col_types = FALSE)

# Vars to compare (must exist in both ref and target)
vars <- c("Temp_C", "Precip_mm", "Wind_Spd_ms", "RadSWD_Wm2")

library(dplyr)
library(tidyr)
library(ggplot2)

# Combine all targets into one long table
target_list <- list(
  ERA5          = era5,
  Buoy          = buoy,
  Town_40177    = twn40177,
  VCS_On        = vcs_on,
  Airport_1770  = ap1770
)

target_colors <- c(
  ERA5          = "#d95f02",
  Buoy          = "#7570b3",
  Town_40177    = "#f0c400",
  VCS_On        = "#1b9e77",
  Airport_1770  = "black"
)
df_all <- bind_rows(lapply(names(target_list), function(nm) {
  target_list[[nm]] %>%
    filter(Date >= as.Date("2000-01-01"), Date <= as.Date("2025-12-31")) %>% #Select a data size within the timeseries
    select(Date, all_of(vars)) %>%
    mutate(target = nm)
}))



df_long <- df_all %>%
  pivot_longer(cols = all_of(vars), names_to = "var", values_to = "value")

p_time <- ggplot(df_long, aes(x = Date, y = value, color = target)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ var, scales = "free_y", ncol = 1) +  # change ncol as you like
  scale_color_manual(values = target_colors) +
  labs(
    title = "Time series by variable across sites",
    x = "Date",
    y = "Value",
    color = "Site"
  ) +
  theme_bw()

print(p_time)

# Wind data alone -----Delete or clean up later-------

Wind_buoy <- ggplot(buoy, aes(x = Date, y = Wind_Spd_ms)) +
  geom_line(alpha = 0.5) +
  geom_smooth() +
  labs(
    title = "Buoy wind speed ms",
       x = "Date",
       y = "Wind speed ms") +
  theme_bw()


print(Wind_buoy)
  



# to do later --- add another script for seaons?? or maybe not use?
filter_season <- function(date, season = c("summer", "winter")) { ... }
# this will show something so we can see when it preforms best e.g (but not acurat would be) “ERA5 performs best in winter for wind, but buoy performs best during summer convective rainfall.”
