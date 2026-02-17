
# Load metric_helper_functions
# run source below if not using quarto file
source("scripts/01_metrics_helpers.R")
#stopifnot(file.exists("scripts/01_metrics_helpers.R"))



# calc_metrics is used inside metrics_vs_ref.
# Ensure vars match the column names in each dataset; adjust if your any column is named differently across met data.
# If any dataset lacks a variable, remove it from vars or handle with tryCatch.


#      AIM Read processed data, comute metrics, make plots

#----------------------------------------
# Make saving function for figs
#----------------------------------------
# helper to build figure path and ensure the folder exists

path_fig <- function(...) {
  dir.create(file.path("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
  file.path("outputs", "figures", ...)
}

save_fig <- function(plot_obj, filename, width = 8, height = 5, dpi = 150) {
  ggsave(
    filename = path_fig(filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}


#----------------------------------------
# Load processed daily data
#----------------------------------------
# when coding with different data sets change the csv file to the new created files
era5    <- read_csv("data/processed/rotorua_era5_daily.csv", show_col_types = FALSE)
buoy    <- read_csv("data/processed/rotorua_buoy_daily.csv", show_col_types = FALSE)
ap1770  <- read_csv("data/processed/rotorua_airport_1770_daily.csv", show_col_types = FALSE)
twn40177 <- read_csv("data/processed/rotorua_town_40177_daily.csv", show_col_types = FALSE)
vcs_on <- read_csv("data/processed/rotorua_vcs_on_daily.csv", show_col_types = FALSE)

# Assign files and make sure date aligns
era5$Date <- as.Date(era5$Date)
buoy$Date <- as.Date(buoy$Date)
ap1770$Date <- as.Date(ap1770$Date)
twn40177$Date <- as.Date(twn40177$Date)
vcs_on$Date <- as.Date(vcs_on$Date)



# force Date to date so joins dont fail - avoids timezone drift, POSIXct vs date mismatch 
datasets <- list(era5, buoy, ap1770, twn40177, vcs_on)

datasets <- lapply(datasets, function(df) {
  df$Date <- as.Date(df$Date)
  df
})

era5     <- datasets[[1]]
buoy     <- datasets[[2]]
ap1770   <- datasets[[3]]
twn40177 <- datasets[[4]]
vcs_on   <- datasets[[5]]




# this blow code (3 lines of code is to be tested to get filtered metrics for high wind and rain days)
# It should return n,cor,slope,intercept,mae,rmse,ccc,bias,rel_bias
# Example: ERA5 vs Airport for precipitation where ap1770$precip_mm = refernce "obs" series ear5$precip_mm = dataset we are evaluating "sim"
# mertrics_all uses every day where both datasets have values (after NA removal) (baseline overall preformance)
# metrics_wet uses only days where the reference has “meaningful rain”:if threshold_mm = 1, it keeps days where ap1770$Precip_mm > 1.
# This answers: “How well does ERA5 match rainy days?”(which usually matters more for modelling than dry days).
# metrics_windy  uses only days where the reference indictes high wind :if threshold_ms = 10, it keeps days where ap1770$Wind_Spd_ms >=10.
# This answers: “How well does ERA5 match wind-storm days?”
# matters because correlation may look high but preform poorly in seasonal extremes, wind and rain heavy events === (findinf out what the best met source is and when)
# example below
#   metrics_all   <- calc_metrics_all_days(ap1770$Precip_mm, era5$Precip_mm)   
# metrics_wet   <- calc_metrics_wet_days(ap1770$Precip_mm, era5$Precip_mm, threshold_mm = 1) # threshold changes = test 1 and 5)
#   metrics_windy <- calc_metrics_windy_days(ap1770$Wind_Spd_ms, era5$Wind_Spd_ms, threshold_ms = 10) #used percentage (top 10% observed winds) to be lake transferable



#----------------------------------------
#1. Metrics table
#----------------------------------------
# note to add to corr plot locatrion# “Correlation for precipitation can be influenced by many zero-rain days; we also evaluate performance on wet days (e.g., Precip > 0 or > 1 mm).”
#  Compute metrics twice - all days and then wet days only (e.g. obs > 1mm)

# table metrics_vs_ref that compares each target dataset to a reference dataset
# returns one table with columns:target, var, subset, n, cor, slope, intercept, mae, rmse, ccc, bias, rel_bias
# computes:all days for every variable,wet days for precipitation (threshold configurable),windy days for wind using either:
# a fixed threshold (e.g. 10 m/s), or
#top X% of observed winds (What is used in rotorua demo)
# It uses helper function 00_metrics_helpers



# compute metrics table for one target vs referecnce (e.g. Airport)
metrics_vs_ref <- function(ref_df,
                           target_df,
                           target_name,
                           vars = NULL,
                           wet_threshold_mm = 1,
                           windy_threshold_ms = 10,
                           windy_top_pct = NULL) {
  
  if (is.null(vars)) {
    vars <- c("Temp_C", "Precip_mm", "Wind_Spd_ms", "RadSWD_Wm2")
  }
  
  joined <- inner_join(ref_df, target_df, by = "Date", suffix = c("_ref", "_tgt"))
  
  #helper to safety pull vectors
  get_pair <- function(v) {
    list(
      obs = joined[[paste0(v, "_ref")]],
      sim = joined[[paste0(v, "_tgt")]]
    )
  }
  
  #row by row results
  out <- purrr::map_dfr(vars, function(v) {
    
    pair <- get_pair(v)
    obs <- pair$obs
    sim <- pair$sim
    
    # all days always
    res_all <- calc_metrics_all_days(obs, sim) |>
      dplyr::mutate(var = v, subset = "all_days", .before = 1)
    
    # Wet days (precip only)
    res_wet <- tibble::tibble()
    if (v == "Precip_mm") {
      res_wet <- calc_metrics_wet_days(obs, sim, threshold_mm = wet_threshold_mm) |>
        dplyr::mutate(var = v, subset = paste0("wet_days_obs>", wet_threshold_mm, "mm"), .before = 1)
    }
    
    #windy days (wind only)
    res_windy <- tibble::tibble()
    if (v == "Wind_Spd_ms") {
      
      thr <- if (!is.null(windy_top_pct)) {
        as.numeric(stats::quantile(obs, probs = 1 - windy_top_pct, na.rm = TRUE, type = 7))
      } else {
        windy_threshold_ms
      }
      
      filt <- obs >= thr
      
      res_windy <- calc_metrics_filtered(obs, sim, filter = filt, add_bias = TRUE) |>
        dplyr::mutate(
          wind_threshold = thr,
          var = v,
          subset = if (!is.null(windy_top_pct)) {
            paste0("windy_top_", windy_top_pct * 100, "%_obs")
          } else {
            paste0("windy_obs>=", windy_threshold_ms, "ms")
          },
          .before = 1
        )
    }
#res_all = baseline preformance across all overlapping days 
#res_wet = Conditional performance on hydrologically meaningful precipitation events (avoids zero-inflation bias) and
#res_wind = Conditional performance during dynamically important wind regimes (either fixed threshold or top-X%)
    dplyr::bind_rows(res_all, res_wet, res_windy) 
  }) |>
    dplyr::mutate(target = target_name, .before = 1)
  
  out
}

# This threshold is computed per comparison (per target vs reference and per overlap period).;
# The overlap period can differ across targets. 
# If a single global threshold shared across all targets is required, compute once from the reference dataset and pass it in — but don’t do that unless you explicitly want it.


# windy_top_pct = 0.10 switches the windy-day definition from “≥ 10 m/s” to “top 10% of observed winds”
# if windy_top_pct is not NULL it calcs a threshold e.g. thr <- quantile(obs_wind, probs = 1 - windy_top_pct, na.rm = TRUE)
# For windy_top_pct = 0.10, its the 90th percentile of the reference wind (obs_wind). then keeps obs_wind >= thr so windy days = days where reference wind is in the tp 10%

#What “top 10% of obs” does -It finds a cutoff value so that only the windiest 10% of reference days are kept.

#How that cutoff (threshold) is made
# Code uses the reference wind series (obs) and calculates the 90th percentile:
#90% of observed days are below this value
#10% of observed days are at or above this value
#That percentile value is stored as wind_threshold.
#e.g for target (sim) vs refrence (obs) wind_threshold = 5.78 (threshold defind from the reference then applied to select dates for both series)
# the 90th-percentile observed wind is 5.78 m/s, So “windy day” is defined as:
#keep day if obs >= 5.78
#drop day if obs < 5.78    Then metrics are computed using:
#obs (Airport wind) on those kept days
#sim (Buoy wind) on those same days
#So for windy_top_10%_obs, Buoy is being judged only during days when the reference says it was in its windiest 10%.


metrics_era5 <- metrics_vs_ref(ap1770, era5, "ERA5", wet_threshold_mm = 1, windy_top_pct = 0.10)
metrics_buoy <- metrics_vs_ref(ap1770, buoy, "Buoy", wet_threshold_mm = 1, windy_top_pct = 0.10)
metrics_vcsn <- metrics_vs_ref(ap1770, vcs_on, "VCS_On", wet_threshold_mm = 1, windy_top_pct = 0.10 )
metrics_all <- bind_rows(metrics_era5, metrics_buoy, metrics_vcsn)


# View(metrics_all)

#Note radiation only found in two datasets, and has very little points for this reason it is not used
#Add extra code for RAD if yu have the nessesary data for analysis






