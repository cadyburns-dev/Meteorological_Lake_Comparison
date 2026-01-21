
# +++++++++++++++++++++========= new code below for buoy. addresses time step change in df where the data units change from knots to m/s aswell as non 5min data
# code below is alll fucked up and needs fixing so the wind is better =[



buoy5 <- Rotorua_Buoy_raw |>
  mutate(
    DateTime = as.character(DateTime),
    DateTime = if_else(
      str_detect(DateTime, "^\\d{4}-\\d{2}-\\d{2}$"),
      paste0(DateTime, " 00:00:00"),
      DateTime
    ),
    Date     = as_date(ymd_hms(DateTime, tz = "UTC")),
    Hour     = format(ymd_hms(DateTime, tz = "UTC"), "%H:00"),
    datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    Month    = months(Date),
    Year     = format(Date, "%Y"),
    MD       = make_MD(Date),
    WndSpd_ms = case_when(
      Date >= ymd("2022-10-05") & Date < ymd("2025-03-07") ~ WndSpd / 1.944, # knots -> m/s
      TRUE ~ WndSpd
    )) |>
  arrange(DateTime) %>%
  mutate(
    dt_min   = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    irregular = !is.na(dt_min) & dt_min != 5,  mutate(
      Wind_Spd_ms     = WndSpd / 1.944,          # knots → m/s
      PpRain_mm       = PpRain * (5 / 60)        # mm/hr over 5 min → mm depth
    )) |>  # flag non-5-min steps
  
  
  # Inspect interval distribution
  table(buoy5$dt_min, useNA = "ifany")

# Optionally drop/keep irregular rows; here we keep them and aggregate by hour
buoy_hourly <- buoy5 %>%
  mutate(
    hour = floor_date(DateTime, "hour")) %>%
  group_by(hour) %>%
  summarise(
    n_samples  = n(),
    any_irreg  = any(irregular, na.rm = TRUE),
    Temp_C     = mean(TmpAir, na.rm = TRUE),
    Precip_mm  = sum(PpRain, na.rm = TRUE),   # precip: sum over hour
    Wind_Spd_ms= mean(WndSpd, na.rm = TRUE),
    RadSWD_Wm2 = mean(RadSWD, na.rm = TRUE),
    .groups = "drop"
  )

# Then to daily:
buoy_daily <- buoy_hourly %>%
  mutate(Date = as_date(hour)) %>%
  group_by(Date) %>%
  summarise(
    n_hours      = n(),
    any_irreg    = any(any_irreg),
    Temp_C       = mean(Temp_C, na.rm = TRUE),
    Precip_mm    = sum(Precip_mm, na.rm = TRUE),
    Wind_Spd_ms  = mean(Wind_Spd_ms, na.rm = TRUE),
    RadSWD_Wm2   = mean(RadSWD_Wm2, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(buoy_daily)
# ==========+++++++++++++++++++ Above code could be used if working instead of the code below 
# original code below
buoy <- Rotorua_Buoy_raw |>
  mutate(
    DateTime = as.character(DateTime),
    DateTime = if_else(
      str_detect(DateTime, "^\\d{4}-\\d{2}-\\d{2}$"),
      paste0(DateTime, " 00:00:00"),
      DateTime
    ),
    Date     = as_date(ymd_hms(DateTime, tz = "UTC")),
    Hour     = format(ymd_hms(DateTime, tz = "UTC"), "%H:00"),
    datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    Month    = months(Date),
    Year     = format(Date, "%Y"),
    MD       = make_MD(Date)
  ) |>
  mutate(
    Wind_Spd_ms     = WndSpd / 1.944,          # knots → m/s
    PpRain_mm       = PpRain * (5 / 60)        # mm/hr over 5 min → mm depth
  ) |>
  group_by(datetime, Date, Hour, Month, Year, MD) |>
  summarise(
    PpRain = sum(PpRain_mm, na.rm = TRUE),
    across(
      c(RadSWD, TmpAir, HumRel, PrBaro, WndSpd, WndGst, WndDir, RadClr),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(Source = "Buoy") |>
  rename(
    Wind_Spd_ms   = WndSpd,
    Precip_mm     = PpRain,
    Temp_C        = TmpAir,
    Baro_hPa      = PrBaro,
    RadSWD_Wm2    = RadSWD
  )
head(buoy) 
summary(buoy)


wind_unit_plot <- ggplot(buoy, aes(x = datetime, y = Wind_Spd_ms)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Buoy wind speed (hourly mean, m/s)",
    subtitle = "Dashed lines mark assumed unit-change window (knots -> m/s)",
    x = "Time (UTC)",
    y = "Wind speed (m/s)"
  ) +
  theme_minimal()
print(wind_unit_plot)


#########new below slightly changes from code above to include the time change in units
# 5min to hourly to daily



knot_to_ms  <- 0.514444
switch_date <- ymd("2023-11-01")  # knots before this date, m/s on/after

# --- Parse, flag intervals, and convert wind to m/s consistently ---
buoy <- Rotorua_Buoy_raw |>
  mutate(
    DateTime = as.character(DateTime),
    DateTime = if_else(str_detect(DateTime, "^\\d{4}-\\d{2}-\\d{2}$"),
                       paste0(DateTime, " 00:00:00"), DateTime),
    DateTime = ymd_hms(DateTime, tz = "UTC"),
    Date     = as_date(DateTime),
    Hour     = format(DateTime, "%H:00"),
    # time-step check
    dt_min   = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    irregular= !is.na(dt_min) & dt_min != 5,
    
    # Wind: knots -> m/s before switch_date; leave as-is after
    Wind_Spd_ms = if_else(Date < switch_date, WndSpd * knot_to_ms, WndSpd),
    Wind_Gst_ms = if_else(Date < switch_date, WndGst * knot_to_ms, WndGst),
    
    # Precip: mm/hr over 5 min -> mm depth
    PpRain_mm = PpRain * (5 / 60)
  )

# Optional: quick look at interval distribution
table(buoy$dt_min, useNA = "ifany")

# --- Hourly aggregation (keeps irregular hours; you can filter(!irregular) if desired) ---
buoy_hourly <- buoy |>
  mutate(hour = floor_date(DateTime, "hour")) |>
  group_by(hour) |>
  summarise(
    n_samples    = n(),
    any_irreg    = any(irregular, na.rm = TRUE),
    Precip_mm    = sum(PpRain_mm, na.rm = TRUE),
    Temp_mean    = mean(TmpAir, na.rm = TRUE),
    Temp_min     = suppressWarnings(min(TmpAir, na.rm = TRUE)),
    Temp_max     = suppressWarnings(max(TmpAir, na.rm = TRUE)),
    HumRel       = mean(HumRel, na.rm = TRUE),
    Baro_hPa     = mean(PrBaro, na.rm = TRUE),
    RadSWD_Wm2   = mean(RadSWD, na.rm = TRUE),
    Wind_Spd_ms  = mean(Wind_Spd_ms, na.rm = TRUE),
    Wind_Gst_ms  = mean(Wind_Gst_ms, na.rm = TRUE),
    WndDir       = mean(WndDir, na.rm = TRUE),
    RadClr       = mean(RadClr, na.rm = TRUE),
    .groups = "drop"
  )

# --- Daily aggregation from hourly ---
buoy_daily <- buoy_hourly |>
  mutate(Date = as_date(hour)) |>
  group_by(Date) |>
  summarise(
    n_hours     = n(),
    any_irreg   = any(any_irreg),
    Temp_mean   = mean(Temp_mean, na.rm = TRUE),
    Temp_min    = suppressWarnings(min(Temp_mean, na.rm = TRUE)),
    Temp_max    = suppressWarnings(max(Temp_mean, na.rm = TRUE)),
    Temp_C      = coalesce(Temp_mean, (Temp_min + Temp_max) / 2),
    Precip_mm   = sum(Precip_mm, na.rm = TRUE),
    Wind_Spd_ms = mean(Wind_Spd_ms, na.rm = TRUE),
    HumRel      = mean(HumRel, na.rm = TRUE),
    Baro_hPa    = mean(Baro_hPa, na.rm = TRUE),
    RadSWD_Wm2  = mean(RadSWD_Wm2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(Date, Temp_C, Precip_mm, Wind_Spd_ms, RadSWD_Wm2, n_hours, any_irreg)

# Optional: data-driven breakpoint finder (daily medians, look for ~2x jumps)
# meds <- Rotorua_Buoy_raw |>
#   mutate(Date = as_date(ymd_hms(DateTime, tz = "UTC"))) |>
#  group_by(Date) |>
#  summarise(med = median(WndSpd, na.rm = TRUE), .groups = "drop") |>
#  arrange(Date) |>
#  mutate(ratio = med / lag(med))
# meds |> filter(ratio > 1.8 | ratio < 0.6)  # candidate switch dates


##### above is in place of the code to see if it works, will come back and decide out of the three above works the best

















# BELoW old to delete??? metrics for analysis helpers






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
era5    <- read_csv("data/processed/rotorua_era5_daily.csv", show_col_types = FALSE)
buoy    <- read_csv("data/processed/rotorua_buoy_daily.csv", show_col_types = FALSE)
ap1770  <- read_csv("data/processed/rotorua_airport_1770_daily.csv", show_col_types = FALSE)
twn40177 <- read_csv("data/processed/rotorua_town_40177_daily.csv", show_col_types = FALSE)
vcs_on <- read_csv("data/processed/rotorua_vcs_on_daily.csv", show_col_types = FALSE)

# Vars to compare (must exist in both ref and target)
vars <- c("Temp_C", "Precip_mm", "Wind_Spd_ms", "RadSWD_Wm2")

# Helper: compute metrics for one target vs reference (1770)
metrics_vs_ref <- function(target_df, target_name) {
  joined <- inner_join(ap1770, target_df, by = "Date", suffix = c("_ref", "_tgt"))
  
  map_dfr(vars, function(v) {
    obs <- joined[[paste0(v, "_ref")]]
    sim <- joined[[paste0(v, "_tgt")]]
    calc_metrics(obs, sim) |>
      mutate(var = v, .before = 1)
  }) |>
    mutate(target = target_name, .before = 1)
}

metrics_all <- bind_rows(
  metrics_vs_ref(era5,   "ERA5"),
  metrics_vs_ref(buoy,   "Buoy"),
  metrics_vs_ref(twn40177,"Town_40177"),
  metrics_vs_ref(vcs_on,  "VCS_On")
)

# View metrics table
metrics_all


# this blow code (3 lines of code is to be tested to get filtered metrics for high wind and rain days)
# It should return n,cor,slope,intercept,mae,rmse,ccc,bias,rel_bias
# Example: ERA5 vs Airport for precipitation where ap1770$precip_mm = refernce "obs" series ear5$precip_mm = dataset we are evaluating "sim"
# mertrics_all uses every day where both datasets have values (after NA removal) (baseline overall preformance)
# metrics_wet uses only days where the reference has “meaningful rain”:if threshold_mm = 1, it keeps days where ap1770$Precip_mm > 1.
# This answers: “How well does ERA5 match rainy days?”(which usually matters more for modelling than dry days).
# metrics_windy  uses only days where the reference indictes high wind :if threshold_ms = 10, it keeps days where ap1770$Wind_Spd_ms >=10.
# This answers: “How well does ERA5 match wind-storm days?”
# matters because correlation may look high but preform poorly in seasonal extremes, wind and rain heavy events === (findinf out what the best met source is and when)
metrics_all   <- calc_metrics_all_days(ap1770$Precip_mm, era5$Precip_mm)   
metrics_wet   <- calc_metrics_wet_days(ap1770$Precip_mm, era5$Precip_mm, threshold_mm = 1) # threshold changes = test 0.1,1 and 5)
metrics_windy <- calc_metrics_windy_days(ap1770$Wind_Spd_ms, era5$Wind_Spd_ms, threshold_ms = 10) #used percentage (top 10% observed winds) to be lake transferable



#----------------------------------------
#1. Metrics table
#----------------------------------------
# note to add to corr plot locatrion# “Correlation for precipitation can be influenced by many zero-rain days; we also evaluate performance on wet days (e.g., Precip > 0 or > 1 mm).”
# # note to do after 1. 1.1?? Compute metrics twice - all days and then wet days only (e.g. obs > 0.1mm)

# tidy-table metrics_vs_ref that compares each target dataset to a reference dataset
# returns one table with columns:target, var, subset, n, cor, slope, intercept, mae, rmse, ccc, bias, rel_bias
# computes:all days for every variable,wet days for precipitation (threshold configurable),windy days for wind using either:
# a fixed threshold (e.g. 10 m/s), or
#top X% of observed winds (recommended)
# It uses your new helper function 00_metrics_helpers

# vars you want to evaluate
vars <- c("Temp_C", "Precip_mm", "Wind_Spd_ms", "RadSWD_Wm2")

# Compute tidy metrics table for one target vs reference
metrics_vs_ref <- function(ref_df,
                           target_df,
                           target_name,
                           vars = Null,
                           wet_threshold_mm = 1,
                           windy_threshold_ms = 10,
                           windy_top_pct = NULL) {
  
  joined <- inner_join(ref_df, target_df, by = "Date", suffix = c("_ref", "_tgt"))
  
  # helper to safely pull vectors
  get_pair <- function(v) {
    list(
      obs = joined[[paste0(v, "_ref")]],
      sim = joined[[paste0(v, "_tgt")]]
    )
  }
  
  # --- decide windy filter rule (based on OBSERVED / reference wind) ---
  # If windy_top_pct is provided (e.g., 0.10), it overrides windy_threshold_ms.
  
  windy_filter <- function(obs_wind) {
    if (!is.null(windy_top_pct)) {
      stopifnot(windy_top_pct > 0 && windy_top_pct < 1)
      thr <- as.numeric(quantile(obs_wind, probs = 1 - windy_top_pct, na.rm = TRUE, type = 7))
      obs_wind >= thr
    } else {
      obs_wind >= windy_threshold_ms
    }
  }
  
  # Build results row-by-row
  out <- map_dfr(vars, function(v) {
    
    pair <- get_pair(v)
    obs <- pair$obs
    sim <- pair$sim
    
    # All days (always)
    res_all <- calc_metrics_all_days(obs, sim) |>
      mutate(var = v, subset = "all_days", .before = 1)
    
    # Wet days (precip only)
    res_wet <- tibble()
    if (v == "Precip_mm") {
      res_wet <- calc_metrics_wet_days(obs, sim, threshold_mm = wet_threshold_mm) |>
        mutate(var = v, subset = paste0("wet_days_obs>", wet_threshold_mm, "mm"), .before = 1)
    }
    
    # Windy days (wind only)
    # Windy days (wind only)
    res_windy <- tibble()
    if (v == "Wind_Spd_ms") {
      
      thr <- if (!is.null(windy_top_pct)) {
        as.numeric(quantile(obs, probs = 1 - windy_top_pct, na.rm = TRUE, type = 7))
      } else {
        windy_threshold_ms
      }
      
      filt <- obs >= thr
      
      res_windy <- calc_metrics_filtered(obs, sim, filter = filt, add_bias = TRUE) |>
        mutate(
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
    
    bind_rows(res_all, res_wet, res_windy)
  }) |>
    mutate(target = target_name, .before = 1)
  
  out
}



# This threshold is computed per comparison (per target vs reference and per overlap period).;
# That’s usually what you want, because the overlap period can differ across targets. 
# If a single global threshold shared across all targets is required, compute once from the reference dataset and pass it in — but don’t do that unless you explicitly want it.


# windy_top_pct = 0.10 switches the windy-day definition from “≥ 10 m/s” to “top 10% of observed winds”
# if windy_top_pct is not NULL it calcs a threshold e.g. thr <- quantile(obs_wind, probs = 1 - windy_top_pct, na.rm = TRUE)
# For windy_top_pct = 0.10, its the 90th percentile of the reference wind (obs_wind). then keeps obs_wind >= thr so windy days = days where reference wind is in the tp 10%

metrics_era5 <- metrics_vs_ref(ap1770, era5, "ERA5", wet_threshold_mm = 1, windy_top_pct = 0.10)
metrics_buoy <- metrics_vs_ref(ap1770, buoy, "Buoy", wet_threshold_mm = 1, windy_top_pct = 0.10)
metrics_vcsn <- metrics_vs_ref(ap1770, vcs_on, "vcs_on", wet_threshold_mm = 1, windy_top_pct = 0.10 )
metrics_all <- bind_rows(metrics_era5, metrics_buoy, metrics_vcsn)
metrics_all


