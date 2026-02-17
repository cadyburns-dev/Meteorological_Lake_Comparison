
# --- Source helpers (must exist relative to project root) ---
source("scripts/01_metrics_helpers.R")
source("scripts/03_analysis_helpers.R")
source("scripts/04_analysis_plotting.R")

print(metrics_all)

# ============================================================
#
# ============================================================

print(plot_timeseries_line(ref_df, targets_list, "Temp_C"))
print(plot_timeseries_line(ref_df, targets_list, "Wind_Spd_ms"))
print(plot_timeseries_line(ref_df, targets_list, "RadSWD_Wm2"))
print(plot_timeseries_line(ref_df, targets_list, "Precip_mm"))
# only plot timeseries is enough data avalable
# p_ts_rad  <- plot_timeseries_line(ref_df, targets_list, "RadSWD_Wm2")


plot_precip_hydrograph(ref_df, targets_list, display = "bars")
plot_precip_hydrograph(ref_df, targets_list, display = "rolling", window_days = 7)  #7day totals for rain


print(plot_precip_cdf(ref_df, targets_list))
print(plot_precip_wetday_distribution(ref_df, targets_list, threshold_mm = 1))
print(plot_distribution(ref_df, targets_list, "Precip_mm"))

print(plot_scatter_faceted(ref_df, targets_list, "Temp_C", ncol = 2))
print(plot_scatter_faceted(ref_df, targets_list, "Wind_Spd_ms", ncol = 2))
print(plot_scatter_faceted(ref_df, targets_list, "Precip_mm", ncol = 2))
#print(plot_scatter_faceted(ref_df, targets_list, "RadSWD_Wm2", ncol = 2))

# Rolling (30 day) plots

print(plot_rolling_diagnostics(ref_df, era5, "ERA5", "Temp_C", window_days = 30))
print(plot_rolling_diagnostics(ref_df, buoy, "Buoy", "Temp_C", window_days = 30))
print(plot_rolling_diagnostics(ref_df, twn40177, "Town_40177", "Temp_C", window_days = 30))
print(plot_rolling_diagnostics(ref_df, vcs_on, "VCS_On", "Temp_C", window_days = 30))

print(plot_rolling_diagnostics(ref_df, era5, "ERA5", "Wind_Spd_ms", window_days = 30))
print(plot_rolling_diagnostics(ref_df, buoy, "Buoy", "Wind_Spd_ms", window_days = 30))
print(plot_rolling_diagnostics(ref_df, twn40177, "Town_40177", "Wind_Spd_ms", window_days = 30))
print(plot_rolling_diagnostics(ref_df, vcs_on, "VCS_On", "Wind_Spd_ms", window_days = 30))

print(plot_rolling_diagnostics(ref_df, era5, "ERA5", "Precip_mm", window_days = 30))
print(plot_rolling_diagnostics(ref_df, buoy, "Buoy", "Precip_mm", window_days = 30))
print(plot_rolling_diagnostics(ref_df, twn40177, "Town_40177", "Precip_mm", window_days = 30))
print(plot_rolling_diagnostics(ref_df, vcs_on, "VCS_On", "Precip_mm", window_days = 30))


print(skill_precip)
print(plot_event_outcomes(skill_precip,
  title = paste0("Precip events (≥ ", precip_event_threshold, " mm/day): outcomes vs reference")
))
print(plot_event_skill_scores(skill_precip,
  title = paste0("Precip events (≥ ", precip_event_threshold, " mm/day): skill vs reference")
))
print(plot_false_alarm_magnitude(ref_df, targets_list, threshold_mm = precip_event_threshold))


#Climatology
print(plot_monthly_climatology(ref_df, targets_list, "Temp_C"))
print(plot_monthly_climatology(ref_df, targets_list, "Wind_Spd_ms"))
print(plot_monthly_climatology(ref_df, targets_list, "Precip_mm"))
# Radiation only if you have decent overlap:
# print(plot_monthly_climatology(ref_df, targets_list, "RadSWD_Wm2"))

# --- pick one season definition ---
season_defs <- season_defs_default
# season_defs <- season_defs_warmcool  # <- switch to warm/cool quickly

# Seasonal bias plots
print(plot_seasonal_bias(ref_df, targets_list, "Temp_C", season_defs = season_defs))
print(plot_seasonal_bias(ref_df, targets_list, "Wind_Spd_ms", season_defs = season_defs))
print(plot_seasonal_bias(ref_df, targets_list, "Precip_mm", season_defs = season_defs))

# Seasonal metrics tables (useful to print in Quarto later)
season_metrics_wind <- seasonal_metrics_all_targets(ref_df, targets_list, "Wind_Spd_ms", season_defs = season_defs)
season_metrics_precip <- seasonal_metrics_all_targets(ref_df, targets_list, "Precip_mm", season_defs = season_defs, wet_threshold_mm = precip_event_threshold)

print(season_metrics_wind)
print(season_metrics_precip)

# Seasonal scatter (big figure, but very informative)
print(plot_seasonal_scatter(ref_df, targets_list, "Wind_Spd_ms", season_defs = season_defs))
print(plot_seasonal_scatter(ref_df, targets_list, "Precip_mm", season_defs = season_defs))  # can be heavy; optional
