
# creator Cady B 2025/2026
# Created using R version version.string R version 4.4.1 (2024-06-14 ucrt) 



# Script that follows  "00_project_setup" and  "01_metrics_helpers" and precedes "03_analysis_helpers" and "04_analysis_plotting"

# If you have not gone through  00_project_setup or 01_metrics_helpers do so before this script



# Aligns to common date time
# Output = Aggregated daily Meteorological variables for Temp, Wind, Rain and Radiation 
# Met data from; limnotrack buoy, NIWA Virtual climate station (VCSN), Airport, Cliflo NIWA met network
 


#---------------------------------------------- Install packages or retrieve them if not installed -----------------------------------------------

  # AIM : Clean/process data, align date and units; Rotorua met sources: ERA5, Buoy, Airport, VCS.

#-------------------------------------------------------------------------------
  # load library
#1.-------------------------------------------------------------------------------

library(aemetools)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(airGR)
library(purrr)

#-------------------------------------------------------------------------------
  # Make functions - files all from inside data/raw
#2. ------------------------------------------------------------------------------
path_raw <- function(...) file.path("data", "raw", ...)

# Date function to handle any NA in data
make_MD <- function(date) {
  if_else(
    is.na(date),
    as.Date(NA),
    as.Date(paste0("2000-", format(date, "%m-%d")))
  )
}
# the next function can be done closer to the time needed or here
# Function to summarise hourly data to daily mean   
summarise_hourly_mean <- function(df, date_col, value_col) {
  df %>%
    group_by({{ date_col }}) %>%
    summarise(
      mean_value = mean({{ value_col }}, na.rm = TRUE),
      .groups = "drop"
    )
}

# makeing wind function as buoy units changed mid dataset (note = visualize this and get unit changes dates for diff lakes)
wind_knots_conv <- function(df, Wind_Speed_ms, value_col) 
wind_ms_conv <- function(df, Wind_Speed_ms, value_col)

#-------------------------------------------------------------------------------  
  # ERA5 data (daily)
#3.-------------------------------------------------------------------------------

# Lat and long of lake (This is Rotorua)
lat <- -38.114908

lon <- 176.316662

vars <- c(
  "MET_tmpair",   # air temperature degC
  "MET_pprain",   # rainfall
  "MET_wndspd",   # wind speed m/s
  "Met_wnddir",  # wind direction degrees
  "MET_radswd",   # shortwave radiation W/m2
  "MET_radlwd",    # longwave radiation W/m2
  "MET_tmpdew",   # dew point temperature degC
  "MET_prmsl",    # mean sea level pressure Pa
  "MET_humrel",   # relative humidity %
  "MET_prvap",    # vapor pressure hPa
  "MET_prsttn",   # surface pressure Pa – can be derived from mean sea level pressure
  "MET_wnduvu",   # u (eastward) wind component m/s
  "MET_wnduvv"    # v (northward) wind component m/s
)

years_vec <- 1990:2025 # Can change to length desired (has historical limit in NZ) 
chunks <- split(years_vec, years_vec)  # One year each; or define bigger blocks

get_chunk <- function(year_chunk) {
  get_era5_land_point_nz(
    lat   = lat,
    lon   = lon,
    years = year_chunk,
    vars  = vars, 
    api_key = "lernzmp_lakes"
  )
}


Rua_met_era5 <- map(chunks, get_chunk) |> list_rbind() #this will take a moment to process



Rua_met_era5 |> 
  dplyr::filter(Date > "2024-09-21", Date < "2024-10-02") # Even though the data is only a year it will give you more years

summary(Rua_met_era5)
glimpse(Rua_met_era5)

# Calculate derived variables
deriv_met <- AEME::expand_met(
  met = Rua_met_era5,
  lat = lat,
  lon = lon, elev = 280
)

summary(deriv_met)

write.csv(deriv_met, file = "data/raw/Rotorua_ERA5_1990_2025.csv", row.names = FALSE) 

glimpse(deriv_met)

#Use a raw file and change it to match all proceesing data below


Era5 <- read_csv(path_raw("Rotorua_ERA5_1990_2025.csv"), show_col_types = FALSE) |>
  mutate(
    Date       = as_date(ymd(Date, tz = "UTC")), #parse date
    Baro_hPa   = MET_prsttn / 100, #adjust pressure; Use MET_prsttn if that is your col
    MD         = make_MD(Date),
    Source     = "Era5") |>
  rename(
    Precip_mm     = MET_pprain,
    Temp_C        = MET_tmpair,
    Wind_Spd_ms   = MET_wndspd,
    RadSWD_Wm2    = MET_radswd,
    HumRel        = MET_humrel
  )

glimpse(Era5)
# ------------------------------------------------------------------------------
  # Buoy data (5-min → hourly → daily)
#4.------------------------------------------------------------------------------

#Get Bouy data from Limnotrack database - This is from the new Buoy so starts Feb 2022
Rotorua_Buoy_raw <- read.csv(file = "https://www.dropbox.com/scl/fi/s37tbex1uej3r1xfzxkrd/Rotorua_202202-202508_meteorology.csv?rlkey=9oee6fzwx3q9yc7ubiobn9flc&st=obtfridy&dl=1", header = TRUE)

aemetools::check_api_status()
logger::log_threshold(logger::INFO)


# +++++++++++++++++++++++++++++++++
# Alternatively, if downloaded manually, read from data/raw

#download_dir <- "data/raw"
#if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
#Rotorua_Buoy_raw <- read_csv(path_raw("Rotorua_202202-202508_meteorology.csv"), show_col_types = FALSE)

# +++++++++++++++++++++++++++++++++

glimpse(Rotorua_Buoy_raw)
summary(Rotorua_Buoy_raw)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   #   Buoy wind unit check
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Rotorua and other buoys may have wind unit change 
# original code below does not account for the unit change. 
# When you get a new lake apply this code below and visualize the wind. if you see an obvious drop or rise then knots and ms could be used in the same time set

# buoy <- Rotorua_Buoy_raw |>
#  mutate(
#   DateTime = as.character(DateTime),
#   DateTime = if_else(str_detect(DateTime, "^\\d{4}-\\d{2}-\\d{2}$"),
#   paste0(DateTime, "00:00:00"),
#   DateTime
#   ),
#   Date     = as_date(ymd_hms(DateTime, tz = "UTC")),
#   Hour     = format(ymd_hms(DateTime, tz = "UTC"), "%H:00"),
#   datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
#   Month    = months(Date),
#   Year     = format(Date, "%Y"),
#   MD       = make_MD(Date)
# ) |>
# mutate(
#   Wind_Spd_ms     = WndSpd / 1.944,          # knots → m/s
#   PpRain_mm       = PpRain * (5 / 60)        # mm/hr over 5 min → mm depth
#  ) |>
#  group_by(datetime, Date, Hour, Month, Year, MD) |>
#  summarise(
#    PpRain = sum(PpRain_mm, na.rm = TRUE),
#   across(
#    c(RadSWD, TmpAir, HumRel, PrBaro, WndSpd, WndGst, WndDir, RadClr),
#    ~ mean(.x, na.rm = TRUE)
#  ),
#    .groups = "drop"
#  ) |>
#  mutate(Source = "Buoy") |>
#  rename(
#   Wind_Spd_ms   = WndSpd,
#   Precip_mm     = PpRain,
#   Temp_C        = TmpAir,
#    Baro_hPa      = PrBaro,
#    RadSWD_Wm2    = RadSWD
#  )
#head(buoy) 
#summary(buoy)


#wind_unit_plot <- ggplot(buoy, aes(x = datetime, y = Wind_Spd_ms)) +
#  geom_line(color = "steelblue", alpha = 0.7) +
#labs(
#  title = "Buoy wind speed (hourly mean, m/s)",
#  subtitle = "Dashed lines mark assumed unit-change window (knots -> m/s)",
#   x = "Time (UTC)",
#  y = "Wind speed (m/s)"
#) +
#theme_minimal()
#print(wind_unit_plot)

# If you see a time step then use the live code and change the "switch_date" to the year it changes from knots to ms

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# --- Wind conversion


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


# Visulise it 
wind_unit_plot <- ggplot(buoy, aes(x = DateTime, y = Wind_Spd_ms)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Buoy wind speed (hourly mean, m/s)",
    subtitle = "Dashed lines mark assumed unit-change window (knots -> m/s)",
    x = "Time (UTC)",
    y = "Wind speed (m/s)"
  ) +
  theme_minimal()
print(wind_unit_plot)


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
  ) |> select(Date, Temp_C, Precip_mm, Wind_Spd_ms, RadSWD_Wm2)
head(buoy_daily)
summary(buoy_daily)



# -------------------------------------------------------------------
# Rotorua town 40177 (near Rotorua Airport)  2015-2025
#5.-------------------------------------------------------------------


# Download data from NIWA Cliflo manually and save as CSV in "data/raw" folder
# https://cliflo.niwa.co.nz/

# Seperate files need to be joined together - radiation, temperature, rainfall, wind speed - 


# ----------- Daily Radiation 
#Rua_41077_RAD_daily <- read_csv(
#  path_raw("Rotorua_town_41077", "41077__Radiation__Global__daily.csv"),
#  show_col_types = FALSE
#) |>
#  rename(
#    Datetime_txt = `Observation time UTC`,
#    Rad_MJm2     = `Radiation [MJ/m2]`
#  ) |>
#  mutate(
#    Datetime = parse_date_time(Datetime_txt, orders = c("ymd_HMS", "ymd_HM", "ymd"), tz = "UTC"),
#    Date       = as_date(Datetime),
#    Hour       = format(Datetime, "%H:00"),
 #   datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
 #   Month    = months(Date),
 #   Year     = format(Date, "%Y"),  
#    MD       = make_MD(Date),
#    RadSWD_Wm2 = Rad_MJm2 * 1e6 / 86400 # MJ/m2 per day → W/m2
#  ) |>
#  filter(!is.na(Date))
#Rua_41077_RAD_daily <- select(Rua_41077_RAD_daily, Date, RadSWD_Wm2)
#head(Rua_41077_RAD_daily)

#tail(Rua_41077_RAD_daily)


# ---- Hourly radiation


twn_41077_rad_hourly <- read_csv(
  path_raw("Rotorua_town_41077", "41077__Radiation__Global__hourly.csv"),
  show_col_types = FALSE
) |>
  rename(
    Datetime_txt = `Observation time UTC`,
    Rad_MJm2     = `Radiation [MJ/m2]`
  ) |>
  mutate(
    Datetime = parse_date_time(Datetime_txt, orders = c("ymd_HMS", "ymd_HM", "ymd"), tz = "UTC"),
    Date       = as_date(Datetime),
    Hour       = format(Datetime, "%H:00"),
    datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    Month    = months(Date),
    Year     = format(Date, "%Y"),  
    MD       = make_MD(Date),
    RadSWD_Wm2 = Rad_MJm2 * 1e6 / 3600 # MJ/m2 per hour → W/m2
  ) |>
  filter(!is.na(Date))

head(twn_41077_rad_hourly)
tail(twn_41077_rad_hourly)
TWN_41077_RAD_daily <- summarise_hourly_mean(twn_41077_rad_hourly, Date, RadSWD_Wm2) |>  #Converting the daily mean values using the function made at start of file
  rename(RadSWD_Wm2 = mean_value)  #name the mean values radiation
head(TWN_41077_RAD_daily)
tail(TWN_41077_RAD_daily)



# ---------- daily temperature/humidity - if you cant get hourly use th code below

#Rua_41077_TEMP_daily <- read_csv(
 # path_raw("Rotorua_town_41077", "41077__Temperature__daily.csv"),
#  show_col_types = FALSE
#) |>
#  rename(
 #   Datetime_txt    = `Observation time UTC`,
 #   Temp_max_C      = `Maximum Temperature [Deg C]`,
 #   Temp_min_C      = `Minimum Temperature [Deg C]`,
 #   Temp_C     = `Mean Temperature [Deg C]`,
 #   HumRel_percent  = `Mean Relative Humidity [percent]`
 # ) |>
 # mutate(
 #   Datetime = parse_date_time(Datetime_txt, orders = c("ymd_HMS", "ymd_HM", "ymd"), tz = "UTC"),
 #   Date     = as_date(Datetime),
 #   Hour     = format(Datetime, "%H:00"),
 #   Temp_C   = coalesce(Temp_C, (Temp_max_C + Temp_min_C) / 2),  # Average if mean not available
 #   datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
 #   Month    = months(Date),
 #   Year     = format(Date, "%Y"),
 #   MD       = make_MD(Date)
 # ) |>
 # filter(!is.na(Date))
#Rua_41077_TEMP_daily <- select(Rua_41077_TEMP_daily, Date, Temp_C)
#head(Rua_41077_TEMP_daily)  
#summary(Rua_41077_TEMP_daily)  



# ---- Hourly temperature
# Best practice to use hourly for temp and aggregae to daily so all data aligns more accuratly

Rua_41077_TEMP_hourly <- read_csv(
  path_raw("Rotorua_town_41077", "41077__Temperature__hourly.csv"),
  show_col_types = FALSE
) |>
  rename(
    Datetime_txt    = `Observation time UTC`,
    Temp_max_C      = `Maximum Temperature [Deg C]`,
    Temp_min_C      = `Minimum Temperature [Deg C]`,
    Temp_mean_C     = `Mean Temperature [Deg C]`,
    HumRel_percent  = `Mean Relative Humidity [percent]`
  ) |>
  mutate(
    Datetime = parse_date_time(Datetime_txt, orders = c("ymd_HMS", "ymd_HM", "ymd"), tz = "UTC"),
    Date     = as_date(Datetime),
    Hour     = format(Datetime, "%H:00"),
    Temp_C   = coalesce(Temp_mean_C, (Temp_max_C + Temp_min_C) / 2),  # Average if mean not available
    datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    Month    = months(Date),
    Year     = format(Date, "%Y"),
    MD       = make_MD(Date)
  ) |>
  filter(!is.na(Date)) 
glimpse(Rua_41077_TEMP_hourly)  


# ------------- Daily rain 

Rua_41077_rain_daily <- read_csv(
  path_raw("Rotorua_town_41077", "41077__Rain__daily.csv"),
  show_col_types = FALSE
) |>
  transmute(
    Date      = as_date(ymd_hms(`Observation time UTC`, tz = "UTC")),
    Precip_mm = `Rainfall [mm]`
  )
head(Rua_41077_rain_daily)
tail(Rua_41077_rain_daily)
summary(Rua_41077_rain_daily)



# ---- Daily wind

Rua_41077_WIND_daily <- read_csv(
  path_raw("Rotorua_town_41077", "41077__Wind__daily.csv"),
  show_col_types = FALSE
) |>
  transmute(
    Date       = as_date(ymd_hms(`Observation time UTC`, tz = "UTC")),
    Wind_Spd_ms  = `Speed [m/s]`,
    WndDir     = `Direction [deg T]`
  )
Rua_41077_WIND_daily <- select(Rua_41077_WIND_daily, Date, Wind_Spd_ms)
head(Rua_41077_WIND_daily)
tail(Rua_41077_WIND_daily)



# ---- Aggregate hourly -> daily 
TWN_41077_RAD_daily <- TWN_41077_RAD_daily |>
  group_by(Date) |>
  summarise(RadSWD_mean = mean(RadSWD_Wm2, na.rm = TRUE), .groups = "drop")

Rua_41077_TEMP_hourly <- Rua_41077_TEMP_hourly |>
  group_by(Date) |>
  summarise(
    Temp_mean  = mean(Temp_C, na.rm = TRUE),
    Temp_min   = if (all(is.na(Temp_C))) NA_real_ else min(Temp_C, na.rm = TRUE),
    Temp_max   = if (all(is.na(Temp_C))) NA_real_ else max(Temp_C, na.rm = TRUE),
    HumRel_mean = mean(HumRel_percent, na.rm = TRUE),
    n_hours    = sum(!is.na(Temp_C)),
    .groups = "drop"
  ) |>
  rename(Temp_C = Temp_mean)



# ---- Combine into one daily table

Rua_41077_daily <- Rua_41077_TEMP_hourly |>
  left_join(TWN_41077_RAD_daily,  by = "Date") |>
  left_join(Rua_41077_rain_daily, by = "Date") |>
  left_join(Rua_41077_WIND_daily, by = "Date")

glimpse(Rua_41077_daily)



# -------------------------------------------------------------------
# Airport 1770 (near airport Rotorua)
#6.-------------------------------------------------------------------




# Download data from NIWA Cliflo manually and save as CSV in data/raw folder
# https://cliflo.niwa.co.nz/
# Seperae files need to be joined together - temperature, radiation, rainfall, wind speed - this is done in following code
# Read in Airport 1770 data (Radiaton in MJ/m2, Temp in DegC, Rainfall in mm, Wind Speed in m/s)

# files can be downloaded on daily or hourly basis - the following code assumes hourly files for RAD and temp are downloaded
# If daily files are downloaded the code will need to be adjusted accordingly

# ---- Hourly radiation

ap_1770_rad_hourly <- read_csv(
  path_raw("Rotorua_airport_1770", "1770__Radiation__Global__hourly.csv"),
  show_col_types = FALSE
) |>
  rename(
    Datetime_txt = `Observation time UTC`,
    Rad_MJm2     = `Radiation [MJ/m2]`
  ) |>
  mutate(
    Datetime = parse_date_time(Datetime_txt, orders = c("ymd_HMS", "ymd_HM", "ymd"), tz = "UTC"),
    Date       = as_date(Datetime),
    Hour       = format(Datetime, "%H:00"),
    datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    Month    = months(Date),
    Year     = format(Date, "%Y"),  
    MD       = make_MD(Date),
    RadSWD_Wm2 = Rad_MJm2 * 1e6 / 3600 # MJ/m2 per hour → W/m2
  ) |>
  filter(!is.na(Date))

head(ap_1770_rad_hourly)
tail(ap_1770_rad_hourly)
AP_1770_RAD_daily <- summarise_hourly_mean(ap_1770_rad_hourly, Date, RadSWD_Wm2) |>  #Converting the daily mean values using the function made at start of file
  rename(RadSWD_Wm2 = mean_value)  #name the mean values radiation
head(AP_1770_RAD_daily)
tail(AP_1770_RAD_daily)


# ---- Hourly temperature/humidity
ap_1770_temp_hourly <- read_csv(
  path_raw("Rotorua_airport_1770", "1770__Temperature__hourly.csv"),
  show_col_types = FALSE
) |>
  rename(
    Datetime_txt    = `Observation time UTC`,
    Temp_max_C      = `Maximum Temperature [Deg C]`,
    Temp_min_C      = `Minimum Temperature [Deg C]`,
    Temp_mean_C     = `Mean Temperature [Deg C]`,
    HumRel_percent  = `Mean Relative Humidity [percent]`
  ) |>
  mutate(
    Datetime = parse_date_time(Datetime_txt, orders = c("ymd_HMS", "ymd_HM", "ymd"), tz = "UTC"),
    Date     = as_date(Datetime),
    Hour     = format(Datetime, "%H:00"),
    Temp_C   = coalesce(Temp_mean_C, (Temp_max_C + Temp_min_C) / 2),  # Average if mean not available
    datetime = as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    Month    = months(Date),
    Year     = format(Date, "%Y"),
    MD       = make_MD(Date)
  ) |>
  filter(!is.na(Date)) 
glimpse(ap_1770_temp_hourly)  

# ---- Daily rain 
ap_1770_rain_daily <- read_csv(
  path_raw("Rotorua_airport_1770", "1770__Rain__daily.csv"),
  show_col_types = FALSE
) |>
  transmute(
    Date      = as_date(ymd_hms(`Observation time UTC`, tz = "UTC")),
    Precip_mm = `Rainfall [mm]`
  )
head(ap_1770_rain_daily)
tail(ap_1770_rain_daily)

# ---- Daily wind 
ap_1770_wind_daily <- read_csv(
  path_raw("Rotorua_airport_1770", "1770__Wind__daily.csv"),
  show_col_types = FALSE
) |>
  transmute(
    Date       = as_date(ymd_hms(`Observation time UTC`, tz = "UTC")),
    Wind_Spd_ms  = `Speed [m/s]`,
    WndDir     = `Direction [deg T]`
  )
head(ap_1770_wind_daily)
tail(ap_1770_wind_daily)

# ---- Aggregate hourly -> daily 
ap_1770_rad_daily <- ap_1770_rad_hourly |>
  group_by(Date) |>
  summarise(RadSWD_mean = mean(RadSWD_Wm2, na.rm = TRUE), .groups = "drop")

ap_1770_temp_daily <- ap_1770_temp_hourly |>
  group_by(Date) |>
  summarise(
    Temp_mean  = mean(Temp_C, na.rm = TRUE),
    Temp_min   = if (all(is.na(Temp_C))) NA_real_ else min(Temp_C, na.rm = TRUE),
    Temp_max   = if (all(is.na(Temp_C))) NA_real_ else max(Temp_C, na.rm = TRUE),
    HumRel_mean = mean(HumRel_percent, na.rm = TRUE),
    n_hours    = sum(!is.na(Temp_C)),
    .groups = "drop"
  ) |>
  rename(Temp_C = Temp_mean)

# ---- Combine into one daily table
ap_1770_daily <- ap_1770_temp_daily |>
  left_join(AP_1770_RAD_daily,  by = "Date") |>
  left_join(ap_1770_rain_daily, by = "Date") |>
  left_join(ap_1770_wind_daily, by = "Date")


glimpse(ap_1770_daily)






# -------------------------------------------------------------------
# Virtual Climate Station – On Rotorua (daily)
#7.-------------------------------------------------------------------

# To download data from NIWA VCSN manually and save as CSV in data/raw folder (note this is not free access)
# https://data.niwa.co.nz/products/vcsn-timeseries/map?bounds=176.20439529418948%2C-38.12645478095495%2C176.38017654418948%2C-38.04863179448705 

vcs_on_raw <- read_csv(path_raw("On_Rotorua.csv"), show_col_types = FALSE)

vcs_on <- vcs_on_raw |>
  mutate(
    Date     = dmy(Date),
    Month = months(Date),
    Year  = format(Date, "%Y"),
    MD    = make_MD(Date)
  ) |>
  mutate(across(
    c(Tmax, Tmin, WindSpeed, MSLPress, Radiation),  # Only character columns
    ~ na_if(.x, "null")
  )) |>
  mutate(across(
    c(Tmax, Tmin, WindSpeed, Rain, MSLPress, Radiation),
    as.numeric
  )) |>
  mutate(
    Temp_C = (Tmax + Tmin) / 2,
    RadSWD = Radiation * 1e6 / 86400,  # MJ/m2 per day → W/m2
    Source = "VCS - 27879 - On Rotorua"
  ) |>
  rename(
    Wind_Spd_ms = WindSpeed,
    Precip_mm     = Rain,
    Baro_hPa      = MSLPress,
    RadSWD_Wm2    = RadSWD
  )
(!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}


#8. ---- Write to file processed ---------
write_csv(Era5,     "data/processed/rotorua_era5_daily.csv")
write_csv(buoy_daily,     "data/processed/rotorua_buoy_daily.csv")
write_csv(Rua_41077_daily, "data/processed/rotorua_town_40177_daily.csv")
write_csv(ap_1770_daily,  "data/processed/rotorua_airport_1770_daily.csv")
write_csv(vcs_on,   "data/processed/rotorua_vcs_on_daily.csv")

message("Saved cleaned Rotorua met datasets to data/processed/")



#######################################################
#########################################################


# -------------------------------------------------------------------
# ---- Join onto common datetime DF -------
# -------------------------------------------------------------------

Met_alll <- full_join(era5_daily, buoy_daily, by = "Date", suffix = c("_ERA5", "_Buoy")) %>%
  full_join(ap_40177_daily, by = "Date", suffix = c("", "_40177")) %>%
  full_join(ap_1770_daily, by = "Date", suffix = c("", "_1770"))

# Optional: filter to a common period
# met_all <- met_all |>
#   filter(datetime >= ymd("2000-01-01"),
#          datetime <= ymd("2024-12-31"))

# ---- 3. Save a processed version for analysis & Quarto ----
write_csv(Met_alll, "data/processed/rotorua_met_all_daily.csv")

message("Saved combined Rotorua met dataset to data/processed/rotorua_met_all_daily.csv")