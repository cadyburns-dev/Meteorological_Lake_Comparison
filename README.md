
LIVE SITE -> https://cadyburns-dev.github.io/Meteorological_Lake_Comparison/





Where to put raw files: data/raw/


What variables to expect: Temperature (C), Wind (ms), Radiation (Short wave Wm2), Precipitation (mm)


What scripts to run in order: Scripts numbered by order 00, 01, 02, 03, 04, 05


00_project_setup: install needed packages from renv.lock file (renv::restore or renv::snapshot to apply needed packages)


01_metrics_helpers: Setup metrics with functions for later use (statistical set up)


02_prepare_raw_data: Inport and allign raw data to match variable names (All Temp_C, Wind_Spd_ms, RadSWD_Wm2, Precip_mm), make sure dates are all alligned


03_analysis_helpers: Sets up functions. Computes metrics, adds all days, wet days and windy days thresholds codes for conditional preformance


04_analysis_plotting: Writes in plotting functions ready for quarto


05_Analysis_rotorua_plots: executable in R without quarto rendering as long as source is called out for 01,03 and 04. 

These are left un computable in scripts using a # for quarto render. Take # away for R specific plotting without quarto render

