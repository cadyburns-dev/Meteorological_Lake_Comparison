


#Install packages for following scripts


# If you have all packages
renv::init() #if you have installed packages in your cache
renv::status()
renv::restore() # if you have installed packages in renv.lock 
renv::install("Rcpp") 


# For Buoy data
install.packages("rLakeAnalyzer")
devtools::install_github("limnotrack/aemetools")
aemetools::check_api_status()
logger::log_threshold(logger::INFO)


# For the rest
 install.packages(c(
  "tidyverse","lubridate","readxl","openxlsx","arrow","sf",
  "renv","devtools","logger",
  "quarto","rmarkdown","knitr","gt","broom","modelr","janitor",
  "testthat","lintr","styler", "readr", "cli", "ggplot2", "airGR", "zoo", "Rcpp"
))

