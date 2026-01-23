


#Install packages for following scripts
# scripts/000_project_setup.R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

renv::restore(prompt = FALSE)  # use the lockfile; idempotent

# from GitHub  (records to renv.lock)
renv::install("limnotrack/aemetools")
renv::install("rLakeAnalyzer")
renv::install("plotly")

renv::snapshot(prompt = FALSE, prune = TRUE)

# optional: check versions/status interactively
if (interactive()) renv::status()




