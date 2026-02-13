



# Gives clean function that returns all metrics in a tidy order
# 00_metrics_helpers.R
# Helper functions to compute performance metrics between two met data series
 


# 1. Summary metrics - statistical functions only
# 2. Bias metrics
# 3. Filtering metrics (heavy rain/high wind events)
# 4. Wrapping for all days, wet days and windy days

# 1.
#' Compute summary metrics between two numeric vectors

#' @param obs Numeric vector of "observed" or reference values (e.g. Airport)
#' @param sim Numeric vector of "simulated" or alternative source (e.g. ERA5)
#' @return A tibble with correlation, slope, intercept, MAE, RMSE, and concordance
calc_metrics <- function(obs, sim) {
  stopifnot(length(obs) == length(sim))
  
  # Remove pairs with NA
  df <- tibble(obs = obs, sim = sim) |>
    drop_na()
  
  if (nrow(df) == 0) {
    return(tibble(
      n          = 0L,
      cor        = NA_real_,
      slope      = NA_real_,
      intercept  = NA_real_,
      mae        = NA_real_,
      rmse       = NA_real_,
      ccc        = NA_real_
    ))
  }
  
  # Linear regression sim ~ obs
  fit <- lm(sim ~ obs, data = df)
  coefs <- coef(fit)
  
  # Metrics
  cor_val   <- cor(df$obs, df$sim)
  mae_val   <- mean(abs(df$sim - df$obs))
  rmse_val  <- sqrt(mean((df$sim - df$obs)^2))
  
  # Lin's concordance correlation coefficient
  mu_obs <- mean(df$obs)
  mu_sim <- mean(df$sim)
  s2_obs <- var(df$obs)
  s2_sim <- var(df$sim)
  s_obs  <- sd(df$obs)
  s_sim  <- sd(df$sim)
  
  ccc_val <- (2 * cor_val * s_obs * s_sim) /
    (s2_obs + s2_sim + (mu_obs - mu_sim)^2)
  
  tibble(
    n          = nrow(df),
    cor        = cor_val,
    slope      = unname(coefs[2]),
    intercept  = unname(coefs[1]),
    mae        = mae_val,
    rmse       = rmse_val,
    ccc        = ccc_val
  )
}

# 2.
# Seperates bias from correlation
#' Compute bias metrics between two numeric vectors
#'
#' @param obs Numeric vector of observed/reference values
#' @param sim Numeric vector of simulated values
#' @return Tibble with bias and relative bias (%) = % of mean observed 
calc_bias_metrics <- function(obs, sim) {
  
  df <- tibble(obs = obs, sim = sim) |>
    drop_na()
  
  if (nrow(df) == 0) {
    return(tibble(
      bias      = NA_real_,
      rel_bias  = NA_real_
    ))
  }
  
  bias_val <- mean(df$sim - df$obs)
  
  rel_bias_val <- if (mean(df$obs) == 0) {
    NA_real_
  } else {
    (bias_val / mean(df$obs)) * 100
  }
  
  tibble(
    bias     = bias_val,
    rel_bias = rel_bias_val
  )
}


# 3.
#' Compute metrics with optional filtering (e.g. wet days, windy days)
#'
#' @param obs Numeric vector of observed/reference values
#' @param sim Numeric vector of simulated values
#' @param filter Logical vector indicating which rows to keep
#' @param add_bias Logical; include bias metrics?
#' @return Tibble of metrics
calc_metrics_filtered <- function(obs, sim, filter, add_bias = TRUE) {
  
  obs_f <- obs[filter]
  sim_f <- sim[filter]
  
  base_metrics <- calc_metrics(obs_f, sim_f)
  
  if (!add_bias) {
    return(base_metrics)
  }
  
  bias_metrics <- calc_bias_metrics(obs_f, sim_f)
  
  bind_cols(base_metrics, bias_metrics)
}

# Run 1. (calculate summary metrics) after 3. ( filtering metrics )

# 4.
# --- All days
calc_metrics_all_days <- function(obs, sim) {
  calc_metrics_filtered(
    obs = obs,
    sim = sim,
    filter = rep(TRUE, length(obs))
  )
}

# --- Wet days
# Metrics for precipitation computed for wet days only (obs > 1 mm) to reduce the influence of zero-inflated dry periods.
# The wet day thresholds can be changed common = >0mm or >1mm
calc_metrics_wet_days <- function(obs, sim, threshold_mm = 1) {
  wet <- obs > threshold_mm
  calc_metrics_filtered(obs, sim, wet)
}


#--- Windy days
# Define windy based on reference data set (e.g airport)# typical = 8m/s (strong breeze;10m/s = near gale; or % based top = 1-%)??
calc_metrics_windy_days <- function(obs, sim, threshold_ms = 8) {
  windy <- obs >= threshold_ms
  calc_metrics_filtered(obs, sim, windy)
}




