library(tidyverse)
library(here)

almond_model <- function(clim, parameters) {
  
  # Isolate and extract sum January precipitation
  almond_precipitation_jan <- clim_data %>%
    filter(month == 1) %>%
    group_by(year) %>%
    summarise(precipitation_sum_jan = sum(precip, na.rm = TRUE))
  
  # Isolate and extract mean February minimum temperature
  almond_minTemp_feb <- clim_data %>%
    filter(month == 2) %>%
    group_by(year) %>%
    summarise(minTemp_mean_feb = mean(tmin_c, na.rm = TRUE))
  
  # Merge precipitation and temperature
  almond_variables <- merge(almond_precipitation_jan, almond_minTemp_feb, by =
                              "year")
  
  # Apply the yield anomaly equation using the parameters
  almond_variables <- almond_variables %>%
    mutate(
      yield_anomaly = 
        # Temperature potion of equation
        parameters$c_T * minTemp_mean_feb + parameters$c_T2 * minTemp_mean_feb^2 +
        # Precipitation portion of equation
        parameters$c_P * precipitation_sum_jan + parameters$c_P2 * precipitation_sum_jan^2 +
        # Model intercept
        parameters$intercept
    )
  
  # Summarize key statistics
  almond_variables_summary <- almond_variables %>%
    summarize(
      max_yield = max(yield_anomaly, na.rm = TRUE),
      min_yield = min(yield_anomaly, na.rm = TRUE), 
      mean_yield = mean(yield_anomaly, na.rm = TRUE)
    )
  
  return(almond_variables_summary)
}