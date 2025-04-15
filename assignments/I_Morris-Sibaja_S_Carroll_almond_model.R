#' Almond Yield Anomaly Model
#'
#' This function estimates the annual yield anomaly for California almond crops based on climate data.
#'
#' @param clim A data frame that contains climate data with the following columns at minimum:
#'  year: Numeric integer variable indicating the year.
#'  month: Numeric integer variable representing the month, beginning with January as 1.
#'  precip: Precipitation in millimeters.
#'  tmin_c: Minimum temperature in °C.
#'
#' @param parameters A list of variables containing the model parameters:
#'  c_T: Coefficient for the mean February minimum temperature.
#'  c_T2: Coefficient for the squared term of the mean February minimum temperature.
#'  c_P: Coefficient for the sum of January precipitation.
#'  c_P2: Coefficient for the squared term of the January precipitation sum.
#'  intercept: The model intercept which indicates the baseline yield anomaly.
#'
#' @return A data frame with one row and the following columns:
#'  max_yield: The maximum yield anomaly value.
#'  min_yield: The minimum yield anomaly value.
#'  mean_yield: The mean yield anomaly value.
#'
#' @example
#' # Read in climate data
#' clim_data <- read.table(here::here("assignments/assignment2/data/clim.txt"), header = TRUE)
#' 
#' # Assign almond parameters to variables
#' almond_params <- list(
#'   c_T = -0.015,
#'   c_T2 = -0.0046,
#'   c_P = -0.07,
#'   c_P2 = 0.0043,
#'   intercept = 0.28
#' )
#'
#' # Run function
#' almond_model(clim_data, almond_params)

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
