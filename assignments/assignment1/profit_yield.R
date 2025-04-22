
##' Almond Profits from Almond Yield
#'
#' @param yield A list from `almond_yield_anomaly_from_daily()` function
#' @param price_per_ton Almond price per ton in USD (default = 6000)
#' @param base_year The year to adjust profits to (default = 2010)
#' @param year_1 The year to adjust profits to (default = 1988)
#'
#' @return A named list with min, mean, and max inflation-adjusted profits

inflation_adjusted_profit <- function(yield,
                                      price_per_ton = 5000,
                                      base_year = 2010,
                                      year_1= 1988) {
  library(dplyr)
  library(readr)
  
  # Create a data frame for almond yields
  yield <- data.frame(
    year = as.numeric(names(yield$yield_anomalies)),  # Extract years
    almond_yield_anomalies = unname(yield$yield_anomalies)  # Get the values without the names
  )    
  
  # Add yearly profit to yield
  yield <- yield %>%
    mutate(profit = price_per_ton * almond_yield_anomalies)
}