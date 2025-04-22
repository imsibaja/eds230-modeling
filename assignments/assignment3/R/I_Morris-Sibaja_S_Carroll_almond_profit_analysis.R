#' Comprehensive Almond Profit Analysis
#'
#' Performs a complete almond profit analysis including yield modeling, profit calculation,
#' sensitivity analysis, and visualization. This function integrates multiple analysis steps
#' into a single workflow.
#'
#' @param clim_data A data frame containing climate data with at minimum columns for year and 
#'        climate variables required by the almond model.
#' @param almond_params A list of parameters for the almond yield model.
#' @param n_samples Integer. Number of parameter combinations to generate for sensitivity analysis.
#'        Default is 100.
#'
#' @return A list with two elements:
#'   {data}{A data frame containing the complete results of the sensitivity analysis,
#'               including yield and profit metrics for each parameter combination and year.}
#'   {plots}{A list containing two ggplot objects:
#'     {
#'       {sample_series_plot}{Box plot showing uncertainty in profits across years.}
#'       {water_cost_plot}{Box plot showing profit distributions by water cost categories.}
#'     }
#'   }

almond_profit_analysis <- function(clim_data, almond_params, n_samples = 100) {
  # Turn off scientific notation
  options(scipen = 999)
  
  # Define base profit parameters
  base_price <- 5000            # $/ton
  base_labor_costs <- 3000      # $/acre
  water_rate <- 2               # $ cost multiplier per inverse mm precipitation
  baseline_yield <- 1000        # tons/acre
  
  # Parameter variations for sensitivity analysis
  deviation_price <- 0.15       # 15%
  deviation_labor <- 0.15       # 15%
  
  # Generate parameter samples
  sample_price <- runif(
    n = n_samples,
    min = base_price - (deviation_price * base_price),
    max = base_price + (deviation_price * base_price)
  )
  
  sample_labor <- runif(
    n = n_samples,
    min = base_labor_costs - (deviation_labor * base_labor_costs),
    max = base_labor_costs + (deviation_labor * base_labor_costs)
  )
  
  sample_base_year <- sample(1989:2010, n_samples, replace = TRUE)
  
  # Combine parameters into a tibble
  params <- tibble::tibble(
    price_per_ton = sample_price, 
    labor_costs = sample_labor,
    base_year = sample_base_year
  )
  
  # Nested function: Almond yield model
  almond_model <- function(clim_data, almond_params) {
    
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
    almond_variables <- merge(almond_precipitation_jan, almond_minTemp_feb, 
                              by = "year")
    
    # Apply the yield anomaly equation using the parameters
    almond_variables <- almond_variables %>%
      mutate(
        yield_anomaly =
          # Temperature potion of equation
          almond_params$c_T * minTemp_mean_feb + almond_params$c_T2 * minTemp_mean_feb^2 +
          # Precipitation portion of equation
          almond_params$c_P * precipitation_sum_jan + almond_params$c_P2 * precipitation_sum_jan^2 +
          # Model intercept
          almond_params$intercept
      )
    return(almond_variables)
  }
  
  # Nested function: Profit model
  profit_model <- function(yield_anomaly_df, price_per_ton, labor_costs_per_acre, 
                           water_rate, baseline_yield, base_year) {
    # Calculate profit
    profit_df <- yield_anomaly_df %>%
      mutate(
        # Calculate actual yield
        actual_yield = baseline_yield + yield_anomaly,
        # Calculate revenue
        revenue = actual_yield * price_per_ton,
        # Calculate costs
        labor_costs = labor_costs_per_acre,
        water_costs = water_rate / (precipitation_sum_jan + 1),
        # Calculate profit before adjustment
        profit_per_acre = revenue - labor_costs - water_costs,
        # Estimate inflation
        year_factor = 1 + 0.02 * (year - base_year),  # 2% change per year inflation
        # Adjust profit based on the year
        adjusted_profit = profit_per_acre * year_factor,
        # Store the parameter values used for this run
        price_used = price_per_ton,
        labor_costs_used = labor_costs_per_acre,
        base_year_used = base_year
      )
    return(profit_df)
  }
  
  # Nested function: Combined yield and profit model
  almond_yield_profit_model <- function(clim_data, almond_params, price_per_ton, labor_costs_per_acre, water_rate, baseline_yield, base_year) {
    # Get yield anomalies
    yield_anomaly <- almond_model(clim_data, almond_params)
    
    # Calculate profits
    profit_results <- profit_model(
      yield_anomaly, 
      price_per_ton, 
      labor_costs_per_acre, 
      water_rate, 
      baseline_yield, 
      base_year
    )
    
    return(profit_results)
  }
  
  # Run model for each parameter combination
  profit_model_results_table <- params %>%
    pmap(function(price_per_ton, labor_costs, base_year) {
      almond_yield_profit_model(
        clim_data = clim_data,
        almond_params = almond_params,
        price_per_ton = price_per_ton,
        labor_costs_per_acre = labor_costs,
        water_rate = water_rate,
        baseline_yield = baseline_yield,
        base_year = base_year
      )
    })
  
  # Combine all results into a single data frame
  sample_series <- bind_rows(profit_model_results_table)
  
  # Create sensitivity plots
  
  # Plot 1: Uncertainty in profits through time
  plot1 <- ggplot(sample_series, aes(as.factor(year), adjusted_profit)) +
    geom_boxplot() +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), 2)]) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(x = "Year",
         y = "Adjusted Profit (US$/acre)",
         title = "Uncertainty in Annual Almond Profits") +
    theme_minimal()
  
  # Plot 2: How profit varies with changing water costs
  plot2 <- sample_series %>%
    # Bin points
    mutate(cost_bin = cut(
      water_costs,
      breaks = 5,
      labels = c("Very Low", "Low", "Middle", "High", "Very High")
    )) %>%
    # Plot
    ggplot(aes(x = cost_bin, y = adjusted_profit)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(x = "Water Cost Index",
         y = "Adjusted Profit (US$/acre)",
         title = "Profit Distributions by Water Cost Bin") +
    theme_minimal()
  
  # Return results as a list
  return(list(
    data = head(sample_series),
    plots = list(sample_series_plot = plot1, water_cost_plot = plot2)
  ))
}