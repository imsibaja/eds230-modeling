#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param low_flow_months which to use default (Febuary 2)
#' @return annual_max_err, annual_max_corr, low_month_cor, low_month_err

compute_highflowmetrics <- function(m, o, month, day, year, wy, high_flow_months = 2) {
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  # first lets get maximum yearly values

  tmp <- flow %>%
    group_by(wy) %>%
    summarize(maxo = max(o), maxm = max(m))

  annual_max_err <- mean(tmp$maxm - tmp$maxo)

  annual_max_cor <- cor(tmp$maxm, tmp$maxo)

  # now lets get monthly values
  tmp <- flow %>%
    group_by(month, year) %>%
    summarize(model = sum(m), obs = sum(o))
  # now extract august
  high <- subset(tmp, month %in% high_flow_months)
  high_month_err <- mean(high$model - high$obs)
  high_month_cor <- cor(high$model, high$obs)

  return(list(
    annual_max_err = annual_max_err, annual_max_cor = annual_max_cor, high_month_err = high_month_err,
    high_month_cor = high_month_cor
  ))
}
