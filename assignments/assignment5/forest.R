#' Forest Growth Function
#'
#' @param time time in years to run model
#' @param C initial forest size (Carbon (C))
#' @param parmas 
#'    K = 250 kg C (carrying capacity)
#'    r = 0.01 (exponential growth rate before canopy closure)
#'    g = 2 kg/year (linear growth rate after canopy closure)
#'  @param tcc threshold canopy closure
#'
#' @return slope of size over time 
#'
#' @examples

forest <- function(time, C, parms, tcc) {
  dC_dt <- ifelse(
    C < parms$tcc,
    parms$r * C,
    parms$g * (1 - C / parms$K)
    )
  return(list(dC_dt))
}
