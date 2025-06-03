mean_influx_loc <- function(conc_matrix){
  # obtain flux from df
  influx <- conc_matrix[["qin"]]
  
  # location are columns, obtain mean
  return(colMeans(influx))
}
