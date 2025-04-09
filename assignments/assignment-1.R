#     $E = A * r * H * PR$

# * E is energy (kWh),
# * A is the solar panel area (m2)
# * r is panel yield (0-1) (manufacture efficiency - usually around 0.2),
# * PR is performance ratio (0-1) (accounting for site factors that impact efficiency usually around 0.75) 
# * H is annual average solar radiation (kWh)

photovoltaic_energy <- function(A, r=.2, H, PR=.75){
  # check if Panel Yield is between 0 and 1
  if(r > 1 | r < 0){
    stop("Panel Yield (r) must be between 0 and 1 (Default = 0.2)")   
  # check if Performance Ratio is between 0 and 1
  }
  if(PR > 1 | PR < 0){
    stop("Performance Ratio (r) must be between 0 and 1 (Default = 0.75)")   
  }
  # return energy equation
  return (A * r * H * PR)
}