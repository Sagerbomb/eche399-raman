# this function is used to calculate the SERS enhancement factor

SERS_EF <- function(drop_SERS, molarity_SERS, drop_Raman, molarity_Raman, diameter_SERS, diameter_Raman, I_SERS, I_Raman){
  # SERS
  avo <- 6.02214086*10^(23)
  drop_SERS <- drop_SERS / 1000000
  molecules_L <- avo * drop_SERS * molarity_SERS
  
  drop_area <- 0.01 * pi * (diameter_SERS / 2)^2
  N_SERS <- molecules_L / drop_area
  
  # Raman
  drop_Raman <- drop_Raman / 1000000
  molecules_L <- avo * drop_Raman * molarity_Raman
  
  drop_area <- 0.01 * pi * (diameter_Raman / 2)^2
  N_Raman <- molecules_L / drop_area
  
  
  # EF = I_SERS / N_SERS * N_Raman / I_Raman
  EF <- I_SERS / N_SERS * N_Raman / I_Raman
  enhancement <- list(EF = EF, power = floor(log10(EF)))
}