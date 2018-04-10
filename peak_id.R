# this function is used to calculate the SERS enhancement factor

library(tidyverse)
library(stringr)

peak_id <- function(file, from = 200, to = 1800, save = FALSE){
  spectra <- read.delim(file, sep = "\t", header = FALSE)
  colnames(spectra) <- c("shift", "intensity")
  
  max_id <- which(spectra$shift > from & spectra$shift < to)[1] + which.max(spectra$intensity[which(spectra$shift > from & spectra$shift < to)]) - 1
  
  p <- spectra %>% 
    ggplot(aes(x = shift, y = intensity)) +
    geom_line() +
    geom_point(aes(x = shift[max_id], y = intensity[max_id]), color = "red") + 
    scale_x_continuous(breaks = seq(100, 2000, 100), minor_breaks = seq(150, 2000, 50)) + 
    scale_y_continuous(breaks = seq(0, max(spectra$intensity) + 250, 250), minor_breaks = seq(0, max(spectra$intensity) + 250, 250)) +
    labs(title = str_sub(file, 1, -5), x = expression(shift ~ cm^-1), y = "intensity (arbitrary, cps)")
  
  if (save == TRUE){
    ggsave(filename = paste0(str_sub(file, 1, -4), "png"), plot = p)
  }
  
  raman <- list(spectra = spectra, plot = p, shift_max = spectra$shift[max_id], intensity_max = spectra$intensity[max_id])
}

