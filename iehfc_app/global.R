## iehfc Global script

# Purpose — Load packages and set parameters

  ## 1. Load Packages ----
  
  if(!require(pacman)) install.packages("pacman")
  
  pacman::p_load(shiny, bslib, dplyr, tidyr, purrr, ggplot2, janitor, data.table, DT)
  