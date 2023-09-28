## iehfc Global script

# Purpose — Load packages and set parameters

  ## 1. Load Packages ----

  if(!require(pacman)) install.packages("pacman")

  pacman::p_load(
      shiny, dplyr, tidyr, stringr, lubridate, purrr, ggplot2, janitor, data.table, DT, remotes, bsicons,
      shinydashboard, shinyjs, markdown, htmlwidgets, webshot, plotly, bslib
  )

  # remotes::install_(repo = "rstudio/bslib", quiet = TRUE)
  # 
  # library(bslib, quietly = TRUE)

  ## 2. Load Custom Functions ----

# Reminder — Currently, this project works with an .Rproj file. Open the project directly from the .Rproj
# file using the "File — Open Project..." option on RStudio or directly by clicking on the .Rproj file in
# your file explorer. If you would rather not use the .Rproj file, you will need to set the working
# directory using `setwd(...)`, with `...` being the file path that leads you to the `iehfc` folder.
  
  shiny::addResourcePath(prefix = "res", directoryPath = "iehfc_app/www")


  source("R/iehfc_app.R")

  ## 3. Run Application ----

# If you want to launch the iehfc application, you just need to type in `iehfc_app()` in your console after having
# run the iehfc_app.R script as done in part 2. You can do this anytime.

  iehfc_app()
