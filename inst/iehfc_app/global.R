## iehfc Global script

# Purpose — Load packages and set parameters

  ## 1. Load Packages ----

  # Define required packages
  needed_packages <- c(
    "shiny", "dplyr", "tidyr", "stringr", "lubridate", "purrr", "ggplot2", "janitor",
    "data.table", "DT", "remotes", "bsicons", "shinydashboard", "shinyjs", "markdown",
    "htmlwidgets", "webshot", "plotly", "bslib", "kableExtra"
  )

  # Install missing packages
  to_install <- needed_packages[!(needed_packages %in% installed.packages()[, "Package"])]
  if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

  # Load packages
  suppressPackageStartupMessages(lapply(needed_packages, library, character.only = TRUE))


  ## 2. Load Custom Functions ----

# Reminder — Currently, this project works with an .Rproj file. Open the project directfly from the .Rproj
# file using the "File — Open Project..." option on RStudio or directly by clicking on the .Rproj file in
# your file explorer. If you would rather not use the .Rproj file, you will need to set the working
# directory using `setwd(...)`, with `...` being the file path that leads you to the `iehfc` folder.

  if (dir.exists("iehfc_app/www")) {
    shiny::addResourcePath(prefix = "res", directoryPath = "iehfc_app/www")
  } else {
    warning("www/ directory not found. Some assets may not load properly.")
  }

  source("R/iehfc_app.R")

  ## 3. Run Application ----

# If you want to launch the iehfc application, you just need to type in `iehfc_app()` in your console after having
# run the iehfc_app.R script as done in part 2. You can do this anytime.

  iehfc_app()
