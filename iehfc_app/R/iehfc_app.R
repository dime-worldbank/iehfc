

  iehfc_app <- function() {
      
      shiny::addResourcePath(prefix = "res", directoryPath = normalizePath("/Users/worldbank/Documents/GitHub/iehfc/iehfc_app/www"))
      
      extract_source <- function(filename) {
          source(filename, local = parent.frame(), chdir = TRUE)$value
      }
      
      req_packages <- c(
          "pacman", "shiny", "dplyr", "tidyr", "purrr", "ggplot2", "janitor", "data.table", "DT",
          "remotes", "bsicons", "shinydashboard", "shinyjs", "markdown",
          "htmlwidgets", "plotly"
      )
      
      if(sum(req_packages %in% (.packages())) != length(req_packages)) { # Means we need to run global.R to get packages, otherwise we're fine
          extract_source("/Users/worldbank/Documents/GitHub/iehfc/iehfc_app/app.R")
      }
      
      shinyApp(
          ui = extract_source("/Users/worldbank/Documents/GitHub/iehfc/iehfc_app/iehfc_ui.R"),
          server = extract_source("/Users/worldbank/Documents/GitHub/iehfc/iehfc_app/iehfc_server.R")
      )
      
  }
  