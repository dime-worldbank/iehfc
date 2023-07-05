

  iehfc_app <- function() {
      
      # Needs to be able to refer to package files eventually. For now, going to have this work for me
      # inside of this R Project.
      
      extract_source <- function(filename) {
          source(filename, local = parent.frame(), chdir = TRUE)$value
      }
      
      extract_source("iehfc_app/global.R")
      
      shinyApp(
          ui = extract_source("iehfc_app/iehfc_ui.R"),
          server = extract_source("iehfc_app/iehfc_server.R")
      )
      
  }

  iehfc_app()  
  