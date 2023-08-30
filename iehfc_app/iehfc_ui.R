#########################################
#### User interface for IEHFC app ####
#########################################

# libraries

  library(shiny)
  library(bslib)
  library(DT)
  library(shinyjs)

# read source scripts

source("scripts/introduction_tab.R")


  
navbarPage(
      title = "iehfc",
      
      # Initialize shinyjs
      useShinyjs(),
      
      tabPanel(
          "Introduction",
          introduction_tab 
         # includeMarkdown("intro.Rmd")
      ),
      tabPanel(
          "Upload Data",
          id = "upload_tab",  # Give an ID for reference
          uiOutput("upload_tab")
      ),
      tabPanel(
          "Check Selection and Setup",
          id = "setup_tab",  # Give an ID for reference
          uiOutput("setup_tab")
      ),
      tabPanel(
          "Outputs",
          id = "output_tab",  # Give an ID for reference
          uiOutput("output_tab")
      ),
      nav_spacer(),
      nav_item(
          tags$a("Guides", href = "https://www.github.com")
      ),
      nav_item(
          tags$a("About", href = "https://www.github.com")
      ),
      nav_item(
          tags$a("Github", href = "https://www.github.com")
      ),
      theme = bs_theme() %>%
      bs_add_rules(
          sass::sass_file("www/custom.css")
      )
  )
  
  
  