#########################################
#### User interface for IEHFC app ####
#########################################

# libraries

  library(shiny)
  library(bslib)
  library(DT)
  library(shinyjs)

# read source scripts

  source("ui_scripts/introduction_tab.R")

  fluidPage(
      useShinyjs(),
      useShinyFeedback(),
      page_navbar(
          title = "iehfc",
          nav_panel(
              "Introduction",
              introduction_tab 
          ),
          nav_panel(
              "Upload Data",
              id = "upload_tab",  # Give an ID for reference
              uiOutput("upload_tab")
          ),
          nav_panel(
              "Check Selection and Setup",
              id = "setup_tab",  # Give an ID for reference
              uiOutput("setup_tab")
          ),
          nav_panel(
              "Outputs",
              id = "output_tab",  # Give an ID for reference
              uiOutput("output_tab")
          ),
          nav_menu("More",
                     tabPanel(tags$a("Guides", href = "https://github.com/dime-worldbank/iehfc/blob/main/README.md")),
                     # tabPanel(tags$a("About", href = "https://www.github.com")), # Under construction
                     tabPanel(tags$a("Github", href = "https://www.github.com/dime-worldbank/iehfc"))
          ),
          theme = bs_theme(
              base_font    = font_google("Atkinson Hyperlegible"),
              heading_font = font_google("Atkinson Hyperlegible"),
              code_font    = font_google("Fira Code")
          ) %>%
              bs_add_rules(
                  "
                  h3 {
                      font-size: 24px;
                      font-weight: bold;
                  }

                  body {
                      font-size: 16px;
                  }
                  
                  .nav-link {
                      font-size: 18px;
                  }
                  
                  .shiny-notification {
                       position:fixed;
                       right: calc(65%);
                  }
                  "
              ) # Source: https://stackoverflow.com/questions/44112000/move-r-shiny-shownotification-to-center-of-screen
      )
      
  )
  