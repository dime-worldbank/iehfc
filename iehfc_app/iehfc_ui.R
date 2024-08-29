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
      tags$head(
          tags$script(HTML('
            $(document).ready(function() {
                $(".navbar .container-fluid .navbar-nav .dropdown .dropdown-menu").append(\'<li><a href="https://github.com/dime-worldbank/iehfc/blob/main/README.md" >Guides</a></li>\');
                $(".navbar .container-fluid .navbar-nav .dropdown .dropdown-menu").append(\'<li><a href="https://www.github.com/dime-worldbank/iehfc">Github</a></li>\');
            });
          '))),
      navbarPage(
          "IEHFC",
          id = "tabs",
          # Initialize shinyjs
          tabPanel(
              "Introduction",
              introduction_tab 
          ),
          tabPanel(
              "Upload Data",
              value = "upload_tab",  # Give an ID for reference
              uiOutput("upload_tab")
          ),
          tabPanel(
              "Check Selection and Setup",
              value = "setup_tab",  # Give an ID for reference
              uiOutput("setup_tab")
          ),
          tabPanel(
              "Outputs",
              value = "output_tab",  # Give an ID for reference
              uiOutput("output_tab")
              
          ),
          nav_menu("More"
          ),
          
          theme = bs_theme(
              base_font    = font_google("Atkinson Hyperlegible"),
              heading_font = font_google("Atkinson Hyperlegible"),
              code_font    = font_google("Fira Code"), 
              bootswatch = "pulse"
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
                  
                   .navbar, .navbar-light, .bg-light {
        background-color: #9e83cf !important;  /* Purple background for main navbar */
    }
                  "
              ) # Source: https://stackoverflow.com/questions/44112000/move-r-shiny-shownotification-to-center-of-screen
      )
      
  )
  