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
      navbarPage(
          title = "IEHFC",
          
          # Initialize shinyjs
          
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
          navbarMenu("More",
                     nav_panel(tags$a("Guides", href = "https://github.com/dime-worldbank/iehfc/blob/main/README.md")),
                     # nav_panel(tags$a("About", href = "https://www.github.com")), # Under construction
                     nav_panel(tags$a("Github", href = "https://www.github.com/dime-worldbank/iehfc"))
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
    .nav-link, .nav-link.active, .navbar-brand, .navbar-brand:hover, .navbar-brand:focus, .navbar-brand:active, .dropdown-item, .dropdown-item:hover, .dropdown-item:focus, .dropdown-item:active {
        font-size: 18px;
        color: white;  /* Set text color to white for all states */
    }
    .nav-link:hover, .nav-link:focus, .nav-link.active, .dropdown-item:hover, .dropdown-item:focus, .dropdown-item:active {
        color: white !important;  /* Ensure hover, focus, and active states are also white */
        background-color: transparent !important; /* Removes any background color change on hover or active */
    }
    .dropdown-menu {
        background-color: #593196 !important; /* Ensure dropdown menu has a purple background */
    }
    .dropdown-menu > .active > .dropdown-item {
        background-color: #593196 !important; /* Keep the purple background for active items */
    }
    .navbar, .navbar-light, .bg-light {
        background-color: #593196 !important; /* Ensure the navbar has a purple background */
    }
    "
              )
          
      )
      
  )
  