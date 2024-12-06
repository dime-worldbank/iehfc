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
          tags$style(HTML("
            .quote-box {
                background-color: #f3e6ff; /* Light purple background */
                border-left: 5px solid #9E83CF; /* Darker purple border */
                padding: 15px;
                margin-top: 15px;
                border-radius: 5px; /* Rounded corners */
                font-style: italic; /* Italicized text for quotes */
                color: #4b3d73; /* Complementary text color */
            }
        ")),
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
                .navbar-header {
                    max-width: 46px;
                    display: flex;
                    flex-direction: row;
                    align-items: left !important;
                    margin-right: 2rem;
                }

                .navbar-nav {
                    display: flex;
                    flex-wrap: nowrap !important;
                    flex-direction: row;
                    align-items: left !important;
                }

                .navbar-brand {
                    white-space: nowrap;
                    font-size: 1rem;
                    display: flex;
                    max-width: 50px;
                    align-items: left !important;
                }

                .navbar-nav .nav-link {
                    font-size: calc(min(1vw + 8px, 16px));
                    padding: 0.5rem 0.75rem;
                    white-space: nowrap;
                    max-width: 250px;
                    overflow: hidden;
                    text-overflow: ellipsis;
                }

                .navbar, .navbar-light, .bg-light {
                    background-color: #9e83cf !important;
                }

                @media (max-width: 780px) {
                    .navbar-header {
                        display: align-items;
                        flex-direction: column !important;
                        align-items: left !important;
                    }

                    .navbar-brand {
                        align-self: left;
                        margin-bottom: 1rem;
                        align-items: left !important;
                        margin-left: 2.5rem;
                    }

                    .navbar-nav {
                        display: flex;
                        flex-direction: column;
                        align-items: left;
                        min-width: 100%;
                    }

                    .navbar-nav .nav-item {
                        text-align: left;
                        align-items: left !important;
                        width: 100%;
                    }

                    .navbar-nav .nav-link {
                        align-items: left !important;
                        width: 100%;
                    }
                }
            "
            )
    )
)
