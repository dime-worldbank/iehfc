

  library(shiny)
  library(bslib)
  
  parameter_sidebar <- layout_sidebar(
      sidebar = sidebar(
          width = "30%",
          card(
              fileInput(
                  "hfc_data",
                  label = "Upload HFC Data",
                  accept = "csv",
                  placeholder = "mydata.csv"
              )
          ),
          card(
              card_header("Data Quality Checks"),
              checkboxGroupInput(
                  "check_select", "Select High-Frequency Checks",
                  choiceNames = list(
                      "Duplicates", "Outliers", "Enumerator-Level", "Administrative Unit-Level",
                      "Unit of Observation-Level", "Survey Programming"
                  ),
                  choiceValues = list(
                      "duplicates", "outliers", "enumerator", "admin", "unit", "programming"
                  ),
                  selected = c("duplicates", "outliers")
              )
          )
      ),
      textOutput("selected_checks")
  )
  
  page_navbar(
      title = "iehfc",
      fillable = c("Upload Data", "Check Components"),
      nav_panel(
          "Introduction", "Hello introduction"
      ),
      nav_panel(
          "Upload Data",
          parameter_sidebar
      ),
      nav_panel(
          "iehfc Outputs",
          navset_tab(
              nav_panel("Duplicates", "Hello duplicates"),
              nav_panel("Outliers", "Hello outliers"),
              nav_panel("Enumerator", "Hello enumerator"),
              nav_panel("Admin Level", "Hello admin level"),
              nav_panel("Tracking", "Hello tracking"),
              nav_panel("Programming", "Hello programming")
          )
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
      theme = bs_theme(
          base_font = font_google("Atkinson Hyperlegible"),
          code_font = font_google("Fira Code")
      )
  )
  