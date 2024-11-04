###############################################
############### INTRODUCTION TAB ###############
###############################################

library("shinydashboard")

  introduction_tab <- shinydashboard::tabItem(
      tabName = "introduction",
      div(style = "text-align:left", tags$h2("Welcome to IEHFC!")),
      fluidRow(
          column(width = 6,
              box(
                  width = "100%", status = "success",
                  shiny::img(src = "res/BULBS_purple.png", style = "width:220px;"),
                  shiny::img(src = "res/WB_logo.png", style = "max-width:100%; margin-bottom:20px;"),
              ),
              div(style = "text-align:left", tags$h3("The Need for IEHFC")),
              box(
                  width = "100%", status = "success",
                  p("IEHFC arose from the pressing need to standardize and simplify high-frequency data checks in international development research. It aims to bridge the gap between the established necessity of these checks and the complexity involved in implementing them."),
                  p(
                      "Developed by the team at",
                      tags$a("DIME Analytics", href = "https://www.worldbank.org/en/research/dime/data-and-analytics"),
                      ", IEHFC aims to be a one-stop solution for researchers, providing easy-to-create, customizable, and shareable high-frequency check outputs.",
                      "For more information about high-frequency checks you can consult", tags$a("our wiki resource.", href = "https://dimewiki.worldbank.org/High_Frequency_Checks")
                  ),
                  p(
                      "If you encounter any issues or have suggestions, please report them ", 
                      tags$a("on our GitHub issues section.", href = "https://github.com/dime-worldbank/iehfc/issues")
                  )
              )
          ),
          column(width = 6,
              div(style = "text-align:left", tags$h3("How IEHFC Works")),
              box(
                  width = "100%", status = "success",
                  p("IEHFC is designed to be an intuitive platform that streamlines the data checking process. It features a sequential tab design that guides users through data uploading, check selection, parameter setup, and viewing the results.")
              ),
              div(style = "text-align:left", tags$h3("Features")),
              box(
                  width = "100%", status = "success", style = "margin-bottom: 0px;",   # Reduce bottom margin here
                  p("IEHFC provides features to perform a comprehensive list of checks on your dataset, ranging from basic data integrity tests to advanced statistical checks."),
                  #       shiny::img(src = "res/features.png", style = "max-width:500px;"), # here if we want a visual after feature
              )
          )
      ),
      fluidRow(
          column(width = 6),
          column(width = 6,
              div(style = "text-align:left; margin-top: -190px;", tags$h3("Getting Started with IEHFC")),  # Reduce top margin here
              box(
                  width = "100%", status = "success",
                  "Using IEHFC is as simple as:",
                  tags$ul(
                      tags$li(a(href="#", "Uploading your dataset", onclick = "Shiny.onInputChange('gotoTab', 'Upload Data')")),
                      tags$li(a(href="#", "Selecting the desired checks", onclick = "Shiny.onInputChange('gotoTab', 'Check Selection and Setup')")),
                      tags$li(a(href="#", "Reviewing and exporting your results", onclick = "Shiny.onInputChange('gotoTab', 'Outputs')"))
                  )
              )
          )
      )
  )
