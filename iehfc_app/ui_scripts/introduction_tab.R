###############################################
############### INTRODUCTION TAB ###############
###############################################

library(shinydashboard)

introduction_tab <- shinydashboard::tabItem(
    tabName = "introduction",
    
    fluidRow(
        column(
            width = 6,
            div(
                style = "display: flex; align-items: center; justify-content: flex-start;",
                div(style = "text-align:left;", tags$h1("Welcome to IEHFC!")
            )
        )
        ),
        column(
            width = 6,
            style = "display: flex; justify-content: flex-end; align-items: flex-end;",
            shiny::img(src = "BULBS_purple.png", style = "width:180px; margin-right: 20px;"),
            
            )
    ),
    
    div(style = "margin: 10px 0;", hr()),
    
    
    fluidRow(
        column(
            width = 6,
            div(style = "text-align:left", tags$h3("The Need for IEHFC")),
            box(
                width = 12, status = "success",
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
            ),
            shiny::img(src = "WB_logo.png", style = "width: 200px; margin-top: 20px;")
        ),
        column(
            width = 6,
            div(style = "text-align:left", tags$h3("How IEHFC Works")),
            box(
                width = 12, status = "success",
                p("IEHFC is designed to be an intuitive platform that streamlines the data checking process. It features a sequential tab design that guides users through data uploading, check selection, parameter setup, and viewing the results.")
            ),
            div(style = "text-align:left; margin-top: 20px;", tags$h3("Features")),
            box(
                width = 12, status = "success",
                p("IEHFC provides features to perform a comprehensive list of checks on your dataset, ranging from basic data integrity tests to advanced statistical checks.")
            ),
            div(style = "text-align:left; margin-top: 20px;", tags$h3("Getting Started with IEHFC")),
            box(
                width = 12, status = "success",
                "Using IEHFC is as simple as:",
                tags$ul(
                    tags$li(a(href = "#", "Uploading your dataset", onclick = "Shiny.onInputChange('gotoTab', 'Upload Data')")),
                    tags$li(a(href = "#", "Selecting the desired checks", onclick = "Shiny.onInputChange('gotoTab', 'Check Selection and Setup')")),
                    tags$li(a(href = "#", "Reviewing and exporting your results", onclick = "Shiny.onInputChange('gotoTab', 'Outputs')"))
                )
            )
        )
    )
)