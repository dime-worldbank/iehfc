

  library(shiny)
  library(bslib)
  library(DT)
  
  page_navbar(
      title = "iehfc",
      nav_panel(
          "Introduction",
          includeMarkdown("intro.Rmd")
      ),
      nav_panel(
          "Upload Data",
          uiOutput("upload_tab")
      ),
      nav_panel(
          "Check Selection and Setup",
          uiOutput("setup_tab")
      ),
      nav_panel(
          "Outputs",
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
  