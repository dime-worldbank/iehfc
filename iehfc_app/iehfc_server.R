

  library(shiny)
  library(bslib)
  
  iehfc_server <- function(input, output) {
      
      ## Check Selection
      output$selected_checks <- renderText({
          paste0(
              "You chose ", paste(input$check_select, collapse = ", ")
          )
      })
  }
  
  iehfc_server