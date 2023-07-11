

  library(shiny)
  library(bslib)
  library(DT)
  
  iehfc_server <- function(input, output) {
      
      ## HFC Dataset
      
      hfc_file <- reactive({
          input$hfc_file
      })
      
      hfc_dataset <- reactive({
          ds <- read.csv(hfc_file()$datapath, na.strings = "")
          return(ds)
      })
      
      # Duplicate Parameter Selection
      
      output$duplicate_id_select <- renderUI({
          validate(need(hfc_file(), "Please upload dataset in sidebar"))
          selectInput(
              "duplicate_id_select_var", label = NULL,
              choices = names(hfc_dataset())
          )
      })
      
      output$duplicate_extra_vars_select <- renderUI({
          validate(need(hfc_file(), "Please upload dataset in sidebar"))
          selectInput(
              "duplicate_extra_vars_select_var", label = NULL, choices = names(hfc_dataset()),
              multiple = TRUE
          )
      })
      
      # Duplicate Output Creation
      
      duplicate_id_var <- reactive({
          input$duplicate_id_select_var
      })
      
      duplicate_extra_vars <- reactive({
          input$duplicate_extra_vars_select_var
      })
      
      duplicate_dataset <- reactive({
          hfc_dataset() %>%
              group_by(!!sym(duplicate_id_var())) %>%
              filter(n() > 1) %>%
              ungroup() %>%
              select(
                  all_of(
                      c(duplicate_id_var(), duplicate_extra_vars())
                  )
              )
      })
      
      output$duplicate_table <- renderDT({
          validate(need(hfc_file(), "Please upload dataset in sidebar"))
          duplicate_dataset()
      })
      
      output$duplicate_table_for_dl <- downloadHandler(
          filename = "duplicate_table.csv",
          content = function(file) {
              write.csv(duplicate_dataset(), file, row.names = FALSE)
          }
      )
      
      output$duplicate_table_dl <- renderUI({
          req(hfc_file())
          downloadButton("duplicate_table_for_dl", label = "Download Table")
      })
  }
  
  iehfc_server