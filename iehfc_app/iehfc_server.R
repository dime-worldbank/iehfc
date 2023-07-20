

  library(shiny)
  library(bslib)
  library(DT)
  
  iehfc_server <- function(input, output) {
      
      ## Upload Tab ----
      
      hfc_file <- reactive({
          input$hfc_file
      })
      
      hfc_dataset <- reactive({
          ds <- read.csv(hfc_file()$datapath, na.strings = "")
          return(ds)
      })
      
      output$hfc_ds_display <- renderDT(
          hfc_dataset(), fillContainer = TRUE
      )
      
      output$hfc_ds_names_display <- renderPrint({
          print_matrix <- function(vctr, col_num = 1) {
              matrix <- vctr %>%
                  `[`(
                      1:(col_num * ceiling(length(.) / col_num))
                  ) %>% # To get the right number of elements for the matrix
                  matrix(ncol = col_num, byrow = TRUE)
              matrix[is.na(matrix)] <- ""
              write.table(
                  format(matrix, justify = "right"),
                  row.names = FALSE, col.names = FALSE, quote = FALSE
              )
          }
          
          hfc_ds_names <- hfc_dataset() %>%
              names() %>%
              print_matrix(col_num = 4)
      })
      
      output$upload_tab_nodata <- renderUI({
          "No data"
      })
      
      output$upload_tab_data <- renderUI({
          layout_column_wrap(
              heigth = "85vh",
              width = 1,
              card(
                  card_header("Explore Dataset"),
                  DTOutput(
                      "hfc_ds_display"
                  )
              ),
              card(
                  card_header("Explore Dataset Names"),
                  verbatimTextOutput(
                      "hfc_ds_names_display"
                  )
              )
          )
      })
      
      output$upload_tab_body <- renderUI({
          if(is.null(hfc_file())) {
              return(uiOutput("upload_tab_nodata"))
          } else {
              return(uiOutput("upload_tab_data"))
          }
      })
      
      output$upload_tab <- renderUI({
          layout_sidebar(
              height = "90vh",
              sidebar = sidebar(
                  width = "30%",
                  card(
                      fileInput(
                          "hfc_file",
                          label = "Upload HFC Data",
                          accept = "csv",
                          placeholder = "mydata.csv"
                      )
                  )
              ),
              uiOutput("upload_tab_body")
          )
      })
      
      ## Setup Tab ----
      
        ### Check Selection
      
      output$check_select <- renderUI({
          checkboxGroupInput(
              "check_select", "Select High-Frequency Checks",
              choiceNames = list(
                  "Duplicates", "Outliers", "Enumerator-Level", "Administrative Unit-Level",
                  "Unit of Observation-Level", "Survey Programming"
              ),
              choiceValues = list(
                  "duplicates", "outliers", "enumerator", "admin", "unit", "programming"
              ),
              selected = c("duplicates")
          )
      })
      
        ### Duplicate Parameter Selection
      
      output$duplicate_id_select <- renderUI({
          selectInput(
              "duplicate_id_select_var", label = NULL,
              choices = names(hfc_dataset())
          )
      })
      
      output$duplicate_extra_vars_select <- renderUI({
          selectInput(
              "duplicate_extra_vars_select_var", label = NULL, choices = names(hfc_dataset()),
              multiple = TRUE
          )
      })
      
      output$setup_tab_nodata <- renderUI({
          "Please upload your data in the \"Upload Data\" Tab."
      })
      
      output$setup_tab_data <- renderUI({
          layout_sidebar(
              height = "90vh",
              sidebar = sidebar(
                  width = "30%",
                  card(
                      card_header("Data Quality Checks"),
                      uiOutput("check_select")
                  )
              ),
              card(
                  height = "30vh", fill = FALSE,
                  full_screen = TRUE,
                  card_header("Duplicate Variable Identification"),
                  card_body(
                      layout_column_wrap(
                          width = 1/3,
                          "", "ID Variable",
                          uiOutput("duplicate_id_select"),
                          "", "Additional Variables",
                          uiOutput("duplicate_extra_vars_select")
                      )
                  )
              )
          )
      })
      
      output$setup_tab <- renderUI({
          if(is.null(hfc_file())) {
              return(uiOutput("setup_tab_nodata"))
          } else {
              return(uiOutput("setup_tab_data"))
          }
      })
      
      ## Output Tab ----
      
        ### Duplicate Outputs ----
      
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
      
      output$duplicate_output <- renderUI({
          card(
              height = "85vh",
              DTOutput("duplicate_table"),
              uiOutput("duplicate_table_dl")
          )
      })
      
      output$output_tab_nodata <- renderUI({
          "Please upload your data in the \"Upload Data\" Tab."
      })
      
      output$output_tab_data <- renderUI({
          navset_tab(
              nav_panel("Duplicates", uiOutput("duplicate_output")),
              nav_panel("Outliers", "Hello outliers"),
              nav_panel("Enumerator", "Hello enumerator"),
              nav_panel("Admin Level", "Hello admin level"),
              nav_panel("Tracking", "Hello tracking"),
              nav_panel("Programming", "Hello programming")
          )
      })
      
      output$output_tab <- renderUI({
          if(is.null(hfc_file())) {
              return(uiOutput("output_tab_nodata"))
          } else {
              return(uiOutput("output_tab_data"))
          }
      })
  }
  
  iehfc_server