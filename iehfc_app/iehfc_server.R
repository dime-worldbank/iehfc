

  library(shiny)
  library(bslib)
  library(DT)
  library(bsicons)
  library(shinydashboard)
  
  iehfc_server <- function(input, output, session) {
      
      source("iehfc_app/server_scripts/duplicates.R", local = TRUE)
      source("iehfc_app/server_scripts/outliers.R",   local = TRUE)
      
      observeEvent(
          input$gotoTab, {
              # Use JavaScript to navigate to the selected tab
              shinyjs::runjs(
                  sprintf("$('.nav-item a:contains(%s)').tab('show')", input$gotoTab)
              )
          }
      )
      
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
          "Please upload your dataset using the sidebar on the left."
      })
      
      output$upload_tab_data <- renderUI({
          layout_column_wrap(
              heigth = "85vh",
              width = 1,
              card(
                  card_header(
                      span("Explore Dataset", bsicons::bs_icon("question-circle-fill")) %>%
                          tooltip(
                              "Use this section to make sure that your dataset is displaying as expected, or if you need to refer back to it while setting up your checks in the next tab.",
                              placement = "auto"
                          )
                  ),
                  DTOutput(
                      "hfc_ds_display"
                  )
              ),
              card(
                  card_header(
                      span("Explore Dataset Names", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "You can refer back to these variable names while setting the parameters for your checks in the next tab.",
                          placement = "auto"
                      )
                  ),
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
              height = "88vh",
              sidebar = sidebar(
                  width = "30%",
                  card(
                      fileInput(
                          "hfc_file",
                          label = span("Upload HFC Data", bsicons::bs_icon("question-circle-fill")) %>%
                              tooltip(
                                  "This is where you upload the dataset on which you want to run data quality checks. In the next version of this application, there will be error checks at this point to make sure that the dataset is in the right format/encoding",
                                  placement = "auto"
                              ),
                          accept = ".csv",
                          placeholder = "mydata.csv"
                      ),
                      span(
                          "Currently, this application only accepts .csv files. Please make sure your file is in the .csv format before uploading.",
                          style = "color: blue; font-size: 12px;"
                      )
                  )
              ),
              uiOutput("upload_tab_body")
          )
      })
      
      ## Setup Tab ----
      
        ### Check Selection ----
      
      output$check_select <- renderUI({
          checkboxGroupInput(
              "check_select", "Select High-Frequency Checks",
              choiceNames = list(
                  "Duplicates", "Outliers", "Enumerator-Level", "Administrative Unit-Level",
                  "Unit of Observation-Level", "Survey Programming"
              ),
              choiceValues = list(
                  "duplicate", "outlier", "enumerator", "admin", "unit", "programming"
              ),
              selected = c("duplicate")
          )
      })
      
      selected_checks <- reactive({
          input$check_select
      })
      
        ### Duplicate Check Setup ----
      
      current_duplicate_id_var <- reactiveVal() # For storing current state of 'duplicate_id_select_var'
      current_duplicate_extra_vars <- reactiveVal() # For storing current state of 'duplicate_extra_vars_select_var'
      
      # Observe any change in 'duplicate_id_select_var' and update current_duplicate_id_var
      observe({
          current_duplicate_id_var(input$duplicate_id_select_var)
      })
      
      # Observe any change in 'duplicate_extra_vars_select_var' and update current_duplicate_extra_vars
      observe({
          current_duplicate_extra_vars(input$duplicate_extra_vars_select_var)
      })
      
      output$duplicate_id_select <- renderUI({
          selectizeInput("duplicate_id_select_var", label = NULL, 
                         choices = hfc_dataset() %>%
                             names,
                         selected = current_duplicate_id_var(),
                         options = list('dropdownParent' = 'body'))
      })
      
      output$duplicate_extra_vars_select <- renderUI({
          selectizeInput("duplicate_extra_vars_select_var", label = NULL,
                         choices = hfc_dataset() %>%
                             select(
                                 -all_of(duplicate_id_var()) # Everything but the ID variable
                             ) %>%
                             names(), 
                         selected = current_duplicate_extra_vars(),
                         multiple = TRUE,
                         options = list('dropdownParent' = 'body'))
      })
      
      output$duplicate_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Duplicate Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "The duplicate check requires you to provide (1) the variable you want to check for duplicates and (2) any additional variable you want to include in the output table",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6, 
                             span("ID Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This is the variable that you want to check for duplicates (usually a uniquely identified ID)", 
                                         placement = "right"),
                             uiOutput("duplicate_id_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Additional Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These are additional variables that you may find useful in addressing duplicate observations in your data", 
                                         placement = "right"),
                             uiOutput("duplicate_extra_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Outlier Check Setup ----
      
      current_indiv_outlier_vars <- reactiveVal() # For storing current state of 'indiv_outlier_vars_select_var'
      current_group_outlier_vars <- reactiveVal() # For storing current state of 'group_outlier_vars_select_var'
      current_outlier_id_var     <- reactiveVal() # For storing current state of 'outlier_id_select_var'
      current_outlier_extra_vars <- reactiveVal() # For storing current state of 'outlier_extra_vars_select_var'
      
      # Observe any change in 'indiv_outlier_vars_select_var' and update current_indiv_outlier_vars
      observe({
          current_indiv_outlier_vars(input$indiv_outlier_vars_select_var)
      })
      
      # Observe any change in 'group_outlier_vars_select_var' and update current_group_outlier_vars
      observe({
          current_group_outlier_vars(input$group_outlier_vars_select_var)
      })
      
      # Observe any change in 'indiv_outlier_vars_select_var' and update current_outlier_id_var
      observe({
          current_outlier_id_var(input$outlier_id_select_var)
      })
      
      # Observe any change in 'indiv_outlier_vars_select_var' and update current_outlier_id_var
      observe({
          current_outlier_extra_vars(input$outlier_extra_vars_select_var)
      })
      
      output$indiv_outlier_vars_select <- renderUI({
          selectizeInput("indiv_outlier_vars_select_var", label = NULL,
                         choices = hfc_dataset() %>%
                             select(
                                 -all_of(outlier_id_var()) # Everything but the ID variable
                             ) %>%
                             names(), 
                         selected = current_indiv_outlier_vars(),
                         multiple = TRUE,
                         options = list('dropdownParent' = 'body'))
      })
      
      output$group_outlier_vars_select <- renderUI({
          selectizeInput("group_outlier_vars_select_var", label = NULL,
                         choices = hfc_dataset() %>%
                             select(
                                 -all_of(duplicate_id_var()) # Everything but the ID variable
                             ) %>%
                             names() %>%
                             tibble(var = .) %>%
                             filter(
                                 str_detect(var, "_[a-zA-z]{0,1}[0-9]+$") # All variables with e.g form "_1", "_01", or "_p1"
                             ) %>%
                             mutate(
                                 group = str_replace(var, "_{0,1}[0-9]+$", "") # Extract common portion of variable names
                             ) %>%
                             group_by(group) %>%
                             filter(n() > 1) %>% # Only keep groups that have more than one variable, otherwise just use indiv
                             select(group) %>%
                             distinct() %>%
                             pull(), 
                         selected = current_group_outlier_vars(),
                         multiple = TRUE,
                         options = list('dropdownParent' = 'body'))
      })
      
      output$outlier_id_select <- renderUI({
          selectizeInput("outlier_id_select_var", label = NULL, 
                         choices = hfc_dataset() %>%
                             names(), 
                         selected = current_outlier_id_var(),
                         options = list('dropdownParent' = 'body'))
      })
      
      output$outlier_extra_vars_select <- renderUI({
          selectizeInput("outlier_extra_vars_select_var", label = NULL,
                         choices = hfc_dataset() %>%
                             select(
                                 -all_of(duplicate_id_var()), # Everything but the ID variable or outlier variables
                                 -any_of(indiv_outlier_vars()),
                                 -any_of(matches(paste0("^", group_outlier_vars(), "_{0,1}[0-9]+$")))
                             ) %>%
                             names(), 
                         selected = current_outlier_extra_vars(),
                         multiple = TRUE,
                         options = list('dropdownParent' = 'body'))
      })
      
      output$outlier_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Outlier Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "Placeholder",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6,
                             span("Individual Outlier Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("Placeholder", 
                                         placement = "right"),
                             uiOutput("indiv_outlier_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Grouped Outlier Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("Placeholder", 
                                         placement = "right"),
                             uiOutput("group_outlier_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              ),
              card_body(
                  fluidRow(
                      column(6, 
                             span("ID Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This is the dataset's ID variable", 
                                         placement = "right"),
                             uiOutput("outlier_id_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Additional Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These are additional variables that you may find useful in addressing outliers in your data", 
                                         placement = "right"),
                             uiOutput("outlier_extra_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Enumerator Check Setup ----
      
      output$enumerator_setup <- renderUI(
          card(
              card_header("Enumerator Check Setup")
          )
      )
      
        ### Administrative Unit Check Setup ----
      
      output$admin_setup <- renderUI(
          card(
              card_header("Administrative Unit Check Setup")
          )
      )
      
        ### Unit of Observation Check Setup ----
      
      output$unit_setup <- renderUI(
          card(
              card_header("Unit of Observation Check Setup")
          )
      )
      
        ### Survey Programming Check Setup ----
      
      output$programming_setup <- renderUI(
          card(
              card_header("Survey Programming Check Setup")
          )
      )
      
        ### Setup Tab Setup ----
      
      output$setup_tab_nodata <- renderUI({
          "Please upload your data in the \"Upload Data\" Tab."
      })
      
      setup_check_list <- reactive({
          if(length(selected_checks()) == 0) { # No checks selected
              "Please select the high-frequency check(s) you would like to perform from the list in the sidebar."
          } else {
              selected_checks() %>%
                  purrr::map(
                      ~ uiOutput(paste0(.x, "_setup")) # Takes the name of the selected check and converts it to the corresponding card
                  )
          }
      })
      
      output$setup_tab_body <- renderUI({
          layout_column_wrap(
              height = "90vh",
              width = 1,
              setup_check_list()
          )
      })
      
      output$setup_run_hfcs_button <- renderUI({
          actionButton(
              "run_hfcs",
              "RUN HFCS",
              icon("paper-plane"),
              class = "btn btn-outline-primary btn-lg"
          )
      })
      
      # take you to output tab
      
      observeEvent(input$run_hfcs, {
          updateNavbarPage(session, "tabs", selected = "output_tab")
      })
      
      output$setup_tab_data <- renderUI({
          layout_sidebar(
              height = "90vh",
              sidebar = sidebar(
                  width = "30%",
                  card(
                      card_header(
                          span("Data Quality Checks", bsicons::bs_icon("question-circle-fill")) %>%
                              tooltip(
                                  "This is where you choose which data quality checks to run. For each check that you select, a corresponding box will appear on the right. You will need to make sure that the parameters within the box are correct before proceeding. Once you are done, click on the \"Run HFCs\" button below.",
                                  placement = "auto"
                              )
                      ),
                      uiOutput("check_select")
                  ),
                  card(
                      uiOutput("setup_run_hfcs_button")
                  )
              ),
              uiOutput("setup_tab_body")
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
      
# See server_scripts/duplicates.R for details on outputs creation
      
      output$duplicate_output <- renderUI({
          if("duplicate" %in% selected_checks()) {
              card(
                  DTOutput("duplicate_table"),
                  uiOutput("duplicate_table_dl")
              )
          } else {
              "If you would like to see Duplicate Checks, please select \"Duplicates\" in the left-hand sidebar of the \"Check Selection and Setup \" tab."
          }
      })
      
      output$duplicate_table_for_dl <- downloadHandler(
          filename = "duplicate_table.csv",
          content = function(file) {
              write.csv(duplicate_dataset(), file, row.names = FALSE)
          }
      )
      
      output$duplicate_table_dl <- renderUI({
          downloadButton("duplicate_table_for_dl", label = "Download Table")
      })
      
        ### Outlier Outputs ----
      
# See server_scripts/outliers.R for details on outputs creation
      
      output$outlier_output <- renderUI({
          if("outlier" %in% selected_checks()) {
              card(
                  DTOutput("outlier_table"),
                  uiOutput("outlier_table_dl")
              )
          } else {
              "If you would like to see Outlier Checks, please select \"Outliers\" in the left-hand sidebar of the \"Check Selection and Setup \" tab."
          }
      })
      
      output$outlier_table_for_dl <- downloadHandler(
          filename = "outlier_table.csv",
          content = function(file) {
              write.csv(outlier_dataset(), file, row.names = FALSE)
          }
      )
      
      output$outlier_table_dl <- renderUI({
          downloadButton("outlier_table_for_dl", label = "Download Table")
      })
      
        ### Output Tab Setup ----
      
      output$output_tab_nodata <- renderUI({
          "Please upload your data in the \"Upload Data\" Tab."
      })
      
      output$output_tab_norun <- renderUI({
          "Please click on the \"Run HFCs\" button in the \"Check Selection and Setup\" tab to display outputs."
      })
      
      output$output_tab_data <- renderUI({
          navset_tab(
              nav_panel("Duplicates", uiOutput("duplicate_output")),
              nav_panel("Outliers", uiOutput("outlier_output")),
              nav_panel("Enumerator", "Hello enumerator"),
              nav_panel("Admin Level", "Hello admin level"),
              nav_panel("Tracking", "Hello tracking"),
              nav_panel("Programming", "Hello programming")
          )
      })
      
      output$output_tab <- renderUI({
          if(is.null(hfc_file())) {
              return(uiOutput("output_tab_nodata"))
          } else if(input$run_hfcs == 0) {
              return(uiOutput("output_tab_norun"))
          } else {
              return(uiOutput("output_tab_data"))
          }
      })
  }
  
  iehfc_server