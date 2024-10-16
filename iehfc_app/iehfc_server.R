

  library(shiny)
  library(bslib)
  library(DT)
  library(bsicons)
  library(shinydashboard)
  
  # Increase maximum file upload size
  
  options(shiny.maxRequestSize = 100 * (1024^2))

  iehfc_server <- function(input, output, session) {
      
      source("iehfc_app/server_scripts/duplicates.R", local = TRUE)
      source("iehfc_app/server_scripts/outliers.R",   local = TRUE)
      source("iehfc_app/server_scripts/enumerator.R", local = TRUE)
      source("iehfc_app/server_scripts/admin.R",      local = TRUE)
      source("iehfc_app/server_scripts/unit.R",       local = TRUE)
      
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
      
      hfc_dataset <- reactiveVal()
      
      observeEvent(input$hfc_file, {
          ds <- fread(hfc_file()$datapath, na.strings = c("", "NA_character_", "NA"))
          too_many_cols <- ncol(ds) > 10000
          if(too_many_cols) {
              showNotification(
                  "Your dataset has more than 10,000 variables. Unfortunately, the platform currently cannot handle such a large dataset. Please create a subset of your dataset and try again.",
                  duration = NULL,
                  type     = "error"
              )
          }
          req(!too_many_cols)
          hfc_dataset(ds)
      })
      
      observeEvent(input$use_test_data, {
          ds <- fread("test_data/LWH_FUP2_raw_data.csv", na.strings = "")
          hfc_dataset(ds)
      })
      
      output$hfc_ds_display <- renderDT(
          hfc_dataset(), fillContainer = TRUE
      )
      
      output$hfc_ds_names_types_display <- renderDT({
          hfc_ds_names_types <- hfc_dataset() %>%
              tibble::as_tibble() %>%
              dplyr::summarise_all(class) %>%
              tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Type")
          
          datatable(hfc_ds_names_types, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      
      output$upload_tab_nodata <- renderUI({
          "Please either upload your dataset using the sidebar on the left or click on the \"Use Test Data\" button."
      })
      
      output$upload_tab_data <- renderUI({
          layout_column_wrap(
              height = "120vh",
              width = 1,
              card(
                  card_header(
                      span("Explore Dataset", bsicons::bs_icon("question-circle-fill")) %>%
                          tooltip(
                              "Use this section to make sure that your dataset is displaying as expected, or refer back to it while setting up your checks in the next tab.",
                              placement = "auto"
                          )
                  ),
                  DTOutput(
                      "hfc_ds_display"
                  )
              ),
              card(
                  card_header(
                      span("Explore Variable Names and Types", bsicons::bs_icon("question-circle-fill")) %>%
                          tooltip(
                              "You can refer back to these variable names while setting the parameters for your checks in the next tab.",
                              placement = "auto"
                          )
                  ),
                  DTOutput(
                      "hfc_ds_names_types_display"
                  )
              )
          )
      })
      
      output$upload_tab_body <- renderUI({
          if(is.null(hfc_dataset())) {
              return(uiOutput("upload_tab_nodata"))
          } else {
              return(uiOutput("upload_tab_data"))
          }
      })
      
      output$use_test_data_button <- renderUI({
          actionButton(
              "use_test_data",
              "Use Test Data",
              icon("table"),
              class = "btn btn-outline-primary btn-lg",
              width = "100%"
          )
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
                          style = "color: #593196; font-size: 12px;"
                      )
                  ),
                  card(
                      uiOutput("use_test_data_button")
                  )
              ),
              uiOutput("upload_tab_body")
          )
      })
      
      
      ## Setup Tab ----
      
        ### Read from imported parameters
      
      parameter_file <- reactive({
          input$parameter_file
      })
      
      imported_para_dataset <- reactiveVal()
      
      observeEvent(input$parameter_file, {
          para_ds <- read.csv(parameter_file()$datapath)
          imported_para_dataset(para_ds)
      })
      
      
   
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
              selected = if (!is.null(imported_para_dataset())) {
                  selected_rows <- c(imported_para_dataset()$Check)
              } else {
                  selected_rows <- c("duplicate")  # Default selection if dataset is not created
              }
              
          )
      })
      
      
      selected_checks <- reactive({
          input$check_select
      })
      

      
        ### Duplicate Check Setup ----
      
      # Duplicate data quality checks:
          # Duplicate IDs
          # Additional variables for reference
      
      current_duplicate_id_var     <- reactiveVal() # For storing current state of 'duplicate_id_select_var'
      current_duplicate_extra_vars <- reactiveVal() # For storing current state of 'duplicate_extra_vars_select_var'
      
      # Bring duplicate variables from uploaded parameter dataset
      observe({
          duplicate_id_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "duplicate_id_select_var", "Value"]
          if (!is.null(duplicate_id_select_var_imported)) {
              current_duplicate_id_var(duplicate_id_select_var_imported)
          }
          
          duplicate_extra_vars_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "duplicate_extra_vars_select_var", "Value"]
          if (!is.null(duplicate_extra_vars_imported)) {
              current_duplicate_extra_vars(duplicate_extra_vars_imported)
          }
      })
      
      # Observe any change in 'duplicate_id_select_var' and update current_duplicate_id_var
      observe({
          current_duplicate_id_var(input$duplicate_id_select_var)
      })
      
      # Observe any change in 'duplicate_extra_vars_select_var' and update current_duplicate_extra_vars
      observe({
          current_duplicate_extra_vars(input$duplicate_extra_vars_select_var)
      })
      
      output$duplicate_id_select <- renderUI({
          selectizeInput(
              "duplicate_id_select_var", label = NULL, 
              choices = hfc_dataset() %>% names,
              selected = current_duplicate_id_var(),  # Preserve the selection
              options = list('dropdownParent' = 'body')
          )
      })
      
      observe({
          updateSelectizeInput(session, "duplicate_id_select_var", 
                               choices = hfc_dataset() %>% names,
                               selected = current_duplicate_id_var())
      })
      
      
      
      output$duplicate_extra_vars_select <- renderUI({
          selectizeInput(
              "duplicate_extra_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(duplicate_id_var()[duplicate_id_var() != ""]) # Everything but the ID variable
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(duplicate_extra_vars()[duplicate_extra_vars() != ""]),
                      !any_of(duplicate_extra_vars()[duplicate_extra_vars() != ""])
                  ) %>%
                  names(), 
              selected = current_duplicate_extra_vars(),
              multiple = TRUE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$duplicate_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Duplicate Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "The duplicate check requires you to provide the variable you want to check for duplicates. You can add any additional variable you want to include in the output table",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6, 
                             span("ID Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This is the variable that you want to check for duplicates (usually an ID intended to be uniquely identified)", 
                                         placement = "right"),
                             uiOutput("duplicate_id_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Display Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These are additional variables that you may want to display in the output table", 
                                         placement = "right"),
                             uiOutput("duplicate_extra_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Outlier Check Setup ----
      
        # Outlier data quality checks: 
          # "Individual" outlier checks -- check individual variable for outliers
          # "Group" outlier checks -- check group of variables (e.g. income for every household member) for outliers
          # Additional variables for reference
      
      current_indiv_outlier_vars <- reactiveVal() # For storing current state of 'indiv_outlier_vars_select_var'
      current_group_outlier_vars <- reactiveVal() # For storing current state of 'group_outlier_vars_select_var'
      current_outlier_id_var     <- reactiveVal() # For storing current state of 'outlier_id_select_var'
      current_outlier_extra_vars <- reactiveVal() # For storing current state of 'outlier_extra_vars_select_var'
      current_outlier_method     <- reactiveVal() # For storing current state of 'outlier_method'
      current_outlier_multiplier <- reactiveVal() # For storing current state of 'outlier_multiplier'
      
      # Bring outlier variables from uploaded parameter dataset
      observe({
          indiv_outlier_vars_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "indiv_outlier_vars_select_var", "Value"]
          if (!is.null(indiv_outlier_vars_select_var_imported)) {
              current_indiv_outlier_vars(indiv_outlier_vars_select_var_imported)
          }
          
          group_outlier_vars_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "group_outlier_vars_select_var", "Value"]
          if (!is.null(group_outlier_vars_select_var_imported)) {
              current_duplicate_extra_vars(group_outlier_vars_select_var_imported)
          }
          
          outlier_id_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "outlier_id_select_var", "Value"]
          if (!is.null(outlier_id_select_var_imported)) {
              current_outlier_id_var(outlier_id_select_var_imported)
          }
          
          outlier_extra_vars_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "outlier_extra_vars_select_var", "Value"]
          if (!is.null(outlier_extra_vars_select_var_imported)) {
              current_outlier_extra_vars(outlier_extra_vars_select_var_imported)
          }
          
          outlier_method_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "outlier_method", "Value"]
          if (!is.null(outlier_method_imported)) {
              current_outlier_method(outlier_method_imported)
          }
          
          outlier_multiplier_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "outlier_multiplier", "Value"]
          if (!is.null(outlier_multiplier_imported)) {
              current_outlier_multiplier(outlier_multiplier_imported)
          }
      })
      
      
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
      
      # Observe any change in 'indiv_outlier_method' and update current_outlier_method
      observe({
          current_outlier_method(input$outlier_method)
      })
      
      # Observe any change in 'indiv_outlier_multiplier' and update current_outlier_multiplier
      observe({
          current_outlier_multiplier(input$outlier_multiplier)
      })
      
      output$indiv_outlier_vars_select <- renderUI({
          selectizeInput(
              "indiv_outlier_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(outlier_id_var()[outlier_id_var() != ""]) # Everything but the ID variable
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(indiv_outlier_vars()[indiv_outlier_vars() != ""]),
                      !any_of(indiv_outlier_vars()[indiv_outlier_vars() != ""])
                  ) %>%
                  select(where(is.numeric)) %>%
                  names(), 
              selected = current_indiv_outlier_vars(),
              multiple = TRUE,
              options = list('dropdownParent' = 'body')
          )
      })
      
    
      
      output$group_outlier_vars_select <- renderUI({
          selectizeInput(
              "group_outlier_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(duplicate_id_var()[duplicate_id_var() != ""]) # Everything but the ID variable
                  ) %>%
                  select(where(is.numeric)) %>%
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
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$outlier_id_select <- renderUI({
          selectizeInput(
              "outlier_id_select_var", label = NULL, 
              choices = hfc_dataset() %>%
                  names(), 
              selected = current_outlier_id_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      observe({
          updateSelectizeInput(session, "outlier_id_select_var", 
                               choices = hfc_dataset() %>% names,
                               selected = current_outlier_id_var())
      })
      
      output$outlier_extra_vars_select <- renderUI({
          selectizeInput(
              "outlier_extra_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(duplicate_id_var()[duplicate_id_var() != ""]), # Everything but the ID variable or outlier variables
                      -any_of(indiv_outlier_vars()),
                      -any_of(matches(paste0("^", group_outlier_vars(), "_{0,1}[0-9]+$")))
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(outlier_extra_vars()),
                      !any_of(outlier_extra_vars())
                  ) %>%
                  names(), 
              selected = current_outlier_extra_vars(),
              multiple = TRUE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$outlier_method <- renderUI({
          default_method <- ifelse(is.null(current_outlier_method()) || current_outlier_method() == "", "sd", 
                                   current_outlier_method())
          selectizeInput(
              "outlier_method", label = NULL, 
              choices = c("iqr", "sd"), 
              selected = default_method,
              multiple = FALSE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$outlier_multiplier <- renderUI({
          default_multiplier <- ifelse(is.null(current_outlier_multiplier()) || current_outlier_multiplier() == "", "3", 
                                       current_outlier_multiplier())
          selectizeInput(
              "outlier_multiplier", label = NULL, 
              choices = c("1.5", "3"), 
              selected = default_multiplier,
              multiple = FALSE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$outlier_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Outlier Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "The outlier check requires you to provide (1) individual variables or groups of variables you want to check for outliers and (2) an ID variable to identify the observations containing outliers. You may also add any additional variables you want to include in the output table",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6,
                             span("Individual Outlier Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These need to be numeric variables. Use this if you have an individual variable you'd like to check for outliers", 
                                         placement = "right"),
                             uiOutput("indiv_outlier_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Grouped Outlier Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These need to be numeric variables. Use this if you have a set of variables you'd like to check for outliers together (e.g. inc_01, inc_02, etc.)", 
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
                             span("Display Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These are additional variables that you may want to display in the output table", 
                                         placement = "right"),
                             uiOutput("outlier_extra_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              ),
              card_body(
                  fluidRow(
                      column(6, 
                             span("Method", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("Select preferred method for outlier calculation", 
                                         placement = "right"),
                             uiOutput("outlier_method", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Multiplier", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("Select the multiplier to be used for outlier calculation", 
                                         placement = "right"),
                             uiOutput("outlier_multiplier", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Enumerator Check Setup ----
      
          # Enumerator data quality checks:
          # Number of surveys per enumerator
          # Average variable value per enumerator
          # Number of surveys per day per enumerator (requires submission date variable)
          # Average survey time per enumerator (requires survey duration)
          # Average time per module per enumerator (requires module time)
      
      current_enumerator_var          <- reactiveVal() # For storing current state of 'enumerator_var_select_var'
      current_enumerator_ave_vars     <- reactiveVal() # For storing current state of 'enumerator_ave_vars_select_var'
      current_enumerator_date_var     <- reactiveVal() # For storing current state of 'enumerator_date_var_select_var'
      current_enumerator_complete_var <- reactiveVal() # For storing current state of 'enumerator_complete_var_select_var'
      
      # Bring enumerator variables from uploaded parameter dataset
      observe({
          enumerator_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "enumerator_var_select_var", "Value"]
          if (!is.null(enumerator_var_select_var_imported)) {
              current_enumerator_var(enumerator_var_select_var_imported)
          }
          
          enumerator_ave_vars_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "enumerator_ave_vars_select_var", "Value"]
          if (!is.null(enumerator_ave_vars_select_var_imported)) {
              current_enumerator_ave_vars(enumerator_ave_vars_select_var_imported)
          }
          
          enumerator_date_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "enumerator_date_var_select_var", "Value"]
          if (!is.null(enumerator_date_var_select_var_imported)) {
              current_enumerator_date_var(enumerator_date_var_select_var_imported)
          }
          
          enumerator_complete_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "enumerator_complete_var_select_var", "Value"]
          if (!is.null(enumerator_complete_var_select_var_imported)) {
              current_enumerator_complete_var(enumerator_complete_var_select_var_imported)
          }
      })
      
      # Observe any change in 'enumerator_var_select_var' and update current_enumerator_var
      observe({
          current_enumerator_var(input$enumerator_var_select_var)
      })
      
      # Observe any change in 'enumerator_ave_vars_select_var' and update current_enumerator_ave_vars
      observe({
          current_enumerator_ave_vars(input$enumerator_ave_vars_select_var)
      })
      
      # Observe any change in 'enumerator_date_var_select_var' and update current_enumerator_date_var
      observe({
          current_enumerator_date_var(input$enumerator_date_var_select_var)
      })
      
      # Observe any change in 'enumerator_complete_var_select_var' and update current_enumerator_complete_var
      observe({
          current_enumerator_complete_var(input$enumerator_complete_var_select_var)
      })
      
      output$enumerator_var_select <- renderUI({
          selectizeInput(
              "enumerator_var_select_var", label = NULL, 
              choices = hfc_dataset() %>%
                  names(), 
              selected = current_enumerator_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      observe({
          updateSelectizeInput(session, "enumerator_var_select_var", 
                               choices = hfc_dataset() %>% names,
                               selected = current_enumerator_var())
      })
      
      output$enumerator_ave_vars_select <- renderUI({
          selectizeInput(
              "enumerator_ave_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(enumerator_var()[enumerator_var() != ""])
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(enumerator_ave_vars()[enumerator_ave_vars() != ""]),
                      !any_of(enumerator_ave_vars()[enumerator_ave_vars() != ""])
                  ) %>%
                  names(), 
              selected = current_enumerator_ave_vars(),
              multiple = TRUE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$enumerator_date_var_select <- renderUI({
          selectizeInput(
              "enumerator_date_var_select_var", label = NULL, 
              choices = c(
                  "", # Provides no option as a possibility
                  hfc_dataset() %>%
                      select(
                          -all_of(enumerator_var()[enumerator_var() != ""])
                      ) %>%
                      names()
              ),
              selected = current_enumerator_date_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$enumerator_complete_var_select <- renderUI({
          selectizeInput(
              "enumerator_complete_var_select_var", label = NULL, 
              choices = c(
                  "", # Provides no option as a possibility
                  hfc_dataset() %>%
                      select(
                          -all_of(enumerator_var()[enumerator_var() != ""])
                      ) %>%
                      names()
              ), 
              selected = current_enumerator_complete_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$enumerator_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Enumerator Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "The enumerator check requires you to provide (1) the variable that identifies enumerators and (2) variables for which you'd like to see average values for each enumerator. You can include a submission date variable and a \"submission completeness\" variable",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6,
                             span("Enumerator Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This is the variable that identifies the enumerator for each submission", 
                                         placement = "right"),
                             uiOutput("enumerator_var_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Enumerator Average Value Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These need to be numeric variables. Variables for which you want the average value by enumerator", 
                                         placement = "right"),
                             uiOutput("enumerator_ave_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              ),
              card_body(
                  fluidRow(
                      column(6, 
                             span("Submission Date Variable (Optional)", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This could be the date at which the survey was completed, or the date at which the survey was submitted.", 
                                         placement = "right"),
                             uiOutput("enumerator_date_var_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6, 
                             span("Submission Complete Variable (Optional)", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This should be a dummy variable (either 1/0 or Yes/No) that identifies whether a survey was completed—or successfully submitted—or not.", 
                                         placement = "right"),
                             uiOutput("enumerator_complete_var_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Administrative Unit Check Setup ----
      
      # Administrative-unit data quality checks:
      # Number of surveys per administrative unit
      # Number of surveys per day per administrative unit (requires submission date variable)
      # (Eventually) Progress (requires principal sample dataset)
      
      current_admin_var          <- reactiveVal() # For storing current state of 'admin_var_select_var'
      current_admin_super_vars   <- reactiveVal() # For storing current state of 'admin_super_vars_select_var'
      current_admin_date_var     <- reactiveVal() # For storing current state of 'admin_date_var_select_var'
      current_admin_complete_var <- reactiveVal() # For storing current state of 'admin_complete_var_select_var'
      
      # Bring admin variables from uploaded parameter dataset
      observe({
          admin_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "admin_var_select_var", "Value"]
          if (!is.null(admin_var_select_var_imported)) {
              current_admin_var(admin_var_select_var_imported)
          }
          
          admin_super_vars_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "admin_super_vars_select_var", "Value"]
          if (!is.null(admin_super_vars_select_var_imported)) {
              current_admin_super_vars(admin_super_vars_select_var_imported)
          }
          
          admin_date_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "admin_date_var_select_var", "Value"]
          if (!is.null(admin_date_var_select_var_imported)) {
              current_admin_date_var(admin_date_var_select_var_imported)
          }
          
          admin_complete_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "admin_complete_var_select_var", "Value"]
          if (!is.null(admin_complete_var_select_var_imported)) {
              current_admin_complete_var(admin_complete_var_select_var_imported)
          }
      })
      
      
      # Observe any change in 'admin_var_select_var' and update current_admin_var
      observe({
          current_admin_var(input$admin_var_select_var)
      })
      
      # Observe any change in 'admin_var_select_var' and update current_admin_var
      observe({
          current_admin_super_vars(input$admin_super_vars_select_var)
      })
      
      # Observe any change in 'admin_date_var_select_var' and update current_admin_date_var
      observe({
          current_admin_date_var(input$admin_date_var_select_var)
      })
      
      # Observe any change in 'admin_complete_var_select_var' and update current_admin_complete_var
      observe({
          current_admin_complete_var(input$admin_complete_var_select_var)
      })
      
      output$admin_var_select <- renderUI({
          selectizeInput(
              "admin_var_select_var", label = NULL, 
              choices = hfc_dataset() %>%
                  names(), 
              selected = current_admin_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      observe({
          updateSelectizeInput(session, "admin_var_select_var", 
                               choices = hfc_dataset() %>% names,
                               selected = current_admin_var())
      })
      
      output$admin_super_vars_select <- renderUI({
          selectizeInput(
              "admin_super_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(admin_var()[admin_var() != ""])
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(admin_super_vars()[admin_super_vars() != ""]),
                      !any_of(admin_super_vars()[admin_super_vars() != ""])
                  ) %>%
                  names(), 
              selected = current_admin_super_vars(),
              multiple = TRUE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$admin_date_var_select <- renderUI({
          selectizeInput(
              "admin_date_var_select_var", label = NULL, 
              choices = c(
                  "", # Provides no option as a possibility
                  hfc_dataset() %>%
                      select(
                          -all_of(admin_var()[admin_var() != ""])
                      ) %>%
                      names()
              ),
              selected = current_admin_date_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$admin_complete_var_select <- renderUI({
          selectizeInput(
              "admin_complete_var_select_var", label = NULL, 
              choices = c(
                  "", # Provides no option as a possibility
                  hfc_dataset() %>%
                      select(
                          -all_of(admin_var()[admin_var() != ""])
                      ) %>%
                      names()
              ), 
              selected = current_admin_complete_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$admin_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Administrative Unit-Level Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "The administrative unit-level check requires you to (1) provide the variable that identifies the administrative unit and (2) higher-level administrative units that would help either locate or uniquely identify the administrative level of choice. You can include a submission date variable and a \"submission completeness\" variable",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6,
                             span("Administrative Unit Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This is the variable that identifies the administrative unit for each submission", 
                                         placement = "right"),
                             uiOutput("admin_var_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Higher-Level Administrative Unit Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("These variables could serve to either locate or uniquely identify the administrative units of interest", 
                                         placement = "right"),
                             uiOutput("admin_super_vars_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              ),
              card_body(
                  fluidRow(
                      column(6, 
                             span("Submission Date Variable (Optional)", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This could be the date at which the survey was completed, or the date at which the survey was submitted.", 
                                         placement = "right"),
                             uiOutput("admin_date_var_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      ),
                      column(6, 
                             span("Submission Complete Variable (Optional)", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This should be a dummy variable (either 1/0 or Yes/No) that identifies whether a survey was completed—or successfully submitted—or not.", 
                                         placement = "right"),
                             uiOutput("admin_complete_var_select", style = "z-index: 1000;")  # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Unit of Observation Check Setup ----
      
      # Unit of observation data quality checks:
      # This is more of a "tracking" service. Its purpose is more often to be able to check the
      # status of a specific unit of observation (e.g. household).
      
      current_unit_var        <- reactiveVal() # For storing current state of 'unit_var_select_var'
      current_unit_extra_vars <- reactiveVal() # For storing current state of 'unit_extra_vars_select_var'
      
      # Bring unit variables from uploaded parameter dataset
      observe({
          unit_var_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "unit_var_select_var", "Value"]
          if (!is.null(unit_var_select_var_imported)) {
              current_unit_var(unit_var_select_var_imported)
          }
          
          unit_extra_vars_select_var_imported <- 
              imported_para_dataset()[imported_para_dataset()$Parameter == "unit_extra_vars_select_var", "Value"]
          if (!is.null(unit_extra_vars_select_var_imported)) {
              current_unit_extra_vars(unit_extra_vars_select_var_imported)
          }
      })
      
      # Observe any change in 'unit_var_select_var' and update current_unit_var
      observe({
          current_unit_var(input$unit_var_select_var)
      })
      
      # Observe any change in 'unit_extra_vars_select_var' and update current_unit_extra_vars
      observe({
          current_unit_extra_vars(input$unit_extra_vars_select_var)
      })
      
      output$unit_var_select <- renderUI({
          selectizeInput(
              "unit_var_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  names(),
              selected = current_unit_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      observe({
          updateSelectizeInput(session, "unit_var_select_var", 
                               choices = hfc_dataset() %>% names,
                               selected = current_unit_var())
      })
      
      output$unit_extra_vars_select <- renderUI({
          selectizeInput(
              "unit_extra_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(unit_var()[unit_var() != ""]) 
                  ) %>%
                  select(
                      all_of(unit_extra_vars()[unit_extra_vars() != ""]), 
                      !any_of(unit_extra_vars()[unit_extra_vars() != ""]) 
                  ) %>%
                  names(), 
              selected = current_unit_extra_vars(),
              multiple = TRUE,
              options = list('dropdownParent' = 'body')
          )
      })
      
      
      output$unit_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Unit of Observation Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "Placeholder text",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6,
                             span("Unit of Observation/ID Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip(
                                     "Placeholder text",
                                     placement = "right"
                                 ),
                             uiOutput("unit_var_select", style = "z-index: 1000;") # Set a high z-index to overlap other elements
                      ),
                      column(6,
                             span("Display Variables", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip(
                                     "Placeholder text",
                                     placement = "right"
                                 ),
                             uiOutput("unit_extra_vars_select", style = "z-index: 1000;") # Set a high z-index to overlap other elements
                      )
                  )
              )
          )
      })
      
        ### Survey Programming Check Setup ----
      
      output$programming_setup <- renderUI(
          card(
              card_header("Survey Programming Check Setup"),
              "Under construction!"
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
      
           ### Export parameters
   
      parameter_dataset <- reactive({
          # Initialize an empty data frame
          combined_df <- data.frame(Parameter = character(0), Name = character(0), Value = character(0))
          current_datetime <- now()
          
          # Check if each parameter is selected and add it to the combined data frame
          
          ## Duplicates
          if (!is.null(input$duplicate_id_select_var)) {
              para1 <- data.frame(Check = "duplicate",
                                  Parameter = "duplicate_id_select_var", 
                                  Name = "Duplicates ID variable", 
                                  Value = c(input$duplicate_id_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para1)
          }
          
          if (!is.null(input$duplicate_extra_vars_select_var)) {
              para2 <- data.frame(Check = "duplicate",
                                  Parameter = "duplicate_extra_vars_select_var", 
                                  Name = "Duplicates display variables", 
                                  Value = c(input$duplicate_extra_vars_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para2)
          }
          
          ## Outlier
          if (!is.null(input$indiv_outlier_vars_select_var)) {
              para3 <- data.frame(Check = "outlier",
                                  Parameter = "indiv_outlier_vars_select_var", 
                                  Name = "Individual outlier variables", 
                                  Value = c(input$indiv_outlier_vars_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para3)
          }
          
          if (!is.null(input$group_outlier_vars_select_var)) {
              para4 <- data.frame(Check = "outlier",
                                  Parameter = "group_outlier_vars_select_var", 
                                  Name = "Grouped outlier variables", 
                                  Value = c(input$group_outlier_vars_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para4)
          }
          
          if (!is.null(input$outlier_id_select_var)) {
              para5 <- data.frame(Check = "outlier",
                                  Parameter = "outlier_id_select_var", 
                                  Name = "Outlier ID variable", 
                                  Value = c(input$outlier_id_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para5)
          }
          
          if (!is.null(input$outlier_extra_vars_select_var)) {
              para5 <- data.frame(Check = "outlier",
                                  Parameter = "outlier_extra_vars_select_var", 
                                  Name = "Outlier ID variable", 
                                  Value = c(input$outlier_extra_vars_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para5)
          }
          
          ## Enumerator level
          if (!is.null(input$enumerator_var_select_var)) {
              para6 <- data.frame(Check = "enumerator",
                                  Parameter = "enumerator_var_select_var", 
                                  Name = "Enumerator variable", 
                                  Value = c(input$enumerator_var_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para6)
          }
          
          if (!is.null(input$enumerator_ave_vars_select_var)) {
              para7 <- data.frame(Check = "enumerator",
                                  Parameter = "enumerator_ave_vars_select_var", 
                                  Name = "Enumerator Average Value Variables", 
                                  Value = c(input$enumerator_ave_vars_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para7)
          }
          
          if (!is.null(input$enumerator_date_var_select_var)) {
              para8 <- data.frame(Check = "enumerator",
                                  Parameter = "enumerator_date_var_select_var", 
                                  Name = "Submission Date Variable", 
                                  Value = c(input$enumerator_date_var_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para8)
          }
          
          if (!is.null(input$enumerator_complete_var_select_var)) {
              para9 <- data.frame(Check = "enumerator",
                                  Parameter = "enumerator_complete_var_select_var", 
                                  Name = "Submission Complete Variable", 
                                  Value = c(input$enumerator_complete_var_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para9)
          }
          
          ## Admin level
          if (!is.null(input$admin_var_select_var)) {
              para10 <- data.frame(Check = "admin",
                                   Parameter = "admin_var_select_var", 
                                  Name = "Administrative Unit Variable", 
                                  Value = c(input$admin_var_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para10)
          }
          
          if (!is.null(input$admin_super_vars_select_var)) {
              para11 <- data.frame(Check = "admin",
                                   Parameter = "admin_super_vars_select_var", 
                                  Name = "Higher-Level Administrative Unit Variables", 
                                  Value = c(input$admin_super_vars_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para11)
          }
          
          if (!is.null(input$admin_date_var_select_var)) {
              para12 <- data.frame(Check = "admin",
                                   Parameter = "admin_date_var_select_var", 
                                  Name = "Submission Date Variable", 
                                  Value = c(input$admin_date_var_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para12)
          }
          
          if (!is.null(input$admin_complete_var_select_var)) {
              para13 <- data.frame(Check = "admin",
                                   Parameter = "admin_complete_var_select_var", 
                                  Name = "Submission Complete Variable", 
                                  Value = c(input$admin_complete_var_select_var),
                                  Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para13)
          }
          
          ## Unit of Observation
          if (!is.null(input$unit_var_select_var)) {
              para14 <- data.frame(Check = "unit",
                                   Parameter = "unit_var_select_var", 
                                   Name = "Unit of Observation/ID Variable", 
                                   Value = c(input$unit_var_select_var),
                                   Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para14)
          }
          
          if (!is.null(input$unit_extra_vars_select_var)) {
              para15 <- data.frame(Check = "unit",
                                   Parameter = "unit_extra_vars_select_var", 
                                   Name = "Unit Display Variables", 
                                   Value = c(input$unit_extra_vars_select_var),
                                   Timestamp = format(current_datetime, format = "%d-%b-%Y %I:%M %p"))
              combined_df <- rbind(combined_df, para15)
          }
          
          return(combined_df)
      })
      
      
      
      output$setup_exp_para <- downloadHandler(
          filename = "iehfc_parameters.csv",
          content = function(file) {
              write.csv(parameter_dataset(), file, row.names = TRUE) 
          }
      )
      
      output$setup_exp_para_button <- renderUI({
          downloadButton("setup_exp_para", 
                         label = "Download as csv",
                         icon("download"),
                         class = "btn btn-outline-primary btn-sm")
      })
      

      
      # take you to output tab
      
      observeEvent(input$run_hfcs, {
          updateNavbarPage(session, "tabs", selected = "output_tab")
          Sys.sleep(1)
          showNotification(
              "HFCs were successully run. Please click on the \"Outputs\" tab.",
              duration = 4,
              type     = "message"
          )
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
                                  "This is where you choose which data quality checks to run. 
                                  For each check that you select, a corresponding box will appear on the right. 
                                  Please make sure that the parameters within the box are correct before proceeding. 
                                  Once you are done, click on the \"Run HFCs\" button below.",
                                  placement = "auto"
                              )
                      ),
                      uiOutput("check_select")
                  ),
                  card(
                      uiOutput("setup_run_hfcs_button")
                  ),
                  card(span("Download Parameters", bsicons::bs_icon("question-circle-fill")) %>%
                           tooltip(
                               "Click here to download the selected parameters as a CSV file or to obtain the required template.",
                               placement = "auto"
                           ),
                       uiOutput("setup_exp_para_button")
                  ),
                  card(
                              fileInput( # Import parameters
                                  "parameter_file",
                                  label = span("Import Parameters", bsicons::bs_icon("question-circle-fill")) %>%
                                      tooltip(
                                          "This is optional. If you've saved your parameters in a CSV file, or downloaded it above, 
                                          you can upload the file here. Ensure that your file matches the required template (.csv)",
                                          placement = "auto"
                                      ),
                                  accept = ".csv",
                                  placeholder = "No file selected",
                                  buttonLabel = "Upload"
                              ),
                              span(
                                  "(Optional) Upload the csv parameter file.",
                                  style = "color: #593196; font-size: 12px;"
                              )
                          ),
              ),
              uiOutput("setup_tab_body")
          )
      })
      
      output$setup_tab <- renderUI({
          if(is.null(hfc_dataset())) {
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
                  uiOutput("duplicate_table_dl"),
                  full_screen = TRUE
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
                  uiOutput("outlier_table_dl"),
                  full_screen = TRUE
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
      
        ### Enumerator Outputs ----
      
      output$enumerator_subs_table_output <- renderUI({
          card(
              card_header(
                  span("Submissions by Enumerator", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "Shows the total number of submissions per enumerator and (if a date variable was provided) the number of submissions per enumerator per day.",
                          placement = "auto"
                      )
              ),
              DTOutput("enumerator_subs_table"),
              uiOutput("enumerator_subs_table_dl"),
              full_screen = TRUE
          )
      })
      
      output$enumerator_subs_plot_output <- renderUI({
          card(
              card_header(
                  span("Cumulative Submissions by Enumerator", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "This graph tracks the total number of submissions made by each enumerator over the selected time period. 
                          Each line represents an individual enumerator's cumulative submissions, allowing for a visual comparison of activity levels and workload distribution among the team. 
                          Use this graph to identify trends, outliers, or imbalances in data collection efforts.",
                          placement = "auto"
                      )
              ),
              plotlyOutput("enumerator_daily_subs_plot_rendered"),
              fluidRow(
                  column(3,
                      uiOutput("enumerator_subs_plot_dl_html")
                  ),
                  column(3,
                      uiOutput("enumerator_subs_plot_dl_png")
                  )
              ),
              full_screen = TRUE
          )
      })
      
      output$enumerator_ave_vars_table_output <- renderUI({
          card(
              card_header(
                  span("Variables' Average Value by Enumerator", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "Shows the average value of key variables per enumerator.",
                          placement = "auto"
                      )
              ),
              DTOutput("enumerator_ave_vars_table"),
              uiOutput("enumerator_ave_vars_table_dl"),
              full_screen = TRUE
          )
      })
      
      enumerator_output_components <- reactive({
          case_when(
              enumerator_date_var() != "" & length(enumerator_ave_vars()) > 0 ~ list(
                  c("enumerator_subs_table_output", "enumerator_subs_plot_output", "enumerator_ave_vars_table_output")
              ),
              enumerator_date_var() != "" ~ list(
                  c("enumerator_subs_table_output", "enumerator_subs_plot_output")
              ),
              length(enumerator_ave_vars()) > 0 ~ list(
                  c("enumerator_subs_table_output", "enumerator_ave_vars_table_output")
              ),
              TRUE ~ list("enumerator_subs_table_output")
          ) %>%
          unlist()
      })
      
      output$enumerator_output <- renderUI({
          if("enumerator" %in% selected_checks()) {
              layout_column_wrap(
                  height = "90vh",
                  width = 1,
                  enumerator_output_components() %>%
                   purrr::map(
                       ~ uiOutput(.x)
                   )
              )
          } else {
              "If you would like to see Enumerator-Level Checks, please select \"Enumerator-Level\" in the left-hand sidebar of the \"Check Selection and Setup \" tab."
          }
      })
      
      output$enumerator_subs_table_dl <- renderUI({
          downloadButton("enumerator_subs_table_for_dl", label = "Download Table")
      })
      
      output$enumerator_subs_table_for_dl <- downloadHandler(
          filename = "enumerator_subs_table.csv",
          content = function(file) {
              write.csv(enumerator_subs_dataset(), file, row.names = FALSE)
          }
      )
      
      output$enumerator_subs_plot_dl_html <- renderUI({
          downloadButton("enumerator_subs_plot_for_dl_html", label = "Download Plot (HTML)")
      })
      
      output$enumerator_subs_plot_for_dl_html <- downloadHandler(
          filename = function() {
              paste0("enumerator_subs_plot-", Sys.Date(), ".html")
          },
          content = function(file) {
              plot <- enumerator_daily_subs_plot()
              htmlwidgets::saveWidget(plot, file, selfcontained = TRUE)
          }
      )
     
      
      
      output$enumerator_ave_vars_table_for_dl <- downloadHandler(
          filename = "enumerator_ave_vars_table.csv",
          content = function(file) {
              write.csv(enumerator_ave_vars_dataset(), file, row.names = FALSE)
          }
      )
      
      output$enumerator_ave_vars_table_dl <- renderUI({
          downloadButton("enumerator_ave_vars_table_for_dl", label = "Download Table")
      })
      
        ### Administrative Unit-Level Outputs ----
      
      output$admin_subs_table_output <- renderUI({
          card(
              card_header(
                  span("Submissions by Administrative Unit", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "Shows the total number of submissions per administrative unit and, if a date variable was provided, per day.",
                          placement = "auto"
                      )
              ),
              DTOutput("admin_subs_table"),
              uiOutput("admin_subs_table_dl"),
              full_screen = TRUE
          )
      })
      
      output$admin_subs_plot_output <- renderUI({
          card(
              card_header(
                  span("Cumulative Submissions by Administrative Unit", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "This graph displays the accumulated submissions by the selected Administrative Unit level over time.",
                          placement = "auto"
                      )
              ),
              plotlyOutput("admin_daily_subs_plot_rendered"),
              fluidRow(
                  column(4,
                         uiOutput("admin_subs_plot_dl_html")
                  ),
                  column(4,
                         uiOutput("admin_subs_plot_dl_png")
                  )
              ),
              full_screen = TRUE
          )
      })
      
      admin_output_components <- reactive({
          case_when(
              admin_date_var() != "" ~ list(
                  c("admin_subs_table_output", "admin_subs_plot_output")
              ),
              TRUE ~ list("admin_subs_table_output")
          ) %>%
          unlist()
      })
      
      output$admin_output <- renderUI({
          if("admin" %in% selected_checks()) {
              layout_column_wrap(
                  height = "90vh",
                  width = 1,
                  admin_output_components() %>%
                      purrr::map(
                          ~ uiOutput(.x)
                      )
              )
          } else {
              "If you would like to see Administrative Unit-Level Checks, please select \"Administrative Unit-Level\" in the left-hand sidebar of the \"Check Selection and Setup \" tab."
          }
      })
      
      output$admin_subs_table_for_dl <- downloadHandler(
          filename = "admin_subs_table.csv",
          content = function(file) {
              write.csv(admin_subs_dataset(), file, row.names = FALSE)
          }
      )
      
      output$admin_subs_table_dl <- renderUI({
          downloadButton("admin_subs_table_for_dl", label = "Download Table")
      })
      
      output$admin_subs_plot_dl_html <- renderUI({
          downloadButton("admin_subs_plot_for_dl_html", label = "Download Plot (HTML)")
      })
      
      output$admin_subs_plot_for_dl_html <- downloadHandler(
          filename = function() {
              paste0("admin_subs_plot.html-", Sys.Date(), ".html")
          },
          content = function(file) {
              plot <- admin_daily_subs_plot()
              htmlwidgets::saveWidget(plot, file, selfcontained = TRUE)
          }
      )
      

      
        ### Unit of Observation-Level Outputs ----
      
      output$unit_output <- renderUI({
          if("unit" %in% selected_checks()) {
              card(
                  height = "80vh",
                  DTOutput("unit_table"),
                  uiOutput("unit_table_dl"),
                  full_screen = TRUE
              )
          } else {
              "If you would like to see Unit of Observation-Level Checks, please select \"Unit of Observation-Level\" in the left-hand sidebar of the \"Check Selection and Setup \" tab."
          }
      })
      
      output$unit_table_for_dl <- downloadHandler(
          filename = "unit_table.csv",
          content = function(file) {
              write.csv(unit_dataset(), file, row.names = FALSE)
          }
      )
      
      output$unit_table_dl <- renderUI({
          downloadButton("unit_table_for_dl", label = "Download Table")
      })
      
        ### Output Tab Setup ----
      
      output$output_tab_nodata <- renderUI({
          "Please upload your data in the \"Upload Data\" Tab."
      })
      
      output$output_tab_nocheck <- renderUI({
          "Please complete the \"Check Selection and Setup\" tab and click on the \"Run HFCs\" button to display outputs."
      })
      
      output$output_tab_norun <- renderUI({
          "Please click on the \"Run HFCs\" button in the \"Check Selection and Setup\" tab to display outputs."
      })
      
      output$output_tab_data <- renderUI({
          # Wrap the navset_tab and the download button in a tagList
          tagList(
              # Place the download button at the top
              downloadButton("full_report_dl", "Download Consolidated Report"),
              # Your existing navset_tab structure with panels
              navset_tab(
                  nav_panel("Duplicates", uiOutput("duplicate_output")),
                  nav_panel("Outliers", uiOutput("outlier_output")),
                  nav_panel("Enumerator", uiOutput("enumerator_output")),
                  nav_panel("Admin Level", uiOutput("admin_output")),
                  nav_panel("Tracking", uiOutput("unit_output")),
                  nav_panel("Programming", "Under construction!")
              )
              # If you want the download button at the bottom, move it here after the navset_tab
          )
      })
      
      output$output_tab <- renderUI({
          if(is.null(hfc_dataset())) {
              return(uiOutput("output_tab_nodata"))
          } else if(is.null(input$run_hfcs)) {
              return(uiOutput("output_tab_nocheck"))
          } else if(input$run_hfcs == 0) {
              return(uiOutput("output_tab_norun"))
          } else {
              return(uiOutput("output_tab_data"))
          }
      })
     
      
      
        ### Download consolidated report ----

      output$full_report_dl <- downloadHandler(
          filename = function() {
              paste0("full-report-", Sys.Date(), ".html")
          },
          content = function(file) {
              # 1. Check if 'duplicate' check is selected
              includeDuplicates <- "duplicate" %in% selected_checks()
              duplicatesData <- NULL
              # Prepare the dataset only if duplicates check is selected
              if (includeDuplicates) {
                  # Use isolate to fetch the value of the reactive expression without triggering reactivity
                  duplicatesData <- isolate(duplicate_dataset())
              }
              # 2. Check if 'outlier' check is selected
              includeOutliers <- "outlier" %in% selected_checks()
              
              # Prepare the dataset only if outliers check is selected
              outliersData <- NULL
              if (includeOutliers) {
                  outliersData <- isolate(outlier_dataset())
              }
              
              # 3. Check if 'enumerator' check is selected
              includeEnumerator <- "enumerator" %in% selected_checks()
              
              includeEnumerator <- "enumerator" %in% selected_checks()
              enumeratorSubsData <- NULL 
              enumeratorAveData <- NULL 
              enumeratorPlotPath <- NULL  # Initialize as NULL
              
              if (includeEnumerator) {
                  enumeratorSubsData <- isolate(enumerator_subs_dataset())
                  enumeratorAveData <- isolate(enumerator_ave_vars_dataset())
                  
                  # Generate and save the Plotly plot
                #  plot <- enumerator_daily_subs_plot()  # This function needs to generate a Plotly plot
                 # enumeratorPlotPath <- tempfile(fileext = ".html")  # Temporary HTML file path
                 # saveWidget(plot, enumeratorPlotPath, selfcontained = TRUE)
              }
              
              # 4. Check if 'admin' check is selected
              includeAdmin <- "admin" %in% selected_checks()
              
              # Prepare the dataset only if enum check is selected
              adminData <- NULL
              if (includeAdmin) {
                  adminData <- isolate(admin_subs_dataset())
              }
              
              # 5. Check if 'unit' check is selected
              includeUnit <- "unit" %in% selected_checks()
              
              # Prepare the dataset only if enum check is selected
              unitData <- NULL
              if (includeUnit) {
                  unitData <- isolate(unit_dataset())
              }
              
              
              
              # Render the R Markdown file with parameters
              rmarkdown::render("iehfc_app/server_scripts/template_report.Rmd", output_file = file,
                                params = list(
                                    includeDuplicates = includeDuplicates,
                                    duplicatesData = duplicatesData,
                                    includeOutliers = includeOutliers,
                                    outliersData = outliersData, 
                                    includeEnumerator = includeEnumerator, 
                                    enumeratorSubsData = enumeratorSubsData, 
                                    enumeratorAveData = enumeratorAveData, 
                                    includeAdmin = includeAdmin, 
                                    adminData = adminData, 
                                    includeUnit = includeUnit,
                                    unitData = unitData
                                ),
                                envir = new.env(parent = globalenv()))
          }
      )
      

     
      
      
  }
  
  iehfc_server