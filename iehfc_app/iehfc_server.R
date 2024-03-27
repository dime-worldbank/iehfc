

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
          ds <- read.csv(hfc_file()$datapath, na.strings = c("", "NA"))
          hfc_dataset(ds)
      })
      
      observeEvent(input$use_test_data, {
          ds <- read.csv("test_data/LWH_FUP2_raw_data.csv", na.strings = "")
          hfc_dataset(ds)
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
          "Please either upload your dataset using the sidebar on the left or click on the \"Use Test Data\" button."
      })
      
      output$upload_tab_data <- renderUI({
          layout_column_wrap(
              height = "85vh",
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
              class = "btn btn-outline-primary btn-lg"
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
                          style = "color: blue; font-size: 12px;"
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
      
      # Duplicate data quality checks:
          # Duplicate IDs
          # Additional variables for reference
      
      current_duplicate_id_var     <- reactiveVal() # For storing current state of 'duplicate_id_select_var'
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
          selectizeInput(
              "duplicate_id_select_var", label = NULL, 
              choices = hfc_dataset() %>%
                  names,
              selected = current_duplicate_id_var(),
              options = list('dropdownParent' = 'body')
          )
      })
      
      output$duplicate_extra_vars_select <- renderUI({
          selectizeInput(
              "duplicate_extra_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(duplicate_id_var()) # Everything but the ID variable
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(duplicate_extra_vars()),
                      !any_of(duplicate_extra_vars())
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
      
        # Outlier data quality checks: 
          # "Individual" outlier checks -- check individual variable for outliers
          # "Group" outlier checks -- check group of variables (e.g. income for every household member) for outliers
          # Additional variables for reference
      
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
          selectizeInput(
              "indiv_outlier_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(outlier_id_var()) # Everything but the ID variable
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(indiv_outlier_vars()),
                      !any_of(indiv_outlier_vars())
                  ) %>%
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
      
      output$outlier_extra_vars_select <- renderUI({
          selectizeInput(
              "outlier_extra_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(duplicate_id_var()), # Everything but the ID variable or outlier variables
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
      
      output$outlier_setup <- renderUI({
          card(
              height = "30vh", fill = FALSE,
              full_screen = TRUE,
              card_header(
                  span("Outlier Check Setup", bsicons::bs_icon("question-circle-fill")) %>%
                      tooltip(
                          "The outlier check requires you to provide (1) individual variables or groups of variables you want to check for outliers and (2) an ID variable to identify the observation with the outlier. You can add any additional variables you want to include in the output table",
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
      
      output$enumerator_ave_vars_select <- renderUI({
          selectizeInput(
              "enumerator_ave_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(enumerator_var())
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(enumerator_ave_vars()),
                      !any_of(enumerator_ave_vars())
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
                          -all_of(enumerator_var())
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
                          -all_of(enumerator_var())
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
      
      output$admin_super_vars_select <- renderUI({
          selectizeInput(
              "admin_super_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(admin_var())
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(admin_super_vars()),
                      !any_of(admin_super_vars())
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
                          -all_of(admin_var())
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
                          -all_of(admin_var())
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
                          "The administrative unit-level check requires you to provide the variable that identifies the administrative unit and (2) higher-level administrative units that would help either locate or uniquely identify the administrative level of choice. You can include a submission date variable and a \"submission completeness\" variable",
                          placement = "auto"
                      )
              ),
              card_body(
                  fluidRow(
                      column(6,
                             span("Administrative Unit Variable", bsicons::bs_icon("question-circle-fill")) %>%
                                 tooltip("This is the variable that identifies the enumerator for each submission", 
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
      
      output$unit_extra_vars_select <- renderUI({
          selectizeInput(
              "unit_extra_vars_select_var", label = NULL,
              choices = hfc_dataset() %>%
                  select(
                      -all_of(unit_var()) # Everything but the unit of observation variable
                  ) %>%
                  select( # Ensures that selection order is preserved
                      all_of(unit_extra_vars()),
                      !any_of(unit_extra_vars())
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
                             span("Additional Variables", bsicons::bs_icon("question-circle-fill")) %>%
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
              "RUN HFCs",
              icon("paper-plane"),
              class = "btn btn-outline-primary btn-lg"
          )
      })
      
      # take you to output tab
      
      observeEvent(input$run_hfcs, {
          updateTabsetPanel(session,"setup_tab", selected = "output_tab")
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
          if(is.null(hfc_dataset())) {
              return(uiOutput("setup_tab_nodata"))
          } else {
              return(uiOutput("setup_tab_data"))
          }
      })
      
        ### Output Tab ----
      
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
                          "Placeholder",
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
          filename = "enumerator_subs_plot.html",
          content = function(file) {
              # Because this is a plotly object, using htmlWidget::savewidget()
              htmlwidgets::saveWidget(
                  as_widget(test_plotly),
                  file = file,
                  selfcontained = FALSE
              )
          }
      )
      
      output$enumerator_subs_plot_dl_png <- renderUI({
          downloadButton("enumerator_subs_plot_for_dl_png", label = "Download Plot (PNG)")
      })
      
      output$enumerator_subs_plot_for_dl_png <- downloadHandler(
          filename = "enumerator_subs_plot.png",
          content = function(file) {
              # Because this is a plotly object, using htmlWidget::savewidget() and webshot::webshot()
              htmlwidgets::saveWidget(
                  as_widget(test_plotly),
                  file = paste0(tempdir(), "temp.html"), # Saves to specific section's temporary files directory
                  selfcontained = FALSE
              )
              
              webshot::webshot(
                  url = paste0(tempdir(), "temp.html"),
                  file = file
              )
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
                          "Shows the total number of submissions per administrative unit and (if a date variable was provided) the number of submissions per administrative unit per day.",
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
                          "Placeholder",
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
          filename = "admin_subs_plot.html",
          content = function(file) {
              # Because this is a plotly object, using htmlWidget::savewidget()
              htmlwidgets::saveWidget(
                  as_widget(test_plotly),
                  file = file,
                  selfcontained = FALSE
              )
          }
      )
      
      output$admin_subs_plot_dl_png <- renderUI({
          downloadButton("admin_subs_plot_for_dl_png", label = "Download Plot (PNG)")
      })
      
      output$admin_subs_plot_for_dl_png <- downloadHandler(
          filename = "admin_subs_plot.png",
          content = function(file) {
              # Because this is a plotly object, using htmlWidget::savewidget() and webshot::webshot()
              htmlwidgets::saveWidget(
                  as_widget(test_plotly),
                  file = paste0(tempdir(), "temp.html"), # Saves to specific section's temporary files directory
                  selfcontained = FALSE
              )
              
              webshot::webshot(
                  url = paste0(tempdir(), "temp.html"),
                  file = file
              )
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
          navset_tab(
              nav_panel("Duplicates", uiOutput("duplicate_output")),
              nav_panel("Outliers", uiOutput("outlier_output")),
              nav_panel("Enumerator", uiOutput("enumerator_output")),
              nav_panel("Admin Level", uiOutput("admin_output")),
              nav_panel("Tracking", uiOutput("unit_output")),
              nav_panel("Programming", "Under construction!")
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
              
              # Prepare the dataset only if duplicates check is selected
              duplicatesData <- NULL
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
              
              # Prepare the dataset only if enum check is selected
              enumeratorData <- NULL
              if (includeEnumerator) {
                  enumeratorSubsData <- isolate(enumerator_subs_dataset())
                  enumeratorAveData <- isolate(enumerator_ave_vars_dataset())
              }
              
              # 4. Check if 'admin' check is selected
              includeAdmin <- "admin" %in% selected_checks()
              
              # Prepare the dataset only if enum check is selected
              adminData <- NULL
              if (includeAdmin) {
                  adminData <- isolate(admin_subs_dataset())
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
                                    adminData = adminData
                                ),
                                envir = new.env(parent = globalenv()))
          }
      )
      

     
      
      
  }
  
  iehfc_server