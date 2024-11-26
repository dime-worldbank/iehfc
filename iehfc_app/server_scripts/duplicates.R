# Duplicate Data Quality Checks -- Construction ----

  duplicate_var <- reactive({
      input$duplicate_select_var
  })
  
  duplicate_extra_vars <- reactive({
      input$duplicate_extra_vars_select_var
  })
  


  duplicate_dataset <- reactive({
      hfc_dataset() %>%
          group_by(!!sym(selected_id_var())) %>%
          filter(n() > 1) %>%
          ungroup() %>%
          select(
              all_of(
                  c(selected_id_var(), duplicate_extra_vars())
              )
          )
  }) %>%
      bindEvent(input$run_hfcs)
  
  output$duplicate_table <- renderDT(
      duplicate_dataset(), fillContainer = TRUE
  )
  
  ##### Download duplicate codes ----
  output$duplicate_r_exp <- downloadHandler(
      filename = function() {
          "duplicate_run.R"
      },
      content = function(file) {
          # Save the initial script to a temporary file
          initial_script <- "iehfc_app/server_scripts/code_export/duplicate_run.R"
          
          # Read the initial script content
          initial_content <- readLines(initial_script)
          
          # Prepend the additional code
          additional_code <- paste(
              "    #----------------------------------------------------\n",
              "    #    This is code sample for duplicates check. \n",
              "    #---------------------------------------------------- \n",
              "\n",
              "\n",
              "# Load required libraries using pacman\n",
              "if (!requireNamespace(\"pacman\", quietly = TRUE)) {install.packages(\"pacman\")}\n",
              "pacman::p_load(dplyr, data.table)\n\n",
              "# Load your dataset\n",
              "# Replace this path with the actual path to your dataset\n",
              "hfc_dataset <- fread(\"C:/path/to/your/file.csv\")\n\n",
              "# Define the duplicate variables\n",
              "selected_id_var <- \"", input$id_select_var, "\"\n",
              "duplicate_extra_vars <- c(", paste0("\"", input$duplicate_extra_vars_select_var, "\"", collapse = ", "), 
              ")\n\n",
              sep = ""
          )
          
          # Combine the additional code and the initial script content
          combined_content <- c(additional_code, initial_content)
          
          # Write the combined content to the final file
          writeLines(combined_content, file)
      }
  )
  
  
  
  
  
  output$duplicate_s_exp <- downloadHandler(
      filename = function() {
          "duplicate_run.do"
      },
      content = function(file) {
          # Save the initial script to a temporary file
          initial_script <- "iehfc_app/server_scripts/code_export/duplicate_run.do"
          
          # Read the initial script content
          initial_content <- readLines(initial_script)
          
          # Prepend the additional code
          additional_code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for duplicates check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the duplicate variables\n",
              "       local selected_id_var ", paste0(input$id_select_var, collapse = " "), "\n",
              "       local duplicate_extra_vars ", paste0(input$duplicate_extra_vars_select_var, collapse = " "), "\n",
              "\n",
              sep = ""
          )
          
          # Combine the additional code and the initial script content
          combined_content <- c(additional_code, initial_content)
          
          # Write the combined content to the final file
          writeLines(combined_content, file)
      }
  )
  