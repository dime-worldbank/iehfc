# Unit of Observation-Level Data Quality Checks -- Construction ----

  unit_var <- reactive({
      input$unit_var_select_var
  })
  
  unit_extra_vars <- reactive({
      input$unit_extra_vars_select_var
  })
  
  unit_dataset <- reactive({
      hfc_dataset() %>%
          # Address any duplicates if there are some remaining, although they should be dealt with by this point
          group_by(!!sym(unit_var())) %>%
          mutate(
              !!unit_var() := case_when(
                  n() > 1 ~ paste0(!!sym(unit_var()), "_", row_number()),
                  TRUE    ~ as.character(!!sym(unit_var()))
              )
          ) %>%
          ungroup() %>%
          select(
              all_of(
                  c(unit_var(), unit_extra_vars())
              )
          ) %>%
          arrange(
              !!sym(unit_var())
          )
  })
  
  output$unit_table <- renderDT(
      unit_dataset(), fillContainer = TRUE, options = list(paging = FALSE)
  )
  
  
  ##### Download unit codes ----
  output$unit_r_exp <- downloadHandler(
      filename = function() {
          "unit_run.R"
      },
      content = function(file) {
          # Save the initial script to a temporary file
          initial_script <- "iehfc_app/server_scripts/code_export/unit_run.R"
          
          # Read the initial script content
          initial_content <- readLines(initial_script)
          
          # Prepend the additional code
          additional_code <- paste(
              "    #----------------------------------------------------\n",
              "    #    This is code sample for unit check. \n",
              "    #---------------------------------------------------- \n",
              "\n",
              "\n",
              "# Remember to load your dataset \n",
              "hfc_dataset <- \n",
              "\n",
              "# Unit Variables \n",
              "unit_var <- ", paste0("\"", input$unit_var_select_var, "\"", collapse = ", "), "\n",
              "unit_extra_vars <- c(", paste0("\"", input$unit_extra_vars_select_var, "\"", collapse = ", "), ")\n",
              "\n",
              sep = ""
          )
          
          # Combine the additional code and the initial script content
          combined_content <- c(additional_code, initial_content)
          
          # Write the combined content to the final file
          writeLines(combined_content, file)
      }
  )
  
  
  
  
  
  output$unit_s_exp <- downloadHandler(
      filename = function() {
          "unit_run.do"
      },
      content = function(file) {
          # Save the initial script to a temporary file
          initial_script <- "iehfc_app/server_scripts/code_export/unit_run.do"
          
          # Read the initial script content
          initial_content <- readLines(initial_script)
          
          # Prepend the additional code
          additional_code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for unit check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the unit variables\n",
              "       local unit_var ", paste0(input$unit_var_select_var, collapse = " "), "\n",
              "       local unit_extra_vars ", paste0(input$unit_extra_vars_select_var, collapse = " "), "\n",
              "\n",
              sep = ""
          )
          
          # Combine the additional code and the initial script content
          combined_content <- c(additional_code, initial_content)
          
          # Write the combined content to the final file
          writeLines(combined_content, file)
      }
  )
  