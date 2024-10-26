# Duplicate Data Quality Checks -- Construction ----

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
          code <- paste(
              "# Remember to load your dataset \n",
              "hfc_dataset <- \n",
              "\n",
              "# Duplicate Variables \n",
              "duplicate_id_var <- ", paste0("\"", input$duplicate_id_select_var, "\"", collapse = ", "), "\n",
              "duplicate_extra_vars <- c(", paste0("\"", input$duplicate_extra_vars_select_var, "\"", collapse = ", "), ")\n",
              "\n",
              
              "duplicate_dataset <- hfc_dataset %>% \n",
              "    group_by(!!sym(duplicate_id_var)) %>% \n",
              "    filter(n() > 1) %>% \n",
              "    ungroup() %>% \n",
              "    select(all_of(c(duplicate_id_var, duplicate_extra_vars))) \n",
              " \n",
              
              "# Export to CSV  \n",
              "write.csv(duplicate_dataset, \"duplicate_table.csv\", row.names = FALSE) \n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  
  
  output$duplicate_s_exp <- downloadHandler(
      filename = function() {
          "duplicate_run.do"
      },
      content = function(file) {
          code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for duplicates check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the duplicate variables\n",
              "       local duplicate_id_var ", paste0(input$duplicate_id_select_var, collapse = " "), "\n",
              "       local duplicate_extra_vars ", paste0(input$duplicate_extra_vars_select_var, collapse = " "), "\n",
              "\n",
              "    * Sort and identify duplicates\n",
              "       duplicates tag `duplicate_id_var', gen(_dup)\n",
              "\n",
              "    * Filter out duplicates\n",
              "       keep if _dup > 0\n",
              "       sort `duplicate_id_var', stable\n",
              "\n",
              "    * Keep only the relevant columns\n",
              "       keep `duplicate_id_var' `duplicate_extra_vars'\n",
              "       list `duplicate_id_var' `duplicate_extra_vars', sepby(`duplicate_id_var')\n",
              "\n",
              "    * Export to CSV\n",
              "       export delimited using \"duplicate_table.csv\", replace\n",
              sep = ""
          )
          writeLines(code, file)
      }
  )