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
              "write.csv(unit_dataset, \"unit_table.csv\", row.names = FALSE) \n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  
  
  output$unit_s_exp <- downloadHandler(
      filename = function() {
          "unit_run.do"
      },
      content = function(file) {
          code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for duplicates check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the duplicate variables\n",
              "       local unit_var ", paste0(input$unit_var_select_var, collapse = " "), "\n",
              "       local unit_extra_vars ", paste0(input$unit_extra_vars_select_var, collapse = " "), "\n",
              "\n",
              "    * Sort and identify duplicates\n",
              "       bys `unit_var': gen _unit_order = _n \n",
              "       loc unit_type: type `unit_var' \n",
              "       if regex(\"`unit_type'\", \"str\"){ \n",
              "            ren  `unit_var' `unit_var'__str \n",
              "       }\n",
              "       else tostring `unit_var', gen(`unit_var'__str)\n",
              "\n",
              "       replace `unit_var'__str = `unit_var'__str + \"_\" + string(_unit_order) \n",
              "\n",
              "    * Export to CSV\n",
              "       keep `unit_var'__str `unit_extra_vars' \n",
              "       ren `unit_var'__str `unit_var' \n",
              "       keep `unit_var' `unit_extra_vars' \n",
              "       export delimited using \"unit_table.csv\", replace\n",
              sep = ""
          )
          writeLines(code, file)
      }
  )