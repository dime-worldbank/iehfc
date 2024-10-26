# Outlier Data Quality Checks -- Construction ----

  indiv_outlier_vars <- reactive({
      input$indiv_outlier_vars_select_var
  })
  
  group_outlier_vars <- reactive({
      input$group_outlier_vars_select_var
  })
  
  outlier_id_var <- reactive({
      input$outlier_id_select_var
  })
  
  outlier_extra_vars <- reactive({
      input$outlier_extra_vars_select_var
  })
  
  outlier_method_selected <- reactive({
      input$outlier_method
  })
  
  outlier_multiplier_selected <- reactive({
      input$outlier_multiplier
  })
  
 
  
  
  indiv_outlier_dataset <- reactive({
      method <- outlier_method_selected()
      multiplier <- as.numeric(input$outlier_multiplier)
      indiv_outlier_vars() %>%
          map(
              ~ hfc_dataset() %>%
                  group_by(!!sym(outlier_id_var())) %>%
                  mutate(
                      !!outlier_id_var() := case_when(
                          n() > 1 ~ paste0(!!sym(outlier_id_var()), "_", row_number()),
                          TRUE    ~ as.character(!!sym(outlier_id_var()))
                      )
                  ) %>%
                  ungroup() %>%
                  mutate(
                      across(
                          matches(paste0("^", .x, "$")), ~ mean(.x, na.rm = TRUE), .names = "mean"
                      ),
                      across(
                          matches(paste0("^", .x, "$")), 
                          ~ if (method == "sd") sd(.x, na.rm = TRUE) else NaN, 
                          .names = "sd"
                      ),
                      across(
                          matches(paste0("^", .x, "$")), 
                          ~ if (method == "iqr") IQR(.x, na.rm = TRUE) else NaN, 
                          .names = "iqr"
                      ),
                      across(
                          matches(paste0("^", .x, "$")), 
                          ~ if (method == "iqr") {
                              quantile(.x, probs = 0.25, na.rm = TRUE)
                          } else NaN, 
                          .names = "p25"
                      ),
                      across(
                          matches(paste0("^", .x, "$")), 
                          ~ if (method == "iqr") {
                              quantile(.x, probs = 0.75, na.rm = TRUE)
                          } else NaN, 
                          .names = "p75"
                      ),
                      low_limit = if (method == "sd") {
                          mean - (multiplier * sd)
                      } else {
                          p25 - (multiplier * iqr)
                      },
                      high_limit = if (method == "sd") {
                          mean + (multiplier * sd)
                      } else {
                          p75 + (multiplier * iqr)
                      }
                  ) %>%
                  filter(
                      !!sym(.x) < low_limit | !!sym(.x) > high_limit
                  ) %>%
                  mutate(
                      issue_var = .x,
                      across(
                          mean:high_limit, ~ round(.x, digits = 0)
                      )
                  ) %>%
                  
                {
                  if (method == "sd") {
                      select(., any_of(outlier_id_var()), any_of(outlier_extra_vars()), issue_var, value = matches(paste0("^", .x, "$")), mean, sd, low_limit, high_limit)
                  } else {
                      select(., any_of(outlier_id_var()), any_of(outlier_extra_vars()), issue_var, value = matches(paste0("^", .x, "$")), mean, iqr, low_limit, high_limit)
                  }
                }
              
          ) %>%
          list_rbind()
  }) %>%
  bindEvent(input$run_hfcs)
  
  group_outlier_dataset <- reactive({
      method <- outlier_method_selected()
      multiplier <- as.numeric(input$outlier_multiplier)
      
      group_outlier_vars() %>%
          map(
              ~ hfc_dataset() %>%
                  group_by(!!sym(outlier_id_var())) %>%
                  mutate(
                      !!outlier_id_var() := case_when(
                          n() > 1 ~ paste0(!!sym(outlier_id_var()), "_", row_number()),
                          TRUE    ~ as.character(!!sym(outlier_id_var()))
                      )
                  ) %>%
                  ungroup() %>%
                  select(
                      any_of(outlier_id_var()), any_of(outlier_extra_vars()), matches(paste0("^", .x, "_{0,1}[0-9]+$"))
                  ) %>%
                  pivot_longer(
                      cols = matches(paste0("^", .x, "_{0,1}[0-9]+$")),
                      names_to = "issue_var"
                  ) %>%
                  mutate(
                      mean = mean(value, na.rm = TRUE),
                      sd = sd(value, na.rm = TRUE),
                      iqr = IQR(value, na.rm = TRUE),
                      p25 = if (method == "sd") quantile(value, probs = 0.25, na.rm = TRUE) else NaN,
                      p75 = if (method == "sd") quantile(value, probs = 0.75, na.rm = TRUE) else NaN,
                      low_limit = if (method == "sd") mean - (multiplier * sd) else p25 - (multiplier * iqr),
                      high_limit = if (method == "sd") mean + (multiplier * sd) else p75 + (multiplier * iqr),
                  ) %>%
                  filter(
                      value < low_limit | value > high_limit
                  ) %>%
                  mutate(
                      across(
                          mean:high_limit, ~ round(.x, digits = 0)
                      )
                  ) %>%
                  {
                      if (method == "sd") {
                          select(., any_of(outlier_id_var()), any_of(outlier_extra_vars()), issue_var, value, mean, sd, low_limit, high_limit)
                      } else {
                          select(., any_of(outlier_id_var()), any_of(outlier_extra_vars()), issue_var, value, mean, iqr, low_limit, high_limit)
                      }
                  }
          ) %>%
          list_rbind()
  }) %>%
  bindEvent(input$run_hfcs)
  
  outlier_dataset <- reactive({
      bind_rows(indiv_outlier_dataset(), group_outlier_dataset()) %>%
          arrange(!!sym(outlier_id_var()), issue_var)
  }) %>%
  bindEvent(input$run_hfcs)
  
  output$outlier_table <- renderDT(
      outlier_dataset(), fillContainer = TRUE
  )
  
  
  
  
  ##### Download outlier codes ----
  output$outlier_r_exp <- downloadHandler(
      filename = function() {
          "outlier_run.R"
      },
      content = function(file) {
          code <- paste(
              "# Remember to load your dataset \n",
              "hfc_dataset <- \n",
              "\n",
              "# Outlier Variables \n",
              "indiv_outlier_vars <- ", paste0("\"", input$indiv_outlier_vars_select_var, "\"", collapse = ", "), "\n",
              "group_outlier_vars <- c(", paste0("\"", input$group_outlier_vars_select_var, "\"", collapse = ", "), ")\n",
              "outlier_id_var <- c(", paste0("\"", input$outlier_id_select_var, "\"", collapse = ", "), ")\n",
              "outlier_extra_vars <- c(", paste0("\"", input$outlier_extra_vars_select_var, "\"", collapse = ", "), ")\n",
              "outlier_method_selected <- c(", paste0("\"", input$outlier_method, "\"", collapse = ", "), ")\n",
              "outlier_multiplier_selected <- c(", paste0("\"", input$outlier_multiplier, "\"", collapse = ", "), ")\n",
              "\n",
              
              "duplicate_dataset <- hfc_dataset %>% \n",
              "    group_by(!!sym(duplicate_id_var)) %>% \n",
              "    filter(n() > 1) %>% \n",
              "    ungroup() %>% \n",
              "    select(all_of(c(duplicate_id_var, duplicate_extra_vars))) \n",
              " \n",
              
              "# Export to CSV  \n",
              "write.csv(duplicate_dataset, \"outlier_table.csv\", row.names = FALSE) \n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  
  
  output$outlier_s_exp <- downloadHandler(
      filename = function() {
          "outlier_run.do"
      },
      content = function(file) {
          code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for duplicates check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the outlier variables\n",
              "       local indiv_outlier_vars \"", paste0(input$indiv_outlier_vars_select_var, collapse = " "), "\"\n",
              "       local group_outlier_vars \"", paste0(input$group_outlier_vars_select_var, collapse = " "), "\"\n",
              "       local outlier_id_var \"", paste0(input$outlier_id_select_var, collapse = " "), "\"\n",
              "       local outlier_extra_vars \"", paste0(input$outlier_extra_vars_select_var, collapse = " "), "\"\n",
              "       local outlier_method_selected \"", paste0(input$outlier_method, collapse = " "), "\"\n",
              "       local outlier_multiplier_selected \"", paste0(input$outlier_multiplier, collapse = " "), "\"\n",
              "\n",
              
              "    * Create unique id, in case the id variable is not unique\n",
              "    tostring `outlier_id_var', replace \n",
              "    bysort `outlier_id_var': gen outlier_id = `outlier_id_var' + \"-\" + string(_n) if _N > 1 \n",
              "    replace outlier_id = `outlier_id_var' if mi(outlier_id) \n",
              "\n",
              "    * Create list of group variables\n",
              "    foreach var of loc group_outlier_vars { \n",
              "        unab fullvarlist : `var'* \n",
              "        loc groupvarlist = \"`groupvarlist' \" + \"`fullvarlist'\" \n",
              "    } \n",
              "\n",
              "    * Create a tempfile to store the outlier values\n",
              "    preserve \n",
              "        clear \n",
              "        tempfile outlier_file \n",
              "        save `outlier_file', replace emptyok \n",
              "    restore \n",
              "\n",
              "    * List of outliers for individual variables\n",
              "    foreach var of loc indiv_outlier_vars { \n",
              "        preserve \n",
              "            keep outlier_id `outlier_id_var' `outlier_extra_vars' `var' \n",
              "\n",
              "            if \"`outlier_method_selected'\" == \"sd\" { \n",
              "                qui sum `var' \n",
              "\n",
              "                g sd = r(sd) \n",
              "                g low_limit = r(mean) - (`outlier_multiplier_selected' * r(sd)) \n",
              "                g high_limit = r(mean) + (`outlier_multiplier_selected' * r(sd)) \n",
              "                g mean = r(mean) \n",
              "                g issue_var = \"`var'\" \n",
              "                g value = `var' \n",
              "            } \n",
              "            else { \n",
              "                egen iqr = iqr(`var') \n",
              "                qui sum `var', d \n",
              "\n",
              "                g low_limit = r(p25) - (`outlier_multiplier_selected' * iqr) \n",
              "                g high_limit = r(p75) + (`outlier_multiplier_selected' * iqr) \n",
              "                g mean = r(mean) \n",
              "                g issue_var = \"`var'\" \n",
              "                g value = `var' \n",
              "            } \n",
              "\n",
              "            keep if !inrange(value, low_limit, high_limit) & !mi(value) \n",
              "            keep `outlier_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit \n",
              "\n",
              "            append using `outlier_file', nolabel nonotes \n",
              "            save `outlier_file', replace \n",
              "        restore \n",
              "    } \n",
              "\n",
              "    * List of outliers for group variables\n",
              "    foreach var of loc group_outlier_vars { \n",
              "        preserve \n",
              "            keep outlier_id `outlier_id_var' `outlier_extra_vars' `var'* \n",
              "            reshape long `var'_, i(outlier_id) j(outliersl) \n",
              "\n",
              "            if \"`outlier_method_selected'\" == \"sd\" { \n",
              "                qui sum `var' \n",
              "\n",
              "                g sd = r(sd) \n",
              "                g low_limit = r(mean) - (`outlier_multiplier_selected' * r(sd)) \n",
              "                g high_limit = r(mean) + (`outlier_multiplier_selected' * r(sd)) \n",
              "                g mean = r(mean) \n",
              "                g issue_var = \"`var'\" \n",
              "                g value = `var' \n",
              "            } \n",
              "            else { \n",
              "                egen iqr = iqr(`var') \n",
              "                qui sum `var', d \n",
              "\n",
              "                g low_limit = r(p25) - (`outlier_multiplier_selected' * iqr) \n",
              "                g high_limit = r(p75) + (`outlier_multiplier_selected' * iqr) \n",
              "                g mean = r(mean) \n",
              "                g issue_var = \"`var'\" \n",
              "                g value = `var' \n",
              "            } \n",
              "\n",
              "            keep if !inrange(value, low_limit, high_limit) & !mi(value) \n",
              "            replace issue_var = \"`var'_\" + string(outliersl) \n",
              "\n",
              "            keep `outlier_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit \n",
              "            append using `outlier_file', nolabel nonotes \n",
              "            save `outlier_file', replace \n",
              "        restore \n",
              "    } \n",
              "\n",
              "    u `outlier_file', clear \n",
              "    sort issue_var `outlier_id_var', stable \n",
              "    list `outlier_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit, sepby(issue_var) \n",
              "    order `outlier_id_var' `outlier_extra_vars' issue_var value  mean `outlier_method_selected' low_limit high_limit \n",
              "\n",
              "    * Export to CSV\n",
              "    export delimited using \"outlier_table.csv\", replace \n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  
  