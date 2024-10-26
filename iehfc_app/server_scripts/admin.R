# Administrative Unit-Level Data Quality Checks -- Construction ----
  
  admin_var <- reactive({
      input$admin_var_select_var
  })
  
  admin_super_vars <- reactive({
      input$admin_super_vars_select_var
  })
  
  admin_date_var <- reactive({
      input$admin_date_var_select_var
  })
  
  admin_complete_var <- reactive({
      input$admin_complete_var_select_var
  })
  
  admin_total_subs_dataset <- reactive({
      hfc_dataset() %>%
          group_by(
              across(any_of(admin_super_vars())), !!sym(admin_var())
          ) %>%
          summarize(
              num_submissions = n()
          ) %>%
          ungroup()
  }) %>%
  bindEvent(input$run_hfcs)
  
  admin_complete_subs_dataset <- reactive({
      if(admin_complete_var() != "") {
          hfc_dataset() %>%
              group_by(
                  across(any_of(admin_super_vars())), !!sym(admin_var())
              ) %>%
              summarize(
                  num_complete_submissions = sum(
                      across(admin_complete_var(), ~ .x == 1 | .x == "Yes"), na.rm = TRUE
                  )
              ) %>%
              ungroup()
      } else {
          tibble() %>% # So that it merges without error, but does not add information
              mutate(
                  !!admin_var() := case_when(
                      class(hfc_dataset()[[admin_var()]]) == "character" ~ list(NA_character_),
                      class(hfc_dataset()[[admin_var()]]) == "integer"   ~ list(NA_integer_),
                      class(hfc_dataset()[[admin_var()]]) == "numeric"   ~ list(NA_real_),
                      TRUE                                               ~ list(NA)
                  ) %>%
                  unlist()
              )
      }
  }) %>%
  bindEvent(input$run_hfcs)
  
  admin_daily_subs_dataset <- reactive({
      if(admin_date_var() != "") {
          hfc_dataset() %>%
              # Attempt to format date. This may need to be added to depending on reasonable formats to expect
              mutate(
                  date_var_formatted = lubridate::parse_date_time(
                      !!sym(admin_date_var()), c("Y-m-d", "m/d/Y", "d/m/Y")
                  ) %>%
                  as.Date()
              ) %>%
              group_by(
                  across(any_of(admin_super_vars())), !!sym(admin_var()), date_var_formatted
              ) %>%
              summarize(
                  num_submissions = n()
              ) %>%
              ungroup() %>%
              arrange(date_var_formatted) %>% # To ensure that the dates are in the right order
              pivot_wider(
                  names_from  = date_var_formatted,
                  values_from = num_submissions
              )
      } else {
          tibble() %>% # So that it merges without error, but does not add information
              mutate(
                  !!admin_var() := case_when(
                      class(hfc_dataset()[[admin_var()]]) == "character" ~ list(NA_character_),
                      class(hfc_dataset()[[admin_var()]]) == "integer"   ~ list(NA_integer_),
                      class(hfc_dataset()[[admin_var()]]) == "numeric"   ~ list(NA_real_),
                      TRUE                                               ~ list(NA)
                  ) %>%
                  unlist()
              )
      }
  }) %>%
  bindEvent(input$run_hfcs)
  
  admin_daily_subs_plot <- reactive({
      plot_data <- hfc_dataset() %>%
          # Attempt to format date. This may need to be added to depending on reasonable formats to expect
          mutate(
              date_var_formatted = lubridate::parse_date_time(
                  !!sym(admin_date_var()), c("Y-m-d", "m/d/Y", "d/m/Y")
              ) %>%
              as.Date()
          ) %>%
          group_by(
              across(any_of(admin_super_vars())), !!sym(admin_var()), date_var_formatted
          ) %>%
          summarize(
              num_submissions = n()
          ) %>%
          ungroup() %>%
          group_by(
              across(any_of(admin_super_vars())), !!sym(admin_var())
          ) %>%
          mutate(
              cumul_submissions = cumsum(
                  ifelse(is.na(num_submissions), 0, num_submissions) # Need to do this because cumsum() doesn't have an 'na.rm' argument
              )
          ) %>%
          ungroup() %>%
          arrange(date_var_formatted) # To ensure that the dates are in the right order
      
      # Set up highlighting individual admins
      
      admin_daily_subs_ggplot <- plot_data %>%
          mutate(
              !!admin_var() := factor(!!sym(admin_var()))
          ) %>%
          group_by(
              !!sym(admin_var())
          ) %>%
          highlight_key(
              as.formula(
                  paste0("~", admin_var())
              )
          ) %>%
          ggplot() +
          geom_line(
              aes(x = date_var_formatted, y = cumul_submissions, color = !!sym(admin_var()))
          ) +
          labs(
              x = "Date",
              y = "Cumulative # of Submissions"
          ) +
          theme_minimal() +
          theme(
              legend.position = "none"
          )
      
      admin_daily_subs_ggplotly <- ggplotly(admin_daily_subs_ggplot, tooltip = c("color", "y"))
      
      highlight(admin_daily_subs_ggplotly, on = "plotly_hover", off = "plotly_doubleclick")
      
  })
  
  admin_subs_dataset <- reactive({
      admin_total_subs_dataset() %>%
          left_join(admin_complete_subs_dataset()) %>%  # Works because is empty tibble if not "complete" variable is selected
          left_join(admin_daily_subs_dataset())
  }) %>%
  bindEvent(input$run_hfcs)
  
  output$admin_subs_table <- renderDT(
      admin_subs_dataset(), fillContainer = TRUE
  )
  
  output$admin_daily_subs_plot_rendered <- renderPlotly(
      admin_daily_subs_plot()
  )
  
  
  ##### Download admin codes ----
  output$admin_r_exp <- downloadHandler(
      filename = function() {
          "admin_run.R"
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
              "write.csv(admin_dataset, \"admin_table.csv\", row.names = FALSE) \n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  
  
  output$admin_s_exp <- downloadHandler(
      filename = function() {
          "admin_run.do"
      },
      content = function(file) {
          code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for duplicates check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the duplicate variables\n",
              "       local admin_var ", paste0(input$admin_var_select_var, collapse = " "), "\n",
              "       local admin_super_vars ", paste0(input$admin_super_vars_select_var, collapse = " "), "\n",
              "       local admin_date_var ", paste0(input$admin_date_var_select_var, collapse = " "), "\n",
              "       local admin_complete_var ", paste0(input$admin_complete_var_select_var, collapse = " "), "\n",
              "\n",
              "       loc dateformat  \"MDY\" // check the format and change this to DMY or MDY as appropriate\n",
              "\n",
              "       * Check variables\n",
              "            if !mi(\"`admin_complete_var'\") {\n",
              "                qui ta `admin_complete_var'\n",
              "                if r(r)>2 {\n",
              "                    n di as err \"Submission Complete Variable must be a 1/0 variable.\"\n",
              "                    exit\n",
              "                }\n",
              "            }\n",
              "\n",
              "\n",
              "    * Submissions by Admin\n",
              "        preserve \n",
              "            bys `admin_var' `admin_date_var' `admin_super_vars': gen submissions = _N\n",
              "\n",
              "            if !mi(\"`admin_complete_var'\") {\n",
              "                loc complete \"complete\"\n",
              "                loc num_complete_submissions \"num_complete_submissions\"\n",
              "                bys `admin_var' `admin_date_var' `admin_super_vars': egen complete = total(`admin_complete_var')\n",
              "            }\n",
              "\n",
              "        if !mi(\"`admin_date_var'\") {\n",
              "            * Create subdate\n",
              "            cap confirm string var `admin_date_var'\n",
              "            if !_rc {\n",
              "                g admin_subdate = date(`admin_date_var', \"`dateformat'\")\n",
              "            }\n",
              "\n",
              "            cap confirm numeric var `admin_date_var'\n",
              "            if !_rc {\n",
              "                g admin_subdate = `admin_date_var'\n",
              "            }\n",
              "\n",
              "            format admin_subdate %td\n",
              "\n",
              "            collapse (first) submissions `complete', by(`admin_var' `admin_super_vars' admin_subdate)\n",
              "\n",
              "            tempfile admin_data\n", 
			  "            save `admin_data'\n",
			  "\n",
			  "            * Graph\n",
              "            loc admn_type: type `admin_var'\n",
              "            if regex(\"`admn_type'\", \"str\") {\n",
              "                encode `admin_var', gen(`admin_var'_num)\n",
              "            }\n",
              "\n",
			  "            collapse (sum) submissions `complete', by(`admin_var'_num admin_subdate)\n",
			  "\n",
			  "            xtset `admin_var'_num admin_subdate\n",
              "            bysort `admin_var'_num (admin_subdate): gen cum_submissions = sum(submissions)\n",
              "\n",
              "            levelsof admin_subdate, loc(alldates)\n",
              "            xtline cum_submissions, overlay  ///\n",
              "                    title(\"Cumulative Submissions Over Time\") ///\n",
              "                    xtitle(\"Date\") ytitle(\"Cumulative # of Submissions\") ///\n",
              "                    xlab(\"`alldates'\", labsize(small) ang(v)) ///\n",
              "                    legend(off) ///\n",
              "                    name(admin_graph, replace)\n",
              "            graph export \"admin_subs_plot-`=c(current_date)'.png\", as(png) replace width(5000)\n",
              "            gr close admin_graph\n",
              "\n",
			
              "            * Table\n",
			  "            u `admin_data', clear \n",
              "            tostring admin_subdate, replace format(\"%td\") force\n",
              "            reshape wide `complete' submissions, i(`admin_var' `admin_super_vars') j(admin_subdate) str\n",
              "            if !mi(\"`admin_complete_var'\") {\n",
              "                egen num_complete_submissions = rowtotal(complete*)\n",
              "            }\n",
              "\n",
              "            egen num_submissions = rowtotal(submissions*)\n",
              "            ren (submissions*) (d_*) \n",
              "\n",
              "            order `admin_var' `admin_super_vars' num_submissions `num_complete_submissions' d_*\n",
              "        }\n",
              "\n",
              "        if mi(\"`admin_date_var'\") {\n",
              "            collapse (first) num_submissions=submissions `num_complete_submissions'=`complete', by(`admin_var' `admin_super_vars')\n",
              "            order `admin_var' `admin_super_vars' num_submissions `num_complete_submissions'\n",
              "        }\n",
              "\n",
              "        list, table  abbreviate(22)\n",
              "\n",
              "        * Export to CSV\n",
              "        export delimited using \"admin_subs_table.csv\", replace\n",
              "    restore\n",
              sep = ""
          )
          writeLines(code, file)
      }
  )