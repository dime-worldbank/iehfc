# Enumerator Data Quality Checks -- Construction ----

  enumerator_var <- reactive({
      input$enumerator_var_select_var
  })
  
  enumerator_ave_vars <- reactive({
      input$enumerator_ave_vars_select_var
  })
  
  enumerator_date_var <- reactive({
      input$enumerator_date_var_select_var
  })
  
  enumerator_complete_var <- reactive({
      input$enumerator_complete_var_select_var
  })
  
  enumerator_total_subs_dataset <- reactive({
      hfc_dataset() %>%
          group_by(!!sym(enumerator_var())) %>%
          summarize(
              num_submissions = n()
          ) %>%
          ungroup()
  }) %>%
  bindEvent(input$run_hfcs)
  
  enumerator_complete_subs_dataset <- reactive({
      if(enumerator_complete_var() != "") {
          hfc_dataset() %>%
              group_by(!!sym(enumerator_var())) %>%
              summarize(
                  num_complete_submissions = sum(
                      across(enumerator_complete_var(), ~ .x == 1 | .x == "Yes"), na.rm = TRUE
                  )
              ) %>%
              ungroup()
      } else {
          tibble() %>% # So that it merges without error, but does not add information
              mutate(
                  !!enumerator_var() := case_when(
                      class(hfc_dataset()[[enumerator_var()]]) == "character" ~ list(NA_character_),
                      class(hfc_dataset()[[enumerator_var()]]) == "integer"   ~ list(NA_integer_),
                      class(hfc_dataset()[[enumerator_var()]]) == "numeric"   ~ list(NA_real_),
                      TRUE                                                    ~ list(NA)
                  ) %>%
                  unlist()
              )
      }
  }) %>%
  bindEvent(input$run_hfcs)
  
  enumerator_daily_subs_dataset <- reactive({
      if(enumerator_date_var() != "") {
          hfc_dataset() %>%
              # Attempt to format date. This may need to be added to depending on reasonable formats to expect
              mutate(
                  date_var_formatted = lubridate::parse_date_time(
                      !!sym(enumerator_date_var()), c("Y-m-d", "m/d/Y", "d/m/Y")
                  ) %>%
                  as.Date()
              ) %>%
              group_by(
                  !!sym(enumerator_var()), date_var_formatted
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
                  !!enumerator_var() := case_when(
                      class(hfc_dataset()[[enumerator_var()]]) == "character" ~ list(NA_character_),
                      class(hfc_dataset()[[enumerator_var()]]) == "integer"   ~ list(NA_integer_),
                      class(hfc_dataset()[[enumerator_var()]]) == "numeric"   ~ list(NA_real_),
                      TRUE                                                    ~ list(NA)
                  ) %>%
                  unlist()
              )
      }
  }) %>%
  bindEvent(input$run_hfcs)
  
  enumerator_daily_subs_plot <- reactive({
      plot_data <- hfc_dataset() %>%
          # Attempt to format date. This may need to be added to depending on reasonable formats to expect
          mutate(
              date_var_formatted = lubridate::parse_date_time(
                  !!sym(enumerator_date_var()), c("Y-m-d", "m/d/Y", "d/m/Y")
              ) %>%
              as.Date()
          ) %>%
          group_by(
              !!sym(enumerator_var()), date_var_formatted
          ) %>%
          summarize(
              num_submissions = n()
          ) %>%
          ungroup() %>%
          group_by(
              !!sym(enumerator_var())
          ) %>%
          mutate(
              cumul_submissions = cumsum(
                  ifelse(is.na(num_submissions), 0, num_submissions) # Need to do this because cumsum() doesn't have an 'na.rm' argument
              )
          ) %>%
          ungroup() %>%
          arrange(date_var_formatted) # To ensure that the dates are in the right order
      
      # Set up highlighting individual enumerators
      
      enumerator_daily_subs_ggplot <- plot_data %>%
          mutate(
              !!enumerator_var() := factor(!!sym(enumerator_var()))
          ) %>%
          group_by(!!sym(enumerator_var())) %>%
          highlight_key(
              as.formula(
                  paste0("~", enumerator_var())
              )
          ) %>%
          ggplot() +
          geom_line(
              aes(x = date_var_formatted, y = cumul_submissions, color = !!sym(enumerator_var()))
          ) +
          labs(
              x = "Date",
              y = "Cumulative # of Submissions"
          ) +
          theme_minimal() +
          theme(
              legend.position = "none"
          )
      
      enumerator_daily_subs_ggplotly <- ggplotly(enumerator_daily_subs_ggplot, tooltip = c("color", "y"))
      
      highlight(enumerator_daily_subs_ggplotly, on = "plotly_hover", off = "plotly_doubleclick")
        
  })
  
  enumerator_subs_dataset <- reactive({
      enumerator_total_subs_dataset() %>%
          left_join(enumerator_complete_subs_dataset()) %>%  # Works because is empty tibble if not "complete" variable is selected
          left_join(enumerator_daily_subs_dataset())
  }) %>%
  bindEvent(input$run_hfcs)
  
  enumerator_ave_vars_dataset <- reactive({
      hfc_dataset() %>%
          group_by(!!sym(enumerator_var())) %>%
          summarize(
              across(
                  all_of(enumerator_ave_vars()),
                  ~ round(mean(.x, na.rm = TRUE), digits = 2)
              )
          ) %>%
          ungroup()
  }) %>%
  bindEvent(input$run_hfcs)
  
  output$enumerator_subs_table <- renderDT(
      enumerator_subs_dataset(), fillContainer = TRUE
  )
  
  output$enumerator_daily_subs_plot_rendered <- renderPlotly(
      enumerator_daily_subs_plot()
  )
  
  output$enumerator_ave_vars_table <- renderDT(
      enumerator_ave_vars_dataset(), fillContainer = TRUE
  )
  
  
  ##### Download enumerator codes ----
  output$enumerator_r_exp <- downloadHandler(
      filename = function() {
          "enumerator_run.R"
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
              "write.csv(duplicate_dataset, \"enumerator_table.csv\", row.names = FALSE) \n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  
  
  output$enumerator_s_exp <- downloadHandler(
      filename = function() {
          "enumerator_run.do"
      },
      content = function(file) {
          code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for duplicates check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the duplicate variables\n",
              "       local enumerator_var ", paste0(input$enumerator_var_select_var, collapse = " "), "\n",
              "       local enumerator_ave_vars ", paste0(input$enumerator_ave_vars_select_var, collapse = " "), "\n",
              "       local enumerator_date_var ", paste0(input$enumerator_date_var_select_var, collapse = " "), "\n",
              "       local enumerator_complete_var ", paste0(input$enumerator_complete_var_select_var, collapse = " "), "\n",
              "\n",
              "       loc dateformat  \"MDY\" // check the format and change this to DMY or MDY as appropriate\n",
              "\n",
              "       * Check variables\n",
              "            if !mi(\"`enumerator_complete_var'\") {\n",
              "                qui ta `enumerator_complete_var'\n",
              "                if r(r)>2 {\n",
              "                    n di as err \"Submission Complete Variable must be a 1/0 variable.\"\n",
              "                    exit\n",
              "                }\n",
              "            }\n",
              "\n",
              "            foreach var in `enumerator_ave_vars' `enumerator_complete_var' {\n",
              "                confirm numeric var `var'\n",
              "            }\n",
              "\n",
              "\n",
              "\n",
              "    * Submissions by Enumerator\n",
              "        preserve \n",
              "            bys `enumerator_var' `enumerator_date_var': gen submissions = _N\n",
              "\n",
              "            if !mi(\"`enumerator_complete_var'\") {\n",
              "                loc complete \"complete\"\n",
              "                loc num_complete_submissions \"num_complete_submissions\"\n",
              "                bys `enumerator_var' `enumerator_date_var': egen complete = total(`enumerator_complete_var')\n",
              "            }\n",
              "\n",
              "            if !mi(\"`enumerator_date_var'\") {\n",
              "                * Create subdate\n",
              "                cap confirm string var `enumerator_date_var'\n",
              "                if !_rc {\n",
              "                    g enum_subdate = date(`enumerator_date_var', \"`dateformat'\")\n",
              "                }\n",
              "\n",
              "                cap confirm numeric var `enumerator_date_var'\n",
              "                if !_rc {\n",
              "                    g enum_subdate = `enumerator_date_var'\n",
              "                }\n",
              "\n",
              "                format enum_subdate %td\n",
              "\n",
              "                collapse (first) submissions `complete', by(`enumerator_var' enum_subdate)\n",
              "\n",
              "                * Graph\n",
              "                xtset `enumerator_var' enum_subdate\n",
              "                bysort `enumerator_var' (enum_subdate): gen cum_submissions = sum(submissions)\n",
              "\n",
              "                levelsof enum_subdate, loc(alldates)\n",
              "                xtline cum_submissions, overlay  /// \n",
              "                        title(\"Cumulative Submissions Over Time\") /// \n",
              "                        xtitle(\"Date\") ytitle(\"Cumulative # of Submissions\") ///\n",
              "                        xlab(\"`alldates'\", labsize(small) ang(v)) ///\n",
              "                        legend(off) ///\n",
              "                        name(enum_graph, replace)\n",
              "                graph export \"enumerator_subs_plot-`=c(current_date)'.png\", as(png) replace width(5000)\n",
              "                gr close enum_graph\n",
              "                drop cum_submissions\n",
              "\n",
              "                * Table\n",
              "                tostring enum_subdate, replace format(\"%td\") force\n",
              "                reshape wide `complete' submissions, i(`enumerator_var') j(enum_subdate) str\n",
              "                if !mi(\"`enumerator_complete_var'\") {\n",
              "                    egen num_complete_submissions = rowtotal(complete*)\n",
              "                }\n",
              "\n",
              "                egen num_submissions = rowtotal(submissions*)\n",
              "                ren (submissions*) (d_*) \n",
              "\n",
              "                order `enumerator_var' num_submissions `num_complete_submissions' d_*\n",
              "            }\n",
              "\n",
              "            if mi(\"`enumerator_date_var'\") {\n",
              "                collapse (first) num_submissions=submissions `num_complete_submissions'=`complete', by(`enumerator_var')\n",
              "                order `enumerator_var' num_submissions `num_complete_submissions'\n",
              "            }\n",
              "\n",
              "            list, table  abbreviate(22)\n",
              "\n",
              "            * Export to CSV\n",
              "            export delimited using \"enumerator_subs_table.csv\", replace\n",
              "        restore\n",
              "\n",
              "\n",
              "    * Variables' Average Value by Enumerator\n",
              "        preserve \n",
              "            collapse (mean) `enumerator_ave_vars', by(`enumerator_var')\n",
              "\n",
              "            list, table  abbreviate(22)\n",
              "\n",
              "            * Export to CSV\n",
              "            export delimited using \"enumerator_ave_vars_table.csv\", replace\n",
              "        restore\n",
              sep = ""
          )
          writeLines(code, file)
      }
  )
  