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
          ungroup()
      
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
  