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
  