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
  
  indiv_outlier_dataset <- reactive({
      indiv_outlier_vars() %>%
          map(
              ~ hfc_dataset() %>%
                  # Address any duplicates if there are some remaining, although they should be dealt with by this point
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
                          matches(paste0("^", .x, "$")), ~ sd(.x, na.rm = TRUE), .names = "sd"
                      ),
                      low_limit  = mean - 3 * sd,
                      high_limit = mean + 3 * sd
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
                  select(
                      any_of(outlier_id_var()), any_of(outlier_extra_vars()),
                      issue_var, value = matches(paste0("^", .x, "$")), mean, sd, low_limit, high_limit
                  )
          ) %>%
          list_rbind()
  }) %>%
  bindEvent(input$run_hfcs)
  
  group_outlier_dataset <- reactive({
      group_outlier_vars() %>%
          map(
              ~ hfc_dataset() %>%
                  # Address any duplicates if there are some remaining, although they should be dealt with by this point
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
                      cols         = matches(paste0("^", .x, "_{0,1}[0-9]+$")),
                      names_to     = "issue_var"
                  ) %>%
                  mutate(
                      mean = mean(value, na.rm = TRUE),
                      sd   = sd(value, na.rm = TRUE),
                      low_limit  = mean - 3 * sd,
                      high_limit = mean + 3 * sd
                  ) %>%
                  filter(
                      value < low_limit | value > high_limit
                  ) %>%
                  mutate(
                      across(
                          mean:high_limit, ~ round(.x, digits = 0)
                      )
                  ) %>%
                  select(
                      any_of(outlier_id_var()), any_of(outlier_extra_vars()),
                      issue_var, value, mean, sd, low_limit, high_limit
                  )
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
  