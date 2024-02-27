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
  