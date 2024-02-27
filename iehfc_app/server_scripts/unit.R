# Unit of Observation-Level Data Quality Checks -- Construction ----

  unit_var <- reactive({
      input$unit_var_select_var
  })
  
  unit_extra_vars <- reactive({
      input$unit_extra_vars_select_var
  })
  
  unit_dataset <- reactive({
      hfc_dataset() %>%
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
      unit_dataset(), fillContainer = TRUE
  )
  