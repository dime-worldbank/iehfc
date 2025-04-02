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
      if (!is.null(admin_date_var()) && admin_date_var() != "") {
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
      if (!is.null(admin_date_var()) && admin_date_var() != "") {
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
      }
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
          # Save the initial script to a temporary file

          initial_script <- system.file("iehfc_app/server_scripts/code_export/admin_run.R", package = "iehfc")

          # Read the initial script content
          initial_content <- readLines(initial_script)

          # Prepend the additional code
          additional_code <- paste(
              "    #----------------------------------------------------\n",
              "    #    This is code sample for admin check. \n",
              "    #---------------------------------------------------- \n",
              "\n",
              "\n",
              "# Load required libraries using pacman\n",
              "if (!requireNamespace(\"pacman\", quietly = TRUE)) {install.packages(\"pacman\")}\n",
              "pacman::p_load(dplyr, tidyr, lubridate, ggplot2, plotly, data.table)\n\n",
              "# Load your dataset\n",
              "# Replace this path with the actual path to your dataset\n",
              "hfc_dataset <- fread(\"C:/path/to/your/file.csv\")\n\n",
              "# Define the admin variables\n",
              "admin_var <- ", paste0("\"", input$admin_var_select_var, "\"", collapse = ", "), "\n",
              "admin_super_vars <- c(", paste0("\"", input$admin_super_vars_select_var, "\"", collapse = ", "), ")\n",
              "admin_date_var <- ", paste0("\"", input$admin_date_var_select_var, "\"", collapse = ", "), "\n",
              "admin_complete_var <- ", paste0("\"", input$admin_complete_var_select_var, "\"", collapse = ", "), "\n",
              "\n",
              sep = ""
          )

          # Combine the additional code and the initial script content
          combined_content <- c(additional_code, initial_content)

          # Write the combined content to the final file
          writeLines(combined_content, file)
      }
  )





  output$admin_s_exp <- downloadHandler(
      filename = function() {
          "admin_run.do"
      },
      content = function(file) {
          # Save the initial script to a temporary file
          initial_script <- system.file("iehfc_app/server_scripts/code_export/admin_run.do", package = "iehfc")

          # Read the initial script content
          initial_content <- readLines(initial_script)

          # Prepend the additional code
          additional_code <- paste(
              "    /*----------------------------------------------------\n",
              "           This is code sample for admin check. \n",
              "    -----------------------------------------------------*/ \n",
              "\n",
              "\n",
              "    * Define the admin variables\n",
              "       local admin_var ", paste0(input$admin_var_select_var, collapse = " "), "\n",
              "       local admin_super_vars ", paste0(input$admin_super_vars_select_var, collapse = " "), "\n",
              "       local admin_date_var ", paste0(input$admin_date_var_select_var, collapse = " "), "\n",
              "       local admin_complete_var ", paste0(input$admin_complete_var_select_var, collapse = " "), "\n",
              "\n",
              sep = ""
          )

          # Combine the additional code and the initial script content
          combined_content <- c(additional_code, initial_content)

          # Write the combined content to the final file
          writeLines(combined_content, file)
      }
  )
