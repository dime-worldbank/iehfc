# Outlier Data Quality Checks -- Construction ----

## Check setup -------------

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
  
 
  ## Table -------------
  
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
  
  
  
  filtered_hfc_dataset <- reactive({
      req(hfc_dataset(), outlier_dataset()) 
      
      outlier_ids <- outlier_dataset() %>%
          pull(!!sym(outlier_id_var())) %>%
          unique()
      
      hfc_dataset() %>%
          filter(!(!!sym(outlier_id_var()) %in% outlier_ids))
  })
  
  
  ## Individual Variables Histograms -------------
  
  #calculate appropriate bin width for each hist
  calculate_bin_width <- function(data, var) {
      x <- data[[var]]
      n <- length(x)
      iqr_val <- IQR(x, na.rm = TRUE)
      bin_width <- 2 * iqr_val / n^(1/3)  
      return(bin_width)
  }
  
  
  
  generate_histogram <- function(dataset, var, bin_width, title_suffix) {
      ggplot(dataset, aes_string(x = var)) +
          geom_histogram(binwidth = bin_width, fill = "#9e83cf", color = "black") +
          labs(title = paste("Histogram of", var, title_suffix), x = var, y = "Frequency") +
          theme_minimal() +
          scale_x_continuous(labels = scales::comma)
  }
  
  
  indiv_combined_histograms <- function(var) {
      bin_width_with <- calculate_bin_width(hfc_dataset(), var)
      hist_with_outliers <- generate_histogram(hfc_dataset(), var, bin_width_with, "with outliers")
      
      bin_width_without <- calculate_bin_width(filtered_hfc_dataset(), var)
      hist_without_outliers <- generate_histogram(filtered_hfc_dataset(), var, bin_width_without, "without outliers")
      
      list(with = ggplotly(hist_with_outliers, tooltip = c("x", "y")),
           without = ggplotly(hist_without_outliers, tooltip = c("x", "y")))
  }
  

  render_histogram_ui <- function(selected_vars) {
      if (length(selected_vars) > 0) {
          fluidRow(
              lapply(1:length(selected_vars), function(i) {
                  var <- selected_vars[i]
                  histograms <- indiv_combined_histograms(var)
                  
                  fluidRow(
                      column(6, plotlyOutput(paste0("histogram_with_", i))),
                      column(6, plotlyOutput(paste0("histogram_without_", i)))
                  ) %>% tagAppendAttributes(style = "margin: 40px 0 40px 0;")
              })
          )
      }
  }
  
  
  output$indiv_combined_histogram_rendered <- renderUI({
      render_histogram_ui(current_indiv_outlier_vars())
  })
  
  
  observe({
      selected_vars <- indiv_outlier_vars()
      if (length(selected_vars) > 0) {
          lapply(1:length(selected_vars), function(i) {
              var <- selected_vars[i]
              histograms <- indiv_combined_histograms(var)
              
              output[[paste0("histogram_with_", i)]] <- renderPlotly(histograms$with)
              output[[paste0("histogram_without_", i)]] <- renderPlotly(histograms$without)
          })
      }
  })
  
  ## Group Variables Boxplots -------------
  
    generate_boxplot <- function(dataset, group, title_suffix) {
      variable_group <- dataset %>%
          select(matches(paste0("^", group, "_{0,1}[0-9]+$"))) %>%
          pivot_longer(cols = everything(), names_to = "issue_var", values_to = "value")
      
      ggplot(variable_group, aes(x = issue_var, y = value)) +
          geom_boxplot(fill = "#9e83cf", color = "black") +
          labs(title = paste("Boxplot for Group", group, title_suffix), x = "Variables", y = "Values") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_y_continuous(labels = scales::comma)
  }
  
  group_combined_boxplots <- function(group) {
      boxplot_with_outliers <- generate_boxplot(hfc_dataset(), group, "with Outliers")
      boxplot_without_outliers <- generate_boxplot(filtered_hfc_dataset(), group, "without Outliers")
      
      list(with = ggplotly(boxplot_with_outliers),
           without = ggplotly(boxplot_without_outliers))
  }
  

  render_boxplot_ui <- function(selected_groups) {
      if (length(selected_groups) > 0) {
          fluidRow(
              lapply(1:length(selected_groups), function(i) {
                  group <- selected_groups[i]
                  boxplots <- group_combined_boxplots(group)
                  
                  fluidRow(
                      column(6, plotlyOutput(paste0("boxplot_with_", i))),
                      column(6, plotlyOutput(paste0("boxplot_without_", i)))
                  ) %>% tagAppendAttributes(style = "margin: 40px 0 40px 0;")
              })
          )
      }
  }
  
  output$group_combined_boxplot_rendered <- renderUI({
      render_boxplot_ui(group_outlier_vars())
  })
  
  observe({
      selected_groups <- group_outlier_vars()
      if (length(selected_groups) > 0) {
          lapply(1:length(selected_groups), function(i) {
              group <- selected_groups[i]
              boxplots <- group_combined_boxplots(group)
              
              output[[paste0("boxplot_with_", i)]] <- renderPlotly(boxplots$with)
              output[[paste0("boxplot_without_", i)]] <- renderPlotly(boxplots$without)
          })
      }
  })
  