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
  if (!is.null(enumerator_date_var()) && enumerator_date_var() != "") {
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
  if (!is.null(enumerator_date_var()) && enumerator_date_var() != "") {
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

    enumerator_daily_subs_ggplotly <- ggplotly(enumerator_daily_subs_ggplot, tooltip = c("color", "y"), width = NULL)

    highlight(enumerator_daily_subs_ggplotly, on = "plotly_hover", off = "plotly_doubleclick")
  }
})

enumerator_subs_dataset <- reactive({
  enumerator_total_subs_dataset() %>%
    left_join(enumerator_complete_subs_dataset()) %>%  # Works because is empty tibble if not "complete" variable is selected
    left_join(enumerator_daily_subs_dataset())
}) %>%
  bindEvent(input$run_hfcs)



enumerator_ave_vars_dataset <- reactive({
  data <- hfc_dataset()
  vars <- enumerator_ave_vars()
  enum_var <- enumerator_var()

  if (is.null(vars) || length(vars) == 0 || !all(vars %in% names(data)) || !(enum_var %in% names(data))) {
    return(NULL)
  }

  avg_list <- list()
  out_range_list <- list()
  for (var in vars) {
    avg_col <- data %>%
      group_by(!!sym(enum_var)) %>%
      summarize(
        "{var}_avg" := round(mean(.data[[var]], na.rm = TRUE), digits = 2),
        .groups = "drop"
      )
    avg_list[[var]] <- avg_col

    limits <- enumerator_ave_vars_limits[[var]]
    min_val <- suppressWarnings(as.numeric(limits$min))
    max_val <- suppressWarnings(as.numeric(limits$max))

    if (is.na(min_val)) {
      min_val <- suppressWarnings(suppressMessages(min(data[[var]], na.rm = TRUE)))
    }
    if (is.na(max_val)) {
      max_val <- suppressWarnings(suppressMessages(max(data[[var]], na.rm = TRUE)))
    }

    # Change: column name now includes (%) and value is numeric
    col_name <- paste0(var, "_out_of_range (%)")
    if (!is.na(min_val) && !is.na(max_val)) {
      out_col <- data %>%
        group_by(!!sym(enum_var)) %>%
        summarize(
          n_total = sum(!is.na(.data[[var]])),
          n_out = sum((.data[[var]] < min_val | .data[[var]] > max_val) & !is.na(.data[[var]])),
          .groups = "drop"
        ) %>%
        mutate(
          "{col_name}" := ifelse(n_total > 0, round(100 * n_out / n_total, 2), NA_real_)
        ) %>%
        select(-n_total, -n_out)
    } else {
      out_col <- data %>%
        group_by(!!sym(enum_var)) %>%
        summarize(
          "{col_name}" := NA_real_,
          .groups = "drop"
        )
    }
    out_range_list[[var]] <- out_col
  }

  final_df <- data %>%
    distinct(!!sym(enum_var)) 

  for (var in vars) {
    final_df <- final_df %>%
      left_join(avg_list[[var]], by = enum_var) %>%
      left_join(out_range_list[[var]], by = enum_var)
  }

  final_df
}) %>%
  bindEvent(input$run_hfcs)



output$enumerator_subs_table <- renderDT(
  enumerator_subs_dataset(), fillContainer = TRUE
)

output$enumerator_daily_subs_plot_rendered <- renderPlotly(
  enumerator_daily_subs_plot()
)

output$enumerator_ave_vars_table <- renderDT({
  df <- enumerator_ave_vars_dataset()
  if (is.null(df)) return(NULL)

  # Identify out_of_range columns (now with (%) in name)
  out_of_range_cols <- grep("_out_of_range \\(\\%\\)$", names(df), value = TRUE)

  dt <- datatable(
    df,
    fillContainer = TRUE,
    options = list(scrollX = TRUE),
    class = "custom-dt"
  )

  # Color ramp for 0-100%
  color_ramp <- colorRampPalette(c("#ffffff", "#ffffb2", "#fecc5c", "#fd8d3c", "#d7301f"))(101)

  for (col in out_of_range_cols) {
    dt <- dt %>% formatStyle(
      columns = col,
      backgroundColor = styleInterval(
        seq(0, 100, length.out = 101)[-1], # 0-100 breaks
        color_ramp
      )
    )
  }

  shiny::tags$head(
    shiny::tags$style(HTML("
      table.dataTable.custom-dt td {
        background-color: inherit !important;
      }
    "))
  )

  dt
})



output$enumerator_r_exp <- downloadHandler(
  filename = function() {
    "enumerator_run.R"
  },
  content = function(file) {
    # Save the initial script to a temporary file
    initial_script <- system.file("iehfc_app/server_scripts/code_export/enumerator_run.R", package = "iehfc")

    # Read the initial script content
    initial_content <- readLines(initial_script)

    # Prepend the additional code
    additional_code <- paste(
      "    #----------------------------------------------------\n",
      "    #    This is code sample for enumerator check. \n",
      "    #---------------------------------------------------- \n",
      "\n",
      "\n",
      "# Load required libraries using pacman\n",
      "if (!requireNamespace(\"pacman\", quietly = TRUE)) {install.packages(\"pacman\")}\n",
      "pacman::p_load(dplyr, tidyr, lubridate, ggplot2, plotly, data.table)\n\n",
      "# Load your dataset\n",
      "# Replace this path with the actual path to your dataset\n",
      "hfc_dataset <- fread(\"C:/path/to/your/file.csv\")\n\n",
      "# Define the enumerator variables\n",
      "enumerator_var <- ", paste0("\"", input$enumerator_var_select_var, "\"", collapse = " "), "\n",
      "enumerator_ave_vars <- c(", paste0("\"", input$enumerator_ave_vars_select_var, "\"", collapse = ", "), ")\n",
      "enumerator_date_var <- ", paste0("\"", input$enumerator_date_var_select_var, "\"", collapse = " "), "\n",
      "enumerator_complete_var <- ", paste0("\"", input$enumerator_complete_var_select_var, "\"", collapse = " "), "\n",
      "# Add min/max value variables for each enumerator_ave_var\n",
      paste0(
        "enumerator_ave_vars_limits <- list(\n",
        paste(
          sapply(input$enumerator_ave_vars_select_var, function(var) {
            min_val <- enumerator_ave_vars_limits[[var]]$min
            max_val <- enumerator_ave_vars_limits[[var]]$max
            paste0("  ", var, " = list(min = ", min_val, ", max = ", max_val, ")")
          }),
          collapse = ",\n"
        ),
        "\n)\n"
      ),
      sep = ""
    )

    # Combine the additional code and the initial script content
    combined_content <- c(additional_code, initial_content)

    # Write the combined content to the final file
    writeLines(combined_content, file)
  }
)


output$enumerator_s_exp <- downloadHandler(
  filename = function() {
    "enumerator_run.do"
  },
  content = function(file) {

    # Save the initial script to a temporary file
    initial_script <- system.file("iehfc_app/server_scripts/code_export/enumerator_run.do", package = "iehfc")

    # Read the initial script content
    initial_content <- readLines(initial_script)

    # Prepend the additional code
    additional_code <- paste(
      "    /*----------------------------------------------------\n",
      "           This is code sample for enumerator check. \n",
      "    -----------------------------------------------------*/ \n",
      "\n",
      "\n",
      "    * Define the enumerator variables\n",
      "       local enumerator_var ", paste0(input$enumerator_var_select_var, collapse = " "), "\n",
      "       local enumerator_ave_vars ", paste0(input$enumerator_ave_vars_select_var, collapse = " "), "\n",
      "       local enumerator_date_var ", paste0(input$enumerator_date_var_select_var, collapse = " "), "\n",
      "       local enumerator_complete_var ", paste0(input$enumerator_complete_var_select_var, collapse = " "), "\n",
      "\n",
      "    * Add min/max value locals for each enumerator_ave_var\n",
      paste0(
        paste(
          sapply(input$enumerator_ave_vars_select_var, function(var) {
            min_val <- enumerator_ave_vars_limits[[var]]$min
            max_val <- enumerator_ave_vars_limits[[var]]$max
            paste0("       local min_", var, " = ", min_val, "\n",
                   "       local max_", var, " = ", max_val)
          }),
          collapse = "\n"
        ),
        if (length(input$enumerator_ave_vars_select_var) > 0) "\n" else ""
      ),
      sep = ""
    )

    # Combine the additional code and the initial script content
    combined_content <- c(additional_code, initial_content)

    # Write the combined content to the final file
    writeLines(combined_content, file)
  }
)

