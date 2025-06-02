enumerator_total_subs_dataset <- 
    hfc_dataset %>%
    group_by(!!sym(enumerator_var)) %>%
    summarize(
        num_submissions = n()
    ) %>%
    ungroup()


enumerator_complete_subs_dataset <-
    if(enumerator_complete_var != "") {
        hfc_dataset %>%
            group_by(!!sym(enumerator_var)) %>%
            summarize(
                num_complete_submissions = sum(
                    across(enumerator_complete_var, ~ .x == 1 | .x == "Yes"), na.rm = TRUE
                )
            ) %>%
            ungroup()
    } else {
        tibble() %>% # So that it merges without error, but does not add information
            mutate(
                !!enumerator_var := case_when(
                    class(hfc_dataset[[enumerator_var]]) == "character" ~ list(NA_character_),
                    class(hfc_dataset[[enumerator_var]]) == "integer"   ~ list(NA_integer_),
                    class(hfc_dataset[[enumerator_var]]) == "numeric"   ~ list(NA_real_),
                    TRUE                                                    ~ list(NA)
                ) %>%
                    unlist()
            )
    }


enumerator_daily_subs_dataset <- 
    if(enumerator_date_var != "") {
        hfc_dataset %>%
            # Attempt to format date. This may need to be added to depending on reasonable formats to expect
            mutate(
                date_var_formatted = lubridate::parse_date_time(
                    !!sym(enumerator_date_var), c("Y-m-d", "m/d/Y", "d/m/Y")
                ) %>%
                    as.Date()
            ) %>%
            group_by(
                !!sym(enumerator_var), date_var_formatted
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
                !!enumerator_var := case_when(
                    class(hfc_dataset[[enumerator_var]]) == "character" ~ list(NA_character_),
                    class(hfc_dataset[[enumerator_var]]) == "integer"   ~ list(NA_integer_),
                    class(hfc_dataset[[enumerator_var]]) == "numeric"   ~ list(NA_real_),
                    TRUE                                                    ~ list(NA)
                ) %>%
                    unlist()
            )
    }


enumerator_daily_subs_plot <- 
    
    plot_data <- hfc_dataset %>%
    # Attempt to format date. This may need to be added to depending on reasonable formats to expect
    mutate(
        date_var_formatted = lubridate::parse_date_time(
            !!sym(enumerator_date_var), c("Y-m-d", "m/d/Y", "d/m/Y")
        ) %>%
            as.Date()
    ) %>%
    group_by(
        !!sym(enumerator_var), date_var_formatted
    ) %>%
    summarize(
        num_submissions = n()
    ) %>%
    ungroup() %>%
    group_by(
        !!sym(enumerator_var)
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
        !!enumerator_var := factor(!!sym(enumerator_var))
    ) %>%
    group_by(!!sym(enumerator_var)) %>%
    highlight_key(
        as.formula(
            paste0("~", enumerator_var)
        )
    ) %>%
    ggplot() +
    geom_line(
        aes(x = date_var_formatted, y = cumul_submissions, color = !!sym(enumerator_var))
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



enumerator_subs_dataset <- enumerator_total_subs_dataset %>%
    left_join(enumerator_complete_subs_dataset) %>%  # Works because is empty tibble if not "complete" variable is selected
    left_join(enumerator_daily_subs_dataset)


# Calculate mean and % out-of-range for each variable in enumerator_ave_vars
enumerator_ave_vars_dataset <- {
  data <- hfc_dataset
  vars <- enumerator_ave_vars
  enum_var <- enumerator_var

  # If any required input is missing or invalid, return NULL
  if (is.null(vars) || length(vars) == 0 || !all(vars %in% names(data)) || !(enum_var %in% names(data))) {
    return(NULL)
  }

  avg_list <- list()
  out_range_list <- list()
  for (var in vars) {
    # Average
    avg_col <- data %>%
      group_by(!!sym(enum_var)) %>%
      summarize(
        "{var}_avg" := round(mean(.data[[var]], na.rm = TRUE), digits = 2),
        .groups = "drop"
      )
    avg_list[[var]] <- avg_col

    # Out-of-range percentage
    limits <- enumerator_ave_vars_limits[[var]]
    min_val <- suppressWarnings(as.numeric(limits$min))
    max_val <- suppressWarnings(as.numeric(limits$max))

    # Use dataset min/max if missing
    if (is.na(min_val)) {
      min_val <- suppressWarnings(suppressMessages(min(data[[var]], na.rm = TRUE)))
    }
    if (is.na(max_val)) {
      max_val <- suppressWarnings(suppressMessages(max(data[[var]], na.rm = TRUE)))
    }

    col_name <- paste0(var, "_out_of_range")
    if (!is.na(min_val) && !is.na(max_val)) {
      out_col <- data %>%
        group_by(!!sym(enum_var)) %>%
        summarize(
          n_total = sum(!is.na(.data[[var]])),
          n_out = sum((.data[[var]] < min_val | .data[[var]] > max_val) & !is.na(.data[[var]])),
          .groups = "drop"
        ) %>%
        mutate(
          "{col_name}" := ifelse(n_total > 0, paste0(round(100 * n_out / n_total), "%"), NA_character_)
        ) %>%
        select(-n_total, -n_out)
    } else {
      out_col <- data %>%
        group_by(!!sym(enum_var)) %>%
        summarize(
          "{col_name}" := NA_character_,
          .groups = "drop"
        )
    }
    out_range_list[[var]] <- out_col
  }

  # Start with unique enumerator values
  final_df <- data %>%
    distinct(!!sym(enum_var))

  # Interleave avg and out_of_range columns for each variable
  for (var in vars) {
    final_df <- final_df %>%
      left_join(avg_list[[var]], by = enum_var) %>%
      left_join(out_range_list[[var]], by = enum_var)
  }

  final_df
}