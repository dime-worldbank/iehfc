admin_total_subs_dataset <- hfc_dataset %>%
    group_by(
        across(any_of(admin_super_vars)), !!sym(admin_var)
    ) %>%
    summarize(
        num_submissions = n()
    ) %>%
    ungroup()


admin_complete_subs_dataset <- if(admin_complete_var != "") {
    hfc_dataset %>%
        group_by(
            across(any_of(admin_super_vars)), !!sym(admin_var)
        ) %>%
        summarize(
            num_complete_submissions = sum(
                across(admin_complete_var, ~ .x == 1 | .x == "Yes"), na.rm = TRUE
            )
        ) %>%
        ungroup()
} else {
    tibble() %>% # So that it merges without error, but does not add information
        mutate(
            !!admin_var := case_when(
                class(hfc_dataset[[admin_var]]) == "character" ~ list(NA_character_),
                class(hfc_dataset[[admin_var]]) == "integer"   ~ list(NA_integer_),
                class(hfc_dataset[[admin_var]]) == "numeric"   ~ list(NA_real_),
                TRUE ~ list(NA)
            ) %>%
                unlist()
        )
}


admin_daily_subs_dataset <- if(admin_date_var != "") {
    hfc_dataset %>%
        # Attempt to format date. This may need to be added to depending on reasonable formats to expect
        mutate(
            date_var_formatted = lubridate::parse_date_time(
                !!sym(admin_date_var), c("Y-m-d", "m/d/Y", "d/m/Y")
            ) %>%
                as.Date()
        ) %>%
        group_by(
            across(any_of(admin_super_vars)), !!sym(admin_var), date_var_formatted
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
            !!admin_var := case_when(
                class(hfc_dataset[[admin_var]]) == "character" ~ list(NA_character_),
                class(hfc_dataset[[admin_var]]) == "integer"   ~ list(NA_integer_),
                class(hfc_dataset[[admin_var]]) == "numeric"   ~ list(NA_real_),
                TRUE                                               ~ list(NA)
            ) %>%
                unlist()
        )
}



admin_daily_subs_plot <- 
    plot_data <- hfc_dataset %>%
    # Attempt to format date. This may need to be added to depending on reasonable formats to expect
    mutate(
        date_var_formatted = lubridate::parse_date_time(
            !!sym(admin_date_var), c("Y-m-d", "m/d/Y", "d/m/Y")
        ) %>%
            as.Date()
    ) %>%
    group_by(
        across(any_of(admin_super_vars)), !!sym(admin_var), date_var_formatted
    ) %>%
    summarize(
        num_submissions = n()
    ) %>%
    ungroup() %>%
    group_by(
        across(any_of(admin_super_vars)), !!sym(admin_var)
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
        !!admin_var := factor(!!sym(admin_var))
    ) %>%
    group_by(
        !!sym(admin_var)
    ) %>%
    highlight_key(
        as.formula(
            paste0("~", admin_var)
        )
    ) %>%
    ggplot() +
    geom_line(
        aes(x = date_var_formatted, y = cumul_submissions, color = !!sym(admin_var))
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



admin_subs_dataset <- 
    admin_total_subs_dataset %>%
    left_join(admin_complete_subs_dataset) %>%  # Works because is empty tibble if not "complete" variable is selected
    left_join(admin_daily_subs_dataset)



