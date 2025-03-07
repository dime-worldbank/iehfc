# Individual Outlier Dataset
indiv_outlier_dataset <- indiv_outlier_vars %>%
    map(
        ~ hfc_dataset %>%
            group_by(!!sym(selected_id_var)) %>%
            mutate(
                !!selected_id_var := case_when(
                    n() > 1 ~ paste0(!!sym(selected_id_var), "_", row_number()),
                    TRUE    ~ as.character(!!sym(selected_id_var))
                )
            ) %>%
            ungroup() %>%
            mutate(
                across(
                    matches(paste0("^", .x, "$")), ~ mean(.x, na.rm = TRUE), .names = "mean"
                ),
                across(
                    matches(paste0("^", .x, "$")), 
                    ~ if (outlier_method_selected == "sd") sd(.x, na.rm = TRUE) else NaN, 
                    .names = "sd"
                ),
                across(
                    matches(paste0("^", .x, "$")), 
                    ~ if (outlier_method_selected == "iqr") IQR(.x, na.rm = TRUE) else NaN, 
                    .names = "iqr"
                ),
                across(
                    matches(paste0("^", .x, "$")), 
                    ~ if (outlier_method_selected == "iqr") {
                        quantile(.x, probs = 0.25, na.rm = TRUE)
                    } else NaN, 
                    .names = "p25"
                ),
                across(
                    matches(paste0("^", .x, "$")), 
                    ~ if (outlier_method_selected == "iqr") {
                        quantile(.x, probs = 0.75, na.rm = TRUE)
                    } else NaN, 
                    .names = "p75"
                ),
                low_limit = if (outlier_method_selected == "sd") {
                    mean - (outlier_multiplier_selected * sd)
                } else {
                    p25 - (outlier_multiplier_selected * iqr)
                },
                high_limit = if (outlier_method_selected == "sd") {
                    mean + (outlier_multiplier_selected * sd)
                } else {
                    p75 + (outlier_multiplier_selected * iqr)
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
                if (outlier_method_selected == "sd") {
                    select(., any_of(selected_id_var), any_of(outlier_extra_vars), issue_var, value = matches(paste0("^", .x, "$")), mean, sd, low_limit, high_limit)
                } else {
                    select(., any_of(selected_id_var), any_of(outlier_extra_vars), issue_var, value = matches(paste0("^", .x, "$")), mean, iqr, low_limit, high_limit)
                }
            }
        
    ) %>%
    list_rbind()


# Group Outlier Dataset
group_outlier_dataset <- group_outlier_vars %>%
    map(
        ~ hfc_dataset %>%
            group_by(!!sym(selected_id_var)) %>%
            mutate(
                !!sym(selected_id_var) := case_when(
                    n() > 1 ~ paste0(!!sym(selected_id_var), "_", row_number()),
                    TRUE    ~ as.character(!!sym(selected_id_var))
                )
            ) %>%
            ungroup() %>%
            select(
                any_of(selected_id_var), any_of(outlier_extra_vars), matches(paste0("^", .x, "_{0,1}[0-9]+$"))
            ) %>%
            pivot_longer(
                cols = matches(paste0("^", .x, "_{0,1}[0-9]+$")),
                names_to = "issue_var"
            ) %>%
            mutate(
                mean = mean(value, na.rm = TRUE),
                sd = sd(value, na.rm = TRUE),
                iqr = IQR(value, na.rm = TRUE),
                p25 = if (outlier_method_selected == "sd") quantile(value, probs = 0.25, na.rm = TRUE) else NaN,
                p75 = if (outlier_method_selected == "sd") quantile(value, probs = 0.75, na.rm = TRUE) else NaN,
                low_limit = if (outlier_method_selected == "sd") mean - (outlier_multiplier_selected * sd) else p25 - (outlier_multiplier_selected * iqr),
                high_limit = if (outlier_method_selected == "sd") mean + (outlier_multiplier_selected * sd) else p75 + (outlier_multiplier_selected * iqr)
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
                if (outlier_method_selected == "sd") {
                    select(., any_of(selected_id_var), any_of(outlier_extra_vars), issue_var, value, mean, sd, low_limit, high_limit)
                } else {
                    select(., any_of(selected_id_var), any_of(outlier_extra_vars), issue_var, value, mean, iqr, low_limit, high_limit)
                }
            }
    ) %>%
    list_rbind()


# Combine Datasets
outlier_dataset <- bind_rows(indiv_outlier_dataset, group_outlier_dataset) %>%
    arrange(across(any_of(selected_id_var)))


# Custom Winsorize function
custom_winsorize <- function(data, var) {
    data[[var]] <- DescTools::Winsorize(data[[var]], val = quantile(data[[var]], probs = c(0.05, 0.95), na.rm = TRUE))
    return(data)
}

# Winsorize the dataset
winsorized_hfc_dataset <- hfc_dataset
for (var in indiv_outlier_vars) {
    winsorized_hfc_dataset <- custom_winsorize(winsorized_hfc_dataset, var)
}

#Histograms setup

calculate_bin_width <- function(data, var) {
    x <- data[[var]]
    n <- length(x)
    iqr_val <- IQR(x, na.rm = TRUE)
    bin_width <- 3.5 * sd(x, na.rm = TRUE) / n^(1/3)
    return(bin_width)
}



generate_histogram <- function(dataset, var, bin_width, title_suffix) {
    ggplot(dataset, aes_string(x = var)) +
        geom_histogram(binwidth = bin_width, fill = "#9e83cf", color = "black") +
        labs(title = paste("Histogram of", var, title_suffix), x = var, y = "Frequency") +
        theme_minimal() +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma)
}




# Plot Individual Variable Histograms
indiv_combined_histograms <- function(var) {
    bin_width_with <- calculate_bin_width(hfc_dataset, var)
    hist_with_outliers <- generate_histogram(hfc_dataset, var, bin_width_with, "with outliers")
    
    bin_width_without <- calculate_bin_width(winsorized_hfc_dataset, var)
    hist_without_outliers <- generate_histogram(winsorized_hfc_dataset, var, bin_width_without, title_suffix = "winsorized (95%)")
    
    print(hist_with_outliers)
    print(hist_without_outliers)
}

for (var in indiv_outlier_vars) {
    indiv_combined_histograms(var)
}


# Boxplots for Group Variables
generate_boxplot <- function(dataset, group) {
    variable_group <- dataset %>%
        select(matches(paste0("^", group, "_{0,1}[0-9]+$"))) %>%
        pivot_longer(cols = everything(), names_to = "issue_var", values_to = "value")
    
    
    ggplot(variable_group, aes(x = issue_var, y = value)) +
        geom_boxplot(fill = "#9e83cf", color = "black") +
        labs(title = paste("Boxplot for Group", group), x = "Variables", y = "Values") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::comma)
    
}


for (group in group_outlier_vars) {
    print(generate_boxplot(hfc_dataset, group))
}
