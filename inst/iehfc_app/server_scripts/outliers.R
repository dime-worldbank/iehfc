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
                group_by(!!sym(selected_id_var())) %>%
                mutate(
                    !!selected_id_var() := case_when(
                        n() > 1 ~ paste0(!!sym(selected_id_var()), "_", row_number()),
                        TRUE    ~ as.character(!!sym(selected_id_var()))
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
                        select(., any_of(selected_id_var()), any_of(outlier_extra_vars()), issue_var, value = matches(paste0("^", .x, "$")), mean, sd, low_limit, high_limit)
                    } else {
                        select(., any_of(selected_id_var()), any_of(outlier_extra_vars()), issue_var, value = matches(paste0("^", .x, "$")), mean, iqr, low_limit, high_limit)
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
                group_by(!!sym(selected_id_var())) %>%
                mutate(
                    !!selected_id_var() := case_when(
                        n() > 1 ~ paste0(!!sym(selected_id_var()), "_", row_number()),
                        TRUE    ~ as.character(!!sym(selected_id_var()))
                    )
                ) %>%
                ungroup() %>%
                select(
                    any_of(selected_id_var()), any_of(outlier_extra_vars()), matches(paste0("^", .x, "_{0,1}[0-9]+$"))
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
                        select(., any_of(selected_id_var()), any_of(outlier_extra_vars()), issue_var, value, mean, sd, low_limit, high_limit)
                    } else {
                        select(., any_of(selected_id_var()), any_of(outlier_extra_vars()), issue_var, value, mean, iqr, low_limit, high_limit)
                    }
                }
        ) %>%
        list_rbind()
}) %>%
    bindEvent(input$run_hfcs)

outlier_dataset <- reactive({
    bind_rows(indiv_outlier_dataset(), group_outlier_dataset()) %>%
        arrange(!!sym(selected_id_var()), issue_var)
}) %>%
    bindEvent(input$run_hfcs)

output$outlier_table <- renderDT(
    outlier_dataset(), fillContainer = TRUE
)


custom_winsorize <- function(data, var) {
    lower <- quantile(data[[var]], probs = 0.05, na.rm = TRUE)
    upper <- quantile(data[[var]], probs = 0.95, na.rm = TRUE)

    data[[var]][data[[var]] < lower] <- lower
    data[[var]][data[[var]] > upper] <- upper

    return(data)
}



winsorized_hfc_dataset <- reactive({
    req(hfc_dataset())
    data <- hfc_dataset()
    selected_vars <- current_indiv_outlier_vars()
    for (var in selected_vars) {
        data <- custom_winsorize(data, var)
    }
    return(data)
})


## Individual Variables Histograms -------------

#calculate appropriate bin width for each hist
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
        scale_x_continuous(labels = scales::comma)
}


indiv_combined_histograms <- function(var) {
    bin_width_with <- calculate_bin_width(hfc_dataset(), var)
    hist_with_outliers <- generate_histogram(hfc_dataset(), var, bin_width_with, "with outliers")

    bin_width_without <- calculate_bin_width(winsorized_hfc_dataset(), var)
    hist_without_outliers <- generate_histogram(winsorized_hfc_dataset(), var, bin_width_without, title_suffix = "winsorized (95%)")

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
    if (length(current_indiv_outlier_vars()) > 0) {
        tagList(
        div(style = "position: absolute; top: 10px; right: 10px; font-size: 13px; color: gray;",
            "Data winsorized at the 95% level, regardless of method and multiplier settings in the Check Selection and Setup tab."
        ),
        render_histogram_ui(current_indiv_outlier_vars())
    )
    }
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

generate_boxplot <- function(dataset, group) {
  req(hfc_dataset())
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

group_boxplots <- function(group) {
  req(hfc_dataset())
    boxplot <- generate_boxplot(hfc_dataset(), group)
    list(box = ggplotly(boxplot, tooltip = c("x", "y")))
}

render_boxplot_ui <- function(selected_groups) {
  req(hfc_dataset())
    if (length(selected_groups) > 0) {
        fluidRow(
            lapply(1:length(selected_groups), function(i) {
                group <- selected_groups[i]
                boxplots <- group_boxplots(group)

                fluidRow(
                    column(12, plotlyOutput(paste0("boxplot_", i)))
                ) %>% tagAppendAttributes(style = "margin: 40px 0 40px 0;")
            })
        )
    }
}

output$group_boxplot_rendered <- renderUI({
    render_boxplot_ui(group_outlier_vars())
})

observe({
    selected_groups <- group_outlier_vars()
    if (length(selected_groups) > 0) {
        lapply(1:length(selected_groups), function(i) {
            group <- selected_groups[i]
            plots <- group_boxplots(group)

            output[[paste0("boxplot_", i)]] <- renderPlotly({ plots$box })
        })
    }
})





export_outlier_histogram <- reactive({
    if (length(current_indiv_outlier_vars()) > 0) {
        plot_data <- hfc_dataset()
        selected_vars <- current_indiv_outlier_vars()

        plot_data_long <- plot_data %>%
            select(all_of(selected_vars)) %>%
            pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

        ggplot_obj <- ggplot(plot_data_long, aes(x = value)) +
            geom_histogram(binwidth = (max(plot_data_long$value, na.rm = TRUE) - min(plot_data_long$value, na.rm = TRUE)) / 30,
                           fill = "#9e83cf", color = "black") +
            labs(x = "Value", y = "Frequency") +
            theme_minimal() +
            scale_x_continuous(labels = scales::comma) +
            theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
            facet_wrap(~ variable, scales = "free", ncol = 1)

        n_vars <- length(selected_vars)
        return(ggplotly(ggplot_obj, tooltip = c("x", "y"), height = 250*n_vars, width = 900)) %>%
            layout(margin = list(b = 100))
    }
})

export_outlier_win_histogram <- reactive({
    if (length(current_indiv_outlier_vars()) > 0) {
        plot_data <- winsorized_hfc_dataset()
        selected_vars <- current_indiv_outlier_vars()

        plot_data_long <- plot_data %>%
            select(all_of(selected_vars)) %>%
            pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

        ggplot_obj <- ggplot(plot_data_long, aes(x = value)) +
            geom_histogram(binwidth = (max(plot_data_long$value, na.rm = TRUE) - min(plot_data_long$value, na.rm = TRUE)) / 30,
                           fill = "#9e83cf", color = "black") +
            labs(x = "Value", y = "Frequency") +
            theme_minimal() +
            scale_x_continuous(labels = scales::comma) +
            theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
            facet_wrap(~ variable, scales = "free", ncol = 1, labeller = as_labeller(function(x) paste(x, "_95%_winsorized")))

        n_vars <- length(selected_vars)
        return(ggplotly(ggplot_obj, tooltip = c("x", "y"), height = 250*n_vars, width = 900 )) %>%
            layout(margin = list(b = 100))
    }
})

export_outlier_boxplot <- reactive({
    if (length(group_outlier_vars()) > 0) {
        plot_data <- hfc_dataset()
        selected_groups <- group_outlier_vars()

        plot_data <- plot_data %>%
            select(matches(paste0("^", paste(selected_groups, collapse = "|"), "_{0,1}[0-9]*$"))) %>%
            pivot_longer(cols = everything(), names_to = "issue_var", values_to = "value") %>%
            mutate(group = sub("_.*", "", issue_var))


        ggplot_obj <- ggplot(plot_data, aes(x = issue_var, y = value)) +
            geom_boxplot(fill = "#9e83cf", color = "black") +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 25, hjust = 1),
                plot.title = element_text(margin = margin(b = 20)),
                plot.margin = margin(b = 40)
            ) +
            labs(x = NULL, y = NULL) +
            scale_y_continuous(labels = scales::comma) +
            facet_wrap(~ group, scales = "free", ncol = 1)  +
            theme(panel.spacing.x=unit(5, "lines"),panel.spacing.y=unit(5, "lines"))

        n_groups <- length(selected_groups)
        ggplotly(ggplot_obj, tooltip = c("x", "y"), height = 350*n_groups, width = 900) %>%
            highlight(on = "plotly_hover", off = "plotly_doubleclick")
    }  else {
        return(NULL)  # No boxplot generated if conditions are not met
    }
})



# render_histogram_ui <- function() {
#     fluidRow(
#         column(6, plotlyOutput("histogram_with")),
#         column(6, plotlyOutput("histogram_without"))
#     ) %>% tagAppendAttributes(style = "margin: 40px 0 40px 0;")
# }
# output$indiv_combined_histogram_rendered <- renderUI({
#     tagList(
#         div(style = "position: absolute; top: 40px; left: 60px; font-size: 21px; color: black;",
#             "Histograms for individual variables"
#         ),
#         div(style = "position: absolute; top: 60px; right: 60px; font-size: 13px; color: gray;",
#             "Data winsorized at the 95% level, regardless of method and multiplier settings in the Check Selection and Setup tab."
#         ),
#         div(style = "height: 50px;"),  # Blank space above the plots
#         render_histogram_ui(),
#         div(style = "height: 50px;")   # Blank space below the plots
#     )
# })
#
# output$histogram_with <- renderPlotly({
#     export_outlier_histogram()
# })
#
# output$histogram_without <- renderPlotly({
#     export_outlier_win_histogram()
# })
#


##### Download outlier codes ----
output$outlier_r_exp <- downloadHandler(
    filename = function() {
        "outlier_run.R"
    },
    content = function(file) {
        # Save the initial script to a temporary file
        initial_script <- system.file("iehfc_app/server_scripts/code_export/outlier_run.R", package = "iehfc")

        # Read the initial script content
        initial_content <- readLines(initial_script)

        # Prepend the additional code
        additional_code <- paste(
            "    #----------------------------------------------------\n",
            "    #    This is code sample for outliers check. \n",
            "    #---------------------------------------------------- \n",
            "\n",
            "\n",
            "# Load required libraries using pacman\n",
            "if (!requireNamespace(\"pacman\", quietly = TRUE)) {install.packages(\"pacman\")}\n",
            "pacman::p_load(dplyr, tidyr, purrr, ggplot2, DescTools, data.table)\n",
            "\n",
            "# Load your dataset\n",
            "# Replace this path with the actual path to your dataset\n",
            "hfc_dataset <- fread(\"C:/path/to/your/file.csv\")\n\n",
            "# Define the outlier variables\n",
            "indiv_outlier_vars <- c(", paste0("\"", input$indiv_outlier_vars_select_var, "\"", collapse = ", "), ")\n",
            "group_outlier_vars <- c(", paste0("\"", input$group_outlier_vars_select_var, "\"", collapse = ", "), ")\n",
            "selected_id_var <- ", paste0("\"", input$id_select_var, "\"", collapse = ", "), "\n",
            "outlier_extra_vars <- c(", paste0("\"", input$outlier_extra_vars_select_var, "\"", collapse = ", "), ")\n",
            "outlier_method_selected <- ", paste0("\"", input$outlier_method, "\"", collapse = ", "), "\n",
            "outlier_multiplier_selected <- ", paste0(input$outlier_multiplier, collapse = ", "), "\n",
            "\n",
            sep = ""
        )

        # Combine the additional code and the initial script content
        combined_content <- c(additional_code, initial_content)

        # Write the combined content to the final file
        writeLines(combined_content, file)
    }
)





output$outlier_s_exp <- downloadHandler(
    filename = function() {
        "outlier_run.do"
    },
    content = function(file) {
        # Save the initial script to a temporary file

        initial_script <- system.file("iehfc_app/server_scripts/code_export/outlier_run.do", package = "iehfc")

        # Read the initial script content
        initial_content <- readLines(initial_script)

        # Prepend the additional code
        additional_code <- paste(
            "    /*----------------------------------------------------\n",
            "           This is code sample for outliers check. \n",
            "    -----------------------------------------------------*/ \n",
            "\n",
            "\n",
            "    * Define the outlier variables\n",
            "       local indiv_outlier_vars \"", paste0(input$indiv_outlier_vars_select_var, collapse = " "), "\"\n",
            "       local group_outlier_vars \"", paste0(input$group_outlier_vars_select_var, collapse = " "), "\"\n",
            "       local selected_id_var \"", paste0(input$id_select_var, collapse = " "), "\"\n",
            "       local outlier_extra_vars \"", paste0(input$outlier_extra_vars_select_var, collapse = " "), "\"\n",
            "       local outlier_method_selected \"", paste0(input$outlier_method, collapse = " "), "\"\n",
            "       local outlier_multiplier_selected \"", paste0(input$outlier_multiplier, collapse = " "), "\"\n",
            "\n",
            sep = ""
        )

        # Combine the additional code and the initial script content
        combined_content <- c(additional_code, initial_content)

        # Write the combined content to the final file
        writeLines(combined_content, file)
    }
)
