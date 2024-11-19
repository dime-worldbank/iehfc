pacman::p_load(
    shiny, dplyr, tidyr, stringr, lubridate, purrr, ggplot2, janitor, data.table, DT, remotes, bsicons,
    shinydashboard, shinyjs, markdown, htmlwidgets, webshot, plotly, bslib, kableExtra, here, bit64
)

summary_card_data <- reactive({
    dataset <- hfc_dataset()
    selected_id <- selected_id_var()
    
    total_responses <- nrow(dataset)
    total_complete_responses <- dataset %>% filter_all(all_vars(!is.na(.))) %>% nrow()
    total_duplicate_responses <- dataset %>%
        group_by(!!sym(selected_id)) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        nrow()
    total_unique_responses <- dataset %>%
        distinct(!!sym(selected_id)) %>%
        nrow()
    
    list(
        total_responses = total_responses,
        total_complete_responses = total_complete_responses,
        total_duplicate_responses = total_duplicate_responses,
        total_unique_responses = total_unique_responses
    )
})
