duplicate_dataset <- reactive({
    hfc_dataset() %>%
        group_by(!!sym(duplicate_id_var())) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        select(
            all_of(
                c(duplicate_id_var(), duplicate_extra_vars())
            )
        )
}) %>%
    bindEvent(input$run_hfcs)

output$duplicate_table <- renderDT(
    duplicate_dataset(), fillContainer = TRUE
)