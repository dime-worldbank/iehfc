unit_dataset <-
    hfc_dataset %>%
    group_by(!!sym(unit_var)) %>%
    mutate(
        !!unit_var := case_when(
            n() > 1 ~ paste0(!!sym(unit_var), "_", row_number()),
            TRUE    ~ as.character(!!sym(unit_var))
        )
    ) %>%
    ungroup() %>%
    select(
        all_of(
            c(unit_var, unit_extra_vars)
        )
    ) %>%
    arrange(
        !!sym(unit_var))

