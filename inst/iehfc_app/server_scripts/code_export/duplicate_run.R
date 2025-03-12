duplicate_dataset <- hfc_dataset %>%
    group_by(across(all_of(selected_id_var))) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(all_of(c(selected_id_var, duplicate_extra_vars)))