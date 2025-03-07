#This code generates a dataset of observations with duplicated unique id
duplicate_dataset <- hfc_dataset %>%
    group_by(across(all_of(selected_id_var))) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    {if (all(duplicate_extra_vars == "")) {select(., all_of(selected_id_var)) } else {select(., all_of(c(selected_id_var, duplicate_extra_vars)))}}


#This code generates a dataset of duplicate observation based on the specified variables
duplicate_multi_dataset <- if (all(duplicate_multi_vars == "")) {NULL} else {
    hfc_dataset %>%
        group_by(across(all_of(duplicate_multi_vars))) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        select(all_of(c(selected_id_var, duplicate_multi_vars)))} 
