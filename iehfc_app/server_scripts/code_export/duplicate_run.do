    * Sort and identify duplicates
       duplicates tag `selected_id_var', gen(_dup)

    * Filter out duplicates
       keep if _dup > 0
       sort `selected_id_var', stable

    * Keep only the relevant columns
       keep `selected_id_var' `duplicate_extra_vars'
       list `selected_id_var' `duplicate_extra_vars', sepby(`selected_id_var')

    * Export to CSV
       export delimited using "duplicate_table.csv", replace

