    * Sort and identify duplicates
       duplicates tag `duplicate_id_var', gen(_dup)

    * Filter out duplicates
       keep if _dup > 0
       sort `duplicate_id_var', stable

    * Keep only the relevant columns
       keep `duplicate_id_var' `duplicate_extra_vars'
       list `duplicate_id_var' `duplicate_extra_vars', sepby(`duplicate_id_var')

    * Export to CSV
       export delimited using "duplicate_table.csv", replace

