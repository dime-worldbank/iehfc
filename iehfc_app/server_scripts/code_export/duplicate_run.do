    * ---- Identify duplicates based on id ----
	
	* Temporarily save the original data
		tempfile ___maindata
		save 	`___maindata'

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



    * ---- Identify duplicates based on multiple user-specified variables ----
    
	* Initiate the temporarily saved the original data
		u 	`___maindata'
		
    * Sort and identify duplicates
        duplicates tag `duplicate_multi_vars', gen(_multi_dup)
        
    * Filter out duplicates
       keep if _multi_dup > 0
       sort `duplicate_multi_vars', stable

    * Keep only the relevant columns
       keep `selected_id_var' `duplicate_multi_vars'
       list `selected_id_var' `duplicate_multi_vars', sepby(`duplicate_multi_vars')

    * Export to CSV
       export delimited using "duplicate_multi_table.csv", replace

