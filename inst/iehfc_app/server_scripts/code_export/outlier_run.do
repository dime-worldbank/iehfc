	* Install winsor2 command
	cap which winsor2 
	if _rc ssc install winsor2
	 
	* Create unique id, in case the id variable is not unique
    tostring `selected_id_var', replace 
    bysort `selected_id_var': gen outlier_id = `selected_id_var' + "-" + string(_n) if _N > 1 
    replace outlier_id = `selected_id_var' if mi(outlier_id) 

    * Create list of group variables
    foreach var of loc group_outlier_vars { 
        unab fullvarlist : `var'* 
        loc groupvarlist = "`groupvarlist' " + "`fullvarlist'" 
    } 

    * Create a tempfile to store the outlier values
    preserve 
        clear 
        tempfile outlier_file 
        save `outlier_file', replace emptyok 
    restore 

    * List of outliers for individual variables
    foreach var of loc indiv_outlier_vars { 
        preserve 
            keep outlier_id `selected_id_var' `outlier_extra_vars' `var' 

            if "`outlier_method_selected'" == "sd" { 
                qui sum `var' 

                g sd = r(sd) 
                g low_limit = r(mean) - (`outlier_multiplier_selected' * r(sd)) 
                g high_limit = r(mean) + (`outlier_multiplier_selected' * r(sd)) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var' 
				loc bin_width = (3.5 * r(sd)) / (`=_N'^(1/3))
				
				winsor2 `var', cuts(5 95) suffix(_win)
				
				hist `var' if !mi(`var'), freq ///
						width(`bin_width') ///
						title("Histogram of `var' with outliers") ///
						name(`var'_i, replace)
						
				hist `var'_win if !mi(`var'), freq ///
						width(`bin_width') ///
						title("Histogram of `var' winsorized (95%)") ///
						name(`var'_win, replace)
            } 
            else { 
                egen iqr = iqr(`var') 
                qui sum `var', d 

                g low_limit = r(p25) - (`outlier_multiplier_selected' * iqr) 
                g high_limit = r(p75) + (`outlier_multiplier_selected' * iqr) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var'
				loc bin_width = (2 * (r(p75) - r(p25))) / (`=_N'^(1/3))
				winsor2 `var', cuts(5 95) suffix(_win)
				
				hist `var' if !mi(`var'), freq ///
						width(`bin_width') ///
						title("Histogram of `var' with outliers") ///
						name(`var'_i, replace)
						
				hist `var'_win if !mi(`var'), freq ///
						width(`bin_width') ///
						title("Histogram of `var' winsorized (95%)") ///
						name(`var'_win, replace)
            } 
						
			
            keep if !inrange(value, low_limit, high_limit) & !mi(value) 
            keep `selected_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit 
	
            append using `outlier_file', nolabel nonotes 
            save `outlier_file', replace  
        restore 
    } 

    * List of outliers for group variables
    foreach var of loc group_outlier_vars { 
        preserve 
            keep outlier_id `selected_id_var' `outlier_extra_vars' `var'* 
            reshape long `var'_, i(outlier_id) j(outliersl) 

            if "`outlier_method_selected'" == "sd" { 
                qui sum `var'_ 

                g sd = r(sd) 
                g low_limit = r(mean) - (`outlier_multiplier_selected' * r(sd)) 
                g high_limit = r(mean) + (`outlier_multiplier_selected' * r(sd)) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var'_ 
            } 
            else { 
                egen iqr = iqr(`var'_) 
                qui sum `var'_, d 

                g low_limit = r(p25) - (`outlier_multiplier_selected' * iqr) 
                g high_limit = r(p75) + (`outlier_multiplier_selected' * iqr) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var'_ 
            } 
			
			replace issue_var = "`var'_" + string(outliersl) 
			graph box `var'_  if !mi(`var'_ ), over(issue_var, lab(angle(45) labsize(small))) ///
								asyvars showyvars ///
								ylabel(#10, format(%9.0f) labsize(small) angle(0)) ///
								ytitle("Values") ///
								title("Boxplot for Group `var'") ///
								legend(off) name(`var'_group, replace)
			
            keep if !inrange(value, low_limit, high_limit) & !mi(value) 
           

            keep `selected_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit 
            append using `outlier_file', nolabel nonotes 
            save `outlier_file', replace 
        restore 
    } 

    u `outlier_file', clear 
    sort issue_var `selected_id_var', stable 
    list `selected_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit, sepby(issue_var) 
    order `selected_id_var' `outlier_extra_vars' issue_var value  mean `outlier_method_selected' low_limit high_limit 
	
		
    * Export to CSV
    export delimited using "outlier_table.csv", replace 

