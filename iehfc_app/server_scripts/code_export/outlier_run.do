    * Create unique id, in case the id variable is not unique
    tostring `outlier_id_var', replace 
    bysort `outlier_id_var': gen outlier_id = `outlier_id_var' + "-" + string(_n) if _N > 1 
    replace outlier_id = `outlier_id_var' if mi(outlier_id) 

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
            keep outlier_id `outlier_id_var' `outlier_extra_vars' `var' 

            if "`outlier_method_selected'" == "sd" { 
                qui sum `var' 

                g sd = r(sd) 
                g low_limit = r(mean) - (`outlier_multiplier_selected' * r(sd)) 
                g high_limit = r(mean) + (`outlier_multiplier_selected' * r(sd)) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var' 
            } 
            else { 
                egen iqr = iqr(`var') 
                qui sum `var', d 

                g low_limit = r(p25) - (`outlier_multiplier_selected' * iqr) 
                g high_limit = r(p75) + (`outlier_multiplier_selected' * iqr) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var' 
            } 

            keep if !inrange(value, low_limit, high_limit) & !mi(value) 
            keep `outlier_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit 

            append using `outlier_file', nolabel nonotes 
            save `outlier_file', replace 
        restore 
    } 

    * List of outliers for group variables
    foreach var of loc group_outlier_vars { 
        preserve 
            keep outlier_id `outlier_id_var' `outlier_extra_vars' `var'* 
            reshape long `var'_, i(outlier_id) j(outliersl) 

            if "`outlier_method_selected'" == "sd" { 
                qui sum `var' 

                g sd = r(sd) 
                g low_limit = r(mean) - (`outlier_multiplier_selected' * r(sd)) 
                g high_limit = r(mean) + (`outlier_multiplier_selected' * r(sd)) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var' 
            } 
            else { 
                egen iqr = iqr(`var') 
                qui sum `var', d 

                g low_limit = r(p25) - (`outlier_multiplier_selected' * iqr) 
                g high_limit = r(p75) + (`outlier_multiplier_selected' * iqr) 
                g mean = r(mean) 
                g issue_var = "`var'" 
                g value = `var' 
            } 

            keep if !inrange(value, low_limit, high_limit) & !mi(value) 
            replace issue_var = "`var'_" + string(outliersl) 

            keep `outlier_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit 
            append using `outlier_file', nolabel nonotes 
            save `outlier_file', replace 
        restore 
    } 

    u `outlier_file', clear 
    sort issue_var `outlier_id_var', stable 
    list `outlier_id_var' `outlier_extra_vars' issue_var value mean `outlier_method_selected' low_limit high_limit, sepby(issue_var) 
    order `outlier_id_var' `outlier_extra_vars' issue_var value  mean `outlier_method_selected' low_limit high_limit 

    * Export to CSV
    export delimited using "outlier_table.csv", replace 

