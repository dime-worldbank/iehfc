       loc dateformat  "MDY" // check the format and change this to DMY or MDY as appropriate

       * Check variables
            if !mi("`admin_complete_var'") {
                qui ta `admin_complete_var'
                if r(r)>2 {
                    n di as err "Submission Complete Variable must be a 1/0 variable."
                    exit
                }
            }


    * Submissions by Admin
        preserve 
            bys `admin_var' `admin_date_var' `admin_super_vars': gen submissions = _N

            if !mi("`admin_complete_var'") {
                loc complete "complete"
                loc num_complete_submissions "num_complete_submissions"
                bys `admin_var' `admin_date_var' `admin_super_vars': egen complete = total(`admin_complete_var')
            }

        if !mi("`admin_date_var'") {
            * Create subdate
            cap confirm string var `admin_date_var'
            if !_rc {
                g admin_subdate = date(`admin_date_var', "`dateformat'")
            }

            cap confirm numeric var `admin_date_var'
            if !_rc {
                g admin_subdate = `admin_date_var'
            }

            format admin_subdate %td

            collapse (first) submissions `complete', by(`admin_var' `admin_super_vars' admin_subdate)

            tempfile admin_data
            save `admin_data'

            * Graph
            loc admn_type: type `admin_var'
            if regex("`admn_type'", "str") {
                encode `admin_var', gen(`admin_var'_num)
            }

            collapse (sum) submissions `complete', by(`admin_var'_num admin_subdate)

            xtset `admin_var'_num admin_subdate
            bysort `admin_var'_num (admin_subdate): gen cum_submissions = sum(submissions)

            levelsof admin_subdate, loc(alldates)
            xtline cum_submissions, overlay  ///
                    title("Cumulative Submissions Over Time") ///
                    xtitle("Date") ytitle("Cumulative # of Submissions") ///
                    xlab("`alldates'", labsize(small) ang(v)) ///
                    legend(off) ///
                    name(admin_graph, replace)
            graph export "admin_subs_plot-`=c(current_date)'.png", as(png) replace width(5000)
            gr close admin_graph

            * Table
            u `admin_data', clear 
            tostring admin_subdate, replace format("%td") force
            reshape wide `complete' submissions, i(`admin_var' `admin_super_vars') j(admin_subdate) str
            if !mi("`admin_complete_var'") {
                egen num_complete_submissions = rowtotal(complete*)
            }

            egen num_submissions = rowtotal(submissions*)
            ren (submissions*) (d_*) 

            order `admin_var' `admin_super_vars' num_submissions `num_complete_submissions' d_*
        }

        if mi("`admin_date_var'") {
            collapse (first) num_submissions=submissions `num_complete_submissions'=`complete', by(`admin_var' `admin_super_vars')
            order `admin_var' `admin_super_vars' num_submissions `num_complete_submissions'
        }

        list, table  abbreviate(22)

        * Export to CSV
        export delimited using "admin_subs_table.csv", replace
    restore

