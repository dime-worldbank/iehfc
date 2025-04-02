        loc dateformat  "MDY" // check the format and change this to DMY or MDY as appropriate

       * Check variables
            if !mi("`enumerator_complete_var'") {
                capture assert `enumerator_complete_var' == 0 | `enumerator_complete_var' == 1
                if r(r)>2 {
                    n di as err "Submission Complete Variable must be a 1/0 variable."
                    exit
                }
            }

            foreach var in `enumerator_ave_vars' `enumerator_complete_var' {
                confirm numeric var `var'
            }



    * Submissions by Enumerator
        preserve 
            bys `enumerator_var' `enumerator_date_var': gen submissions = _N

            if !mi("`enumerator_complete_var'") {
                loc complete "complete"
                loc num_complete_submissions "num_complete_submissions"
                bys `enumerator_var' `enumerator_date_var': egen complete = total(`enumerator_complete_var')
            }

            if !mi("`enumerator_date_var'") {
                * Create subdate
                cap confirm string var `enumerator_date_var'
                if !_rc {
                    g enum_subdate = date(`enumerator_date_var', "`dateformat'")
                }

                cap confirm numeric var `enumerator_date_var'
                if !_rc {
                    g enum_subdate = `enumerator_date_var'
                }

                format enum_subdate %td

                collapse (first) submissions `complete', by(`enumerator_var' enum_subdate)

                * Graph
                xtset `enumerator_var' enum_subdate
                bysort `enumerator_var' (enum_subdate): gen cum_submissions = sum(submissions)

                levelsof enum_subdate, loc(alldates)
                xtline cum_submissions, overlay  /// 
                        title("Cumulative Submissions Over Time") /// 
                        xtitle("Date") ytitle("Cumulative # of Submissions") ///
                        xlab("`alldates'", labsize(small) ang(v)) ///
                        legend(off) ///
                        name(enum_graph, replace)
                graph export "enumerator_subs_plot-`=c(current_date)'.png", as(png) replace width(5000)
                gr close enum_graph
                drop cum_submissions

                * Table
                tostring enum_subdate, replace format("%td") force
                reshape wide `complete' submissions, i(`enumerator_var') j(enum_subdate) str
                if !mi("`enumerator_complete_var'") {
                    egen num_complete_submissions = rowtotal(complete*)
                }

                egen num_submissions = rowtotal(submissions*)
                ren (submissions*) (d_*) 

                order `enumerator_var' num_submissions `num_complete_submissions' d_*
            }

            if mi("`enumerator_date_var'") {
				if !mi("`complete'") {
					collapse (first) num_submissions=submissions `num_complete_submissions'=`complete', by(`enumerator_var')
                }
				if mi("`complete'") {
					collapse (first) num_submissions=submissions, by(`enumerator_var')
                }
				order `enumerator_var' num_submissions `num_complete_submissions'
            }

            list, table  abbreviate(22)

            * Export to CSV
            export delimited using "enumerator_subs_table.csv", replace
        restore


    * Variables' Average Value by Enumerator
        preserve 
            collapse (mean) `enumerator_ave_vars', by(`enumerator_var')

            list, table  abbreviate(22)

            * Export to CSV
            export delimited using "enumerator_ave_vars_table.csv", replace
        restore

