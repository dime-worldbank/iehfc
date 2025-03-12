    * Sort and identify duplicates
       bys `unit_var': gen _unit_order = _n 
       loc unit_type: type `unit_var' 
       if regex("`unit_type'", "str"){ 
            ren  `unit_var' `unit_var'__str 
       }
       else tostring `unit_var', gen(`unit_var'__str)

       replace `unit_var'__str = `unit_var'__str + "_" + string(_unit_order) 

    * Export to CSV
       keep `unit_var'__str `unit_extra_vars' 
       ren `unit_var'__str `unit_var' 
       keep `unit_var' `unit_extra_vars' 
       export delimited using "unit_table.csv", replace

