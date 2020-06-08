* Constructing interval estimates from randomization inference

* Step 1: Choose from proposed C = {-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1}
* Step 2: y* = y - C(any) (where any is the treatment)
* Step 3: Calculate treatment effect using true treatment effect status and y*
* Step 4: Calculate RI p-value using y*
* Step 5: If this RI p-value is greater than 0.05, C is within the 95% CI

use https://github.com/scunning1975/mixtape/raw/master/thornton_hiv.dta, clear

tempfile hiv
save "`hiv'", replace

* Steps 1. and 2.  Calculate y* off of C proposed values. 

gen ystar = got - (any)*(1)

* Calculate true effect using absolute value of SDO

egen 	te1 = mean(ystar) if any==1
egen 	te0 = mean(ystar) if any==0

collapse (mean) te1 te0
gen 	ate = te1 - te0
keep 	ate
gen iteration = 1

tempfile permute1
save "`permute1'", replace

* Create a hundred datasets

forvalues i = 2/1000 {

use "`hiv'", replace

drop any
set seed `i'
gen random_`i' = runiform()
sort random_`i'
gen one=_n
drop random*
sort one

gen 	any = 0
replace any = 1 in 1/2222

* Calculate test statistic using absolute value of SDO
egen 	te1 = mean(got) if any==1
egen 	te0 = mean(got) if any==0

collapse (mean) te1 te0
gen 	ate = te1 - te0
keep 	ate

gen 	iteration = `i'
tempfile permute`i'
save "`permute`i''", replace

}

use "`permute1'", replace
forvalues i = 2/1000 {
    append using "`permute`i''"
}

tempfile final
save "`final'", replace

* Calculate exact p-value for C

gsort -ate
gen rank = _n
su rank if iteration==1
gen pvalue = (`r(mean)'/1000)
list if iteration==1

