use https://github.com/scunning1975/mixtape/raw/master/thornton_hiv.dta, clear

tempfile hiv
save "`hiv'", replace

* Calculate true effect using absolute value of SDO
* Step 2 (step 1 was Fisher sharp null) is the test statistic. 
* I use the SDO.

egen 	te1 = mean(got) if any==1
egen 	te0 = mean(got) if any==0

collapse (mean) te1 te0
gen 	ate = te1 - te0
keep 	ate
gen iteration = 1

tempfile permute1
save "`permute1'", replace

* Create a 999 datasets - step 3, randomized treatment vector.

forvalues i = 2/1000 {

use "`hiv'", replace

drop any // drop the treatment assignemt
set seed `i'
gen random_`i' = runiform()
sort random_`i'
gen one=_n
drop random*
sort one

gen 	any = 0 // create a new one based on randomization
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

* Calculate exact p-value
gsort -ate
gen rank = _n
su rank if iteration==1
gen pvalue = (`r(mean)'/1000)
list if iteration==1

