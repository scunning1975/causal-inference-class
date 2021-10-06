*******************************************************
* name: stacking_cs.do
* author: scott cunningham (baylor) 
* description: Stacked regression example with castle
* last updated: October 8, 2021
*******************************************************

capture log close
clear

cd "/Users/scott_cunningham/git/causal-inference-class/Master_do_files"
use https://github.com/scunning1975/mixtape/raw/master/castle.dta, clear

* effyear is the treatment variable. I need to loop through each value
* of the variable.

label variable time_til "Relative event year"

* Four step process. 

* Step 1: Map the distinct values of varname to 1, 2, 3, and up to the number of distinct values.

egen group = group(effyear)
su group, meanonly

tempfile data
save "`data'", replace

* Goal: Keep only the years associated with time_til between -5
* and +4 for each group. A few states don't have a +3. I'll deal
* with that another time.

* Step 2: Loop through by using only a group and it's never treated.
forvalues i = 1/`r(max)' {
		use `data', replace
		gen stacked=`i'

		* Only the treated and not-yet-treated
		keep if (effyear!=.) // Keep all the eventually treated
		gen temp=effyear if group==`i' // Create new time_til variables
		egen treat_date=min(temp)
		gen event=year-treat_date
		drop if event<-5 | event>4 // Data selection
		
		save ../data/stacking/loop1.dta, replace
		
		* Only the never-treated
		use `data', replace
		gen stacked=`i'
		keep if effyear==. | group==`i'
		gen temp=effyear if group==`i'
		egen treat_date=min(temp)
		gen event=year-treat_date
		drop if event<-5 | event>4
		drop if group==`i'
		
		save ../data/stacking/loop2.dta, replace
		
		append using ../data/stacking/loop1.dta

		save ../Data/Stacking/stacked`i'.dta, replace
}


* Step 3: Now append the datasets into one single stacked dataset.

use `data', clear
su group, meanonly

use ../data/stacking/stacked1.dta, replace

forvalues i = 2/`r(max)' {

	append using ../data/stacking/stacked`i'.dta
}

xtset idd event
save ../data/stacking/stacked.dta, replace

* Defining variables
drop post
gen post=0
replace post=1 if event>=0
gen treat=0
replace treat=1 if group~=.

egen idd=group(sid stacked)

* Step 4: Estimation with dataset interaction fixed effects and 
* relative event time fixed effects, clustering on unique stateXdataset
* identifiers

xi: xtreg l_homicide i.event i.stacked*i.sid i.stacked*i.year post##treat, fe robust cluster(idd)


capture log close
exit
