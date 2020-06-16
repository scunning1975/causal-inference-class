********************************************************************************
/* 
Name: 			baker_SA.do
Author: 		shan huang adapting scott cunningham (baylor)/andrew baker (stanford)
Description: 
	Illustrates how Sun-Abraham 2020 performs compared to 2WFE for a simulated 
	data set with a never-treated cohort in staggered diff-in-diff setting. 
	Standard errors are (unlike in the original paper) computed via bootstrap.
Written in stata version 13
	
Last updated: 	june 16, 2020
Email: 			shuang@diw.de
*/
********************************************************************************

clear
capture gr drop sim_1 sim_2
capture log close
capture set scheme plotplainblind
set seed 20200403


//------------------------------------------------------------------------------ 
// SIMULATE DATA SET
//------------------------------------------------------------------------------
	* Create a panel of firm-years. 25 firms per state, observed over 30 years. 
	* Treatment is on the state-level. States can be grouped into "cohorts" 
	* depending on when treatment starts: 4 treatment cohorts each with 10 
	* states, 1 never-treated cohort with 5 states. 
	* --> Total: 45 states, 1125 firms, 30 years.

* First create the states
set obs 45
gen state = _n

* Generate firms. There are 25 firms per state.
expand 25
bys state: gen firms = 0+(5-0)*runiform() // for later Stata ed's bysort state: gen firms=runiform(0,5)
label variable firms "Unique firm fixed effect per state"

* Second create the years
expand 30
sort state
bysort state firms: gen year = _n
gen n=year
replace year = 1980 if year==1
replace year = 1981 if year==2
replace year = 1982 if year==3
replace year = 1983 if year==4
replace year = 1984 if year==5
replace year = 1985 if year==6
replace year = 1986 if year==7
replace year = 1987 if year==8
replace year = 1988 if year==9
replace year = 1989 if year==10
replace year = 1990 if year==11
replace year = 1991 if year==12
replace year = 1992 if year==13
replace year = 1993 if year==14
replace year = 1994 if year==15
replace year = 1995 if year==16
replace year = 1996 if year==17
replace year = 1997 if year==18
replace year = 1998 if year==19
replace year = 1999 if year==20
replace year = 2000 if year==21
replace year = 2001 if year==22
replace year = 2002 if year==23
replace year = 2003 if year==24
replace year = 2004 if year==25
replace year = 2005 if year==26
replace year = 2006 if year==27
replace year = 2007 if year==28
replace year = 2008 if year==29
replace year = 2009 if year==30
egen id =group(state firms)

* Generate treatment. 4 cohorts with treatment onset in 1986, 1992, 1998, 2004. Cohort==0 is never-treated.
gen     cohort=0
replace cohort=1 if state<=10
replace cohort=2 if state>10 & state<=20
replace cohort=3 if state>20 & state<=30
replace cohort=4 if state>30 & state<=40
gen     treat_date = 0 
replace treat_date = 1986 if cohort==1
replace treat_date = 1992 if cohort==2
replace treat_date = 1998 if cohort==3
replace treat_date = 2004 if cohort==4
gen     treat=0  
replace treat=1 if cohort==1 & year>=1986
replace treat=1 if cohort==2 & year>=1992
replace treat=1 if cohort==3 & year>=1998
replace treat=1 if cohort==4 & year>=2004

gen e 	= rnormal(0,(0.5)^2)
gen te1 = rnormal(7,(0.2)^2) 
gen te2 = rnormal(5,(0.2)^2)
gen te3 = rnormal(3,(0.2)^2)
gen te4 = rnormal(1,(0.2)^2)
gen te = .

replace te = te1 if cohort == 1
replace te = te2 if cohort == 2
replace te = te3 if cohort == 3
replace te = te4 if cohort == 4
replace te = 0 if cohort == 0


* Data generating process with heterogeneity over time
	* Create dynamic treatment effects over time that differ by cohort: 
	* Treatment effects increase each year by te1 for cohort 1, te2 for cohort 2 
	* etc. Cumulative treatment effect is te x (year - treat_date + 1).
gen y = firms + n + treat*te*(year - treat_date + 1) + e 			


* Constant treatment effects for comparison.
gen y2 = firms + n + te*treat + e 


* Generate relative time (leads and lags relative to treatment onset)
gen     time_til=-1
replace time_til=year-treat_date if treat_date>1
ta time_til, gen(dd)



//------------------------------------------------------------------------------
// TWFE ESTIMATION 
//------------------------------------------------------------------------------
	* TWFE is alright for constant treatment effects but Diff-in-diff estimate 
	* is heavily biased. Note also in dynamic specification (allowing for 
	* relative time-specific treatment effects), pre-trends are off even when 
	* the never-treated group is included. 
	
* Constant treatment effects
areg y2 i.year treat, a(id) robust 

* Heterogenous treatment effects over time when there wouldn't be a never-treated group
areg y i.year treat if cohort!=0, a(id) robust 					// Diff-in-diff
areg y i.year dd1 - dd23 dd25-dd48 if cohort!=0, a(id) robust 	// Dynamic spec

* Heterogenous treatment effect over time 
areg y i.year treat, a(id) robust 								// Diff-in-diff
areg y i.year dd1 - dd23 dd25-dd48, a(id) robust 						// Dynamic spec
	* all these significant pre-trends shouldn't be there!
	
* Compare: True treatment effects
gen te_att =  treat*te*(year - treat_date + 1)
replace te_att=. if te_att==0
bys time_til: egen att_dd = mean(te_att)							// Relative time specific treatment effects

preserve
	keep time_til att_dd
	duplicates drop
	replace att_dd = 0 if att_dd==.
	mkmat time_til att_dd, matrix(ATT_true_dd)
	
	replace att_dd = . if att_dd==0	
	sum att_dd, meanonly					
	local ATT_true = round(`r(mean)',0.1)						// Diff-in-diff effect	
restore

	* True Diff-in-diff ATT
di "`ATT_true'"
	* Relative time specific ATT
mat list ATT_true_dd
	
	
//------------------------------------------------------------------------------	
// SAVE TWFE ESTIMATES FOR DISPLAY	
//------------------------------------------------------------------------------
	* Saves the estimates for graph later - lots of locals and matrices. These 
	* are the same regressions as in the section above.

* Locals to extract the right coefficients for later display
	unique year
	local T `r(unique)'
	sum time_til, meanonly
	local DDneg  = -`r(min)'-1			// Max number of pre-treatment periods (drop relative year -1) = 23 periods
	disp(`DDneg')
	local DDpos = `r(max)'+1			// Max number of post-treatment periods (including relative year 0) = 24 periods
	disp(`DDpos')

* Static TWFE with no never-treated group 
qui: areg y i.year treat if cohort!=0, a(id) robust 
	mat b = e(b)
	local ATT_2wfeNt = round(b[1,(`T'+1)],0.1)		

* Dynamic TWFE with no never-treated group 	
qui: areg y i.year dd1 - dd23 dd25-dd48 if cohort!=0, a(id) robust 
	matrix Ball2wfeNt = e(b)'
	matrix B2wfeNt = (Ball2wfeNt[(`T'+1)..(`T'+`DDneg'),1]\0\Ball2wfeNt[(`T'+`DDneg' + 1)..(rowsof(Ball2wfeNt)-1),1])		// Stack matrices, with a 0 for the effect in rel year -1
	mat colnames B2wfeNt = b_2wfeNt

* Static TWFE
qui: areg y i.year treat, a(id) robust 
	mat b = e(b)
	local ATT_2wfe = round(b[1,(`T'+1)],0.1)	

* Dynamic TWFE
// Problem even when there is a never-treated group... all these significant pre-trends shouldn't be there!
qui: areg y i.year dd1 - dd23 dd25-dd48, a(id) robust 
	matrix Ball2wfe = e(b)'
	matrix B2wfe = (Ball2wfe[(`T'+1)..(`T'+`DDneg'),1]\0\Ball2wfe[(`T'+`DDneg' + 1)..(rowsof(Ball2wfe)-1),1])				// Stack matrices, with a 0 for the effect in rel year -1
	mat colnames B2wfe = b_2wfe


//------------------------------------------------------------------------------	
// SUN-ABRAHAM
//------------------------------------------------------------------------------	
* Gen cohort indicator
tab treat_date, gen(dE)
rename dE1 dC

* Gen interactions between relative time dummies and cohorts 
foreach e of varlist dE* {
	foreach dd of varlist dd* {
		gen int_`dd'_`e' = `e'*`dd'
	}
}

* Drop relative period -1
drop int_dd24_* dd24

* Fully saturated TWFE estimator 
areg y i.year int_dd*, a(id) robust 

* Aggregate to get their interaction-weighted estimator
	* Compute weights by (dd,e) (where dd: relative time, e: cohort). I'm 
	* writing this as eclass program so bootstrapping works, but for point 
	* estimates that's not necessary - could just run lines 227 - 
capture program drop SAest
program define SAest, eclass
	version 13.0
	
	mata: mata clear
	
	*** Step 1: Get the cohort-relative year specific effects
	quietly areg y i.year int_dd*, a(id) robust 
	
	* Get matrix dimensions right
	qui: unique cohort
	local E = `r(unique)'-1				// Total number of cohorts, dropping control group (4)
	qui: unique time_til
	local DD = `r(unique)'-1 			// Total number of relative years, dropping relative year -1 (23+24=47)
	qui: unique year
	local T = `r(unique)'				// Total number of calendar years (30)
	qui: sum time_til
	local DDneg = -`r(min)' - 1 		// Number of pre-treatment periods, dropping rel year -1 (23)
	local DDpos = `DD' - `DDneg'			// Number of post-treatment periods, including rel year 0 (24)
	
	* Save everything as mata scalars
	mata: DD = strtoreal(st_local("DD"))
	mata: E = strtoreal(st_local("E"))
	mata: T = strtoreal(st_local("T"))
	mata: DDneg = strtoreal(st_local("DDneg"))
	mata: DDpos = strtoreal(st_local("DDpos"))
	
	* Initialize weighting matrix
	mata: W = J(DD*E, 1, .)			

	*** Step 2: Build the cohort weighting matrix
	local i = 1
	foreach e of varlist dE* {	
		foreach dd of varlist dd* {
			mata: i = strtoreal(st_local("i"))
			mata: W[i] = sum(st_data(., ("int_`dd'_`e'"))) / sum(st_data(., ("`dd'")))
			local i = `i'+1
		}
	}	
	
	* Format results
	mata: DDm = J(DDneg, 1, 0)\J(DDpos, 1, 1/DDpos)			// Vector with all zeros in pre-treatment periods, and 1/(Number of post-treatment periods) in post-treatment periods; i.e. "weighting" for Diff-in-diff ATT
	mata: ball = st_matrix("e(b)")
	mata: b = ball[1, T+1..T+DD*E]							// Matrix of coefficient estimates for interaction terms
	mata: bm = rowshape(b, E)'
	mata: wm = rowshape(W, E)'
	
	* Save ATTs
	mata: ATT_dd = rowsum(bm:*wm)		// Relative-time specific treatment effect estimates
	mata: ATT = rowsum(bm:*wm)'*DDm		// Diff-in-diff treatment effect estimates
	mata: coef = (ATT\ATT_dd)'
	mata: st_matrix("coef", coef)

	foreach var of varlist dd* {
		local namesB `namesB' "ATT_`var'"
		}
	matrix colnames coef = ATT `namesB' 
	 
	ereturn post coef 					// coef contains as first element the diff-in-diff ATT-estimate, followed by the relative year-specific ATTs
    ereturn local cmd="bootstrap"
end

* Sun-Abraham estimator with bootstrapped standard errors
		* 2 bootstrap reps for these purposes
set seed 1	
bootstrap _b, reps(5) nowarn cluster(firms): SAest


	* Save Sun-Abraham estimates
	qui: unique year
	local T `r(unique)'
	sum time_til, meanonly
	local DDneg  = -`r(min)'-1		// drop rel year -1
	local DDpos = `r(max)'+1		// add rel year 0

	matrix Bsaall = e(b)'
	matrix Bsa = (Bsaall[2..(`DDneg'+1),1]\0\Bsaall[(`DDneg' + 2)..(rowsof(Bsaall)),1])
	mat colnames Bsa = b_SA
	local ATT_sa = round(Bsaall[1,1],0.1)

	


//------------------------------------------------------------------------------	
// COMPARE ESTIMATES
//------------------------------------------------------------------------------	
matrix allB = (ATT_true_dd, B2wfe, Bsa, B2wfeNt)

clear 
svmat2 allB, names(col) full

lab var att_dd 		"True treatment effect"
lab var b_2wfeNt	"2WFE, drop never-treated"
lab var b_2wfe 		"2WFE"
lab var b_SA 		"Sun-Abraham"

line att_dd b_2wfe b_SA b_2wfeNt time_til, ///
	c(l l l l) msym(i i i i) lpattern(shortdash_dot dash solid dot) lwidth(thick medthick medthick medthick) ///
	xline(-1, lcolor(black)) ///
	legend(title("Point estimates") size(vsmall) col(1)) ///
	xtitle("Relative year") ///
	title("Simulation") ///
	name(sim_1) ///
	note("Simulation with cohort-specific treatment effects and linear growth in treatment effects over time.					" "30 calendar periods: 4 treatment cohorts with size 250 units each (treatment onset in periods 5, 13, 19, 25) and a never-treated group with size 125.", size(vsmall))
	
line att_dd b_2wfe b_SA time_til, ///
	c(l l l) msym(i i i) lpattern(shortdash_dot dash solid) lwidth(thick medthick medthick) ///
	xline(-1, lcolor(black)) ///
	legend(title("Point estimates") size(vsmall) col(1)) ///
	xtitle("Relative year") ///
	title("Simulation")	///
	name(sim_2) ///
	note("Simulation with cohort-specific treatment effects and linear growth in treatment effects over time.					" "30 calendar periods: 4 treatment cohorts with size 250 units each (treatment onset in periods 5, 13, 19, 25) and a never-treated group with size 125.", size(vsmall))

di `ATT_true'
di `ATT_2wfeNt'
di `ATT_2wfe'
di `ATT_sa'
	

	
	
	
	
	
	
	

