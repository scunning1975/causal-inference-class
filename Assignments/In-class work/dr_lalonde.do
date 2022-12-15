*********************************************************************************
* name: dr_lalonde.do
* author: scott cunningham
* description: apply DR to LaLonde 1986 NSW with CPS controls
* last updated: October 16, 2021
*********************************************************************************

clear
capture log close
ssc install drdid, replace

* Reload experimental group data
use https://github.com/scunning1975/mixtape/raw/master/nsw_mixtape.dta, clear
drop if treat==0

* Now merge in the CPS controls from footnote 2 of Table 2 (Dehejia and Wahba 2002)
append using https://github.com/scunning1975/mixtape/raw/master/cps_mixtape.dta
gen agesq=age*age
gen agecube=age*age*age
gen edusq=educ*edu
gen u74 = 0 if re74!=.
replace u74 = 1 if re74==0
gen u75 = 0 if re75!=.
replace u75 = 1 if re75==0
gen interaction1 = educ*re74
gen re74sq=re74^2
gen re75sq=re75^2
gen interaction2 = u74*hisp


/*
1.	Before we implement DR, let's look at the issue of common support. Calculate a propensity score using the same covariates as used in the mixtape with logit maximum likelihood. Create a histogram showing the distribution of the propensity score for the treatment and control group.  What share of the data satisfies common support?
*/

* Now estimate the propensity score
logit treat age agesq agecube educ edusq marr nodegree black hisp re74 re75 u74 u75 interaction1 
predict pscore

* Checking mean propensity scores for treatment and control groups
su pscore if treat==1, detail
su pscore if treat==0, detail

* Now look at the propensity score distribution for treatment and control groups
* histogram pscore, by(treat)

keep if pscore>=0.1 & pscore<=0.9

/*
2.	Rearrange the data as a panel from 1975 to 1978 (you will need to reshape in Stata).
a.	Compare your results if using DR, OR or IPW. 
b.	Estimate your model using TWFE and all covariates contained in the data.  Can you estimate the coefficients on covariates?  Why not?
c.	Estimate the ATT using the DR method by Sant'Anna and Zhao (2020). The package can be found here:
*/

* Reshape the data so that every person is observed twice: once in 1975 and once in 1978.
* We are going from wide to long. Remember stub is the variable name (e.g., re)

gen id=_n
reshape long re u, i(id) j(year)
tsset id year

* 2a: estimate ATT using DR, OR, IPW using the drdid package

* Abadie IPW
drdid re u educ black age, ivar(id) time(year) tr(treat) ipw

* Heckman, et al reg
drdid re u educ black age, ivar(id) time(year) tr(treat) or

* Doubly robust
drdid re u educ black age, ivar(id) time(year) tr(treat) all

