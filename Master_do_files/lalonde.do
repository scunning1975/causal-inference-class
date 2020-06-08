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

* Now estimate the propensity score
logit treat age agesq agecube educ edusq marr nodegree black hisp re74 re75 u74 u75 interaction1 
predict pscore

* reshape by 1974, 1975, 1978
foreach x of varlist age agesq agecube educ edusq marr nodegree black hisp interaction1 pscore{
	
	gen `x'74 = `x'
	gen `x'75 = `x'
	gen `x'78 = `x'

	}

gen id=_n

drop age agesq agecube educ edusq marr nodegree black hisp interaction1 pscore

reshape long age agesq agecube educ edusq marr nodegree black hisp interaction1 pscore re, i(id) j(year)

*** Callaway and Sant'anna (2019) estimator
encode data_id, gen(group)
numlabel, add

* Create group dummies
gen	g1=0
replace g1=1 if group==1

**************************************************************************************************************************************
* Calculate ATT(1986,1986)
* This the the ATT for the group first treated at 1986 (g1==1) in the first period since treatment (1986)
* gen outcomes (easiest way to transform data into wide form is this)
**************************************************************************************************************************************
gen ypost = re if year==78
gen ypre = re if year==75
* Generate Denominators of the weights
* We can use bysort here because g1 and and pg1_1991 are time-invariant
bysort year: egen g1_mean = mean(g1)
bysort year: egen g1_cont_1991mean = mean((1 - g1)*pscore/(1 - pscore))


**************************************************************************************************************************************
* Get weights
**************************************************************************************************************************************
gen w1= g1/g1_mean
gen w0 = ((1 - g1)*pscore/(1 - pscore))/g1_cont_1991mean
* Generate each component of the DID
egen att_11 = mean(w1*ypost)
egen att_10 = mean(w1*ypre)
egen att_01 =  mean(w0*ypost)
egen att_00 = mean(w0*ypre)

* Get the ATT(1978,1975)
gen att1978_1975 = (att_11-att_10) -(att_01 -att_00)
su att1978_1975

* Drop variable so I can copy paste this code!
drop ypost ypre g1_mean g1_cont_1991mean w1 w0 att_* att1978_1975



*** Callaway and Sant'anna (2019) estimator [0.1,0.9]

drop if pscore<0.1
drop if pscore>0.9

gen ypost = re if year==78
gen ypre = re if year==75

bysort year: egen g1_mean = mean(g1)
bysort year: egen g1_cont_1991mean = mean((1 - g1)*pscore/(1 - pscore))

gen w1= g1/g1_mean
gen w0 = ((1 - g1)*pscore/(1 - pscore))/g1_cont_1991mean

egen att_11 = mean(w1*ypost)
egen att_10 = mean(w1*ypre)
egen att_01 =  mean(w0*ypost)
egen att_00 = mean(w0*ypre)

* Get the ATT(1978,1975)
gen att1978_1975 = (att_11-att_10) -(att_01 -att_00)
su att1978_1975

* Drop variable so I can copy paste this code!
drop ypost ypre g1_mean g1_cont_1991mean w1 w0 att_*


