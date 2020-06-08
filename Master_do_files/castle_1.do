use https://github.com/scunning1975/mixtape/raw/master/castle.dta, clear
set scheme cleanplots
* ssc install bacondecomp

* define global macros
global crime1 jhcitizen_c jhpolice_c murder homicide  robbery assault burglary larceny motor robbery_gun_r 
global demo blackm_15_24 whitem_15_24 blackm_25_44 whitem_25_44 //demographics
global lintrend trend_1-trend_51 //state linear trend
global region r20001-r20104  //region-quarter fixed effects
global exocrime l_larceny l_motor // exogenous crime rates
global spending l_exp_subsidy l_exp_pubwelfare
global xvar l_police unemployrt poverty l_income l_prisoner l_lagprisoner $demo $spending

label variable post "Year of treatment"
xi: xtreg l_homicide i.year $region $xvar $lintrend post [aweight=popwt], fe vce(cluster sid)


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


