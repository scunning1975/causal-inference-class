* name: resid_agg.do
* author: scott cunningham (baylor university)
* description: residualized aggregation for diff-in-diff
* date: march 29, 2020

* TEXT FROM BDM (2004)
* "First, one can regress Yst on state fixed effects, year * dummies, and any relevant covariates. 
* One can then divide the residuals of the treatment states only into two groups: residuals from 
* years before the laws, and residuals from years after the laws.  The estimate of the laws’ effect 
* and its standard error can then be obtained from an OLS regression in this two-period panel."

use https://github.com/scunning1975/mixtape/raw/master/castle.dta, clear
set scheme cleanplots

* define global macros
global crime1 jhcitizen_c jhpolice_c murder homicide  robbery assault burglary larceny motor robbery_gun_r 
global demo blackm_15_24 whitem_15_24 blackm_25_44 whitem_25_44 //demographics
global lintrend trend_1-trend_51 //state linear trend
global region r20001-r20104  //region-quarter fixed effects
global exocrime l_larceny l_motor // exogenous crime rates
global spending l_exp_subsidy l_exp_pubwelfare
global xvar l_police unemployrt poverty l_income l_prisoner l_lagprisoner $demo $spending

* "time_til" was generated like this:
*  generate time_til = year - treatment_date

gen treat = 0
replace treat = 1 if time_til>=1

* First regress Y on state fixed effects, year dummies and relevant covariates
xi: reg l_homicide i.sid i.year $xvar [aweight=popwt]
 
* Collect residuals
predict residuals if time_til!=., resid

* residuals are y - y_hat

* Divide the residuals of the treatment states *only* into two groups: before and after
gen after = 0 
replace after = 1 if time_til>=1
replace after = . if time_til==.

* The estimate of the laws' effect and its standard error can be then obtained from an OLS regression in this two-period panel.
reg residuals after [aweight=popwt]

* Versus what I get from twoway fixed effects with clustering. Coefficient on 
xi: xtreg l_homicide  i.year $xvar treat [aweight=popwt], fe vce(cluster sid)


