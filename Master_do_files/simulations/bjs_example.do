**************************************************
* name: bjs_example.do
* author: scott cunningham (baylor) based on kirill
* 		  borusyak's code
* description: estimates treatment effects in event
*              study using bjs and baker data
* last update: september 30, 2021
**************************************************
clear
capture log close
cd "/Users/scott_cunningham/git/causal-inference-class/Master_do_files/simulations"
* use ./baker.dta, replace


********************************************************************************
* name: baker.do
* author: scott cunningham (baylor) adapting andrew baker (stanford)
* description: shows what a piece of shit TWFE is with differential timing and
*              heterogenous treatment effects over time
* last updated: april 17, 2020
********************************************************************************

clear
capture log close
set seed 20200403

* 1,000 firms (25 per state), 40 states, 4 groups (250 per groups), 30 years
* First create the states
set obs 40
gen state = _n

* Finally generate 1000 firms.  These are in each state. So 25 per state.
expand 25
bysort state: gen firms=runiform(0,5)
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

* Add 250 firms treated every period with the treatment effect still 5 on average
* Cohort years 1986, 1992, 1998, 2004
su state, detail
gen     group=0
replace group=1 if state<=`r(p25)'
replace group=2 if state>`r(p25)' & state<=`r(p50)'
replace group=3 if state>`r(p50)' & state<=`r(p75)'
replace group=4 if state>`r(p75)' & `r(p75)'!=.
gen     treat_date = 0 
replace treat_date = 1986 if group==1
replace treat_date = 1992 if group==2
replace treat_date = 1998 if group==3
replace treat_date = 2004 if group==4
gen     treat=0  
replace treat=1 if group==1 & year>=1986
replace treat=1 if group==2 & year>=1992
replace treat=1 if group==3 & year>=1998
replace treat=1 if group==4 & year>=2004

* Data generating process
gen e 	= rnormal(0,100)
gen te1 = rnormal(10,(0.2)^2) 
gen te2 = rnormal(8,(0.2)^2)
gen te3 = rnormal(6,(0.2)^2)
gen te4 = rnormal(4,(0.2)^2)
gen te = .

replace te = te1 if group == 1
replace te = te2 if group == 2
replace te = te3 if group == 3
replace te = te4 if group == 4

* Cumulative treatment effect is te x (year - t_g + 1) -- Dynamic treatment effects over time for each group.
* How does (year - treat_date + 1) create dynamic ATT?  Assume treat_date is 1992 and it is year 2000. Then, te=8 x (2000 - 1992 + 1) = 8 x (9) = 72. Group 2's TE rises from an 8 up to 72 in the t+8 year.

* Data generating process with heterogeneity over time
gen y = firms + n + treat*te*(year - treat_date + 1) + e 

* Constant treatment effects.  Notice, the treatment effect is constant. 
gen y2 = firms + n + te*treat + e 

/*
	This simulated example illustrates how to estimate causal effects with event studies using a range of methods
	and plot the coefficients & confidence intervals using the event_plot command.
	
	Date: 28/05/2021
	Author: Kirill Borusyak (UCL), k.borusyak@ucl.ac.uk
	
	You'll need the following commands:
		- did_imputation (Borusyak et al. 2021): available on SSC
		- did_multiplegt (de Chaisemartin and D'Haultfoeuille 2020): available on SSC
		- eventstudyinteract (San and Abraham 2020): available on SSC
		- csdid (Callaway and Sant'Anna 2020) v1.5: available on SSC

ssc install did_imputation, replace
ssc install did_multiplegt, replace
ssc install eventstudyinteract, replace
ssc install reghdfe, replace
ssc install event_plot, replace
ssc install ftools, replace
ssc install drdid, replace

* old simulation code
// Generate a complete panel of 300 units observed in 15 periods
clear all
timer clear
set seed 10
global T = 15
global I = 300

set obs `=$I*$T'
gen i = int((_n-1)/$T )+1 					// unit id
gen t = mod((_n-1),$T )+1					// calendar period
tsset i t

// Randomly generate treatment rollout years uniformly across Ei=10..16 (note that periods t>=16 would not be useful since all units are treated by then)
gen Ei = ceil(runiform()*7)+$T -6 if t==1	// year when unit is first treated
bys i (t): replace Ei = Ei[1]
gen K = t-Ei 								// "relative time", i.e. the number periods since treated (could be missing if never-treated)
gen D = K>=0 & Ei!=. 						// treatment indicator

// Generate the outcome with parallel trends and heterogeneous treatment effects
gen tau = cond(D==1, (t-12.5), 0) 			// heterogeneous treatment effects (in this case vary over calendar periods)
gen eps = rnormal()							// error term
gen Y = i + 3*t + tau*D + eps 				// the outcome (FEs play no role since all methods control for them)

*/


// Estimation with did_imputation of Borusyak et al. (2021)
drop if year>=2004
did_imputation y id year treat_date, allhorizons pretrend(5)
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-5(1)17))

estimates store bjs // storing the estimates for later

/*
// Estimation with did_multiplegt of de Chaisemartin and D'Haultfoeuille (2020)
did_multiplegt Y i t D, robust_dynamic dynamic(5) placebo(5) breps(100) cluster(i) 
event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("de Chaisemartin and D'Haultfoeuille (2020)") xlabel(-5(1)5)) stub_lag(Effect_#) stub_lead(Placebo_#) together

matrix dcdh_b = e(estimates) // storing the estimates for later
matrix dcdh_v = e(variances)
*/

// Estimation with cldid of Callaway and Sant'Anna (2020)
csdid y, ivar(id) time(year) gvar(treat_date) notyet
estat event, estore(cs) // this produces and stores the estimates at the same time
event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-5(1)17) ///
	title("Callaway and Sant'Anna (2020)")) stub_lag(T+#) stub_lead(T-#) together

/*
// Estimation with eventstudyinteract of Sun and Abraham (2020)
sum Ei
gen lastcohort = Ei==r(max) // dummy for the latest- or never-treated cohort
forvalues l = 0/5 {
	gen L`l'event = K==`l'
}
forvalues l = 1/14 {
	gen F`l'event = K==-`l'
}
drop F1event // normalize K=-1 (and also K=-15) to zero
eventstudyinteract Y L*event F*event, vce(cluster i) absorb(i t) cohort(Ei) control_cohort(lastcohort)
event_plot e(b_iw)#e(V_iw), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)5) ///
	title("Sun and Abraham (2020)")) stub_lag(L#event) stub_lead(F#event) together

matrix sa_b = e(b_iw) // storing the estimates for later
matrix sa_v = e(V_iw)
*/

// TWFE OLS estimation (which is correct here because of treatment effect homogeneity). Some groups could be binned.
reghdfe y F*event L*event, a(id year) cluster(id)
event_plot, default_look stub_lag(L#event) stub_lead(F#event) together graph_opt(xtitle("Days since the event") ytitle("OLS coefficients") xlabel(-14(1)5) ///
	title("OLS"))

estimates store ols // saving the estimates for later

// Construct the vector of true average treatment effects by the number of periods since treatment
matrix btrue = J(1,6,.)
matrix colnames btrue = tau0 tau1 tau2 tau3 tau4 tau5
qui forvalues h = 0/5 {
	sum tau if K==`h'
	matrix btrue[1,`h'+1]=r(mean)
}

// Combine all plots using the stored estimates
event_plot btrue# bjs dcdh_b#dcdh_v cs sa_b#sa_v ols, ///
	stub_lag(tau# tau# Effect_# T+# L#event L#event) stub_lead(pre# pre# Placebo_# T-# F#event F#event) plottype(scatter) ciplottype(rcap) ///
	together perturb(-0.325(0.13)0.325) trimlead(5) noautolegend ///
	graph_opt(title("Event study estimators in a simulated panel (300 units, 15 periods)", size(medlarge)) ///
		xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-5(1)5) ylabel(0(1)3) ///
		legend(order(1 "True value" 2 "Borusyak et al." 4 "de Chaisemartin-D'Haultfoeuille" ///
				6 "Callaway-Sant'Anna" 8 "Sun-Abraham" 10 "OLS") rows(3) region(style(none))) ///
	/// the following lines replace default_look with something more elaborate
		xline(-0.5, lcolor(gs8) lpattern(dash)) yline(0, lcolor(gs8)) graphregion(color(white)) bgcolor(white) ylabel(, angle(horizontal)) ///
	) ///
	lag_opt1(msymbol(+) color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(O) color(cranberry)) lag_ci_opt2(color(cranberry)) ///
	lag_opt3(msymbol(Dh) color(navy)) lag_ci_opt3(color(navy)) ///
	lag_opt4(msymbol(Th) color(forest_green)) lag_ci_opt4(color(forest_green)) ///
	lag_opt5(msymbol(Sh) color(dkorange)) lag_ci_opt5(color(dkorange)) ///
	lag_opt6(msymbol(Oh) color(purple)) lag_ci_opt6(color(purple)) 
graph export "five_estimators_example.png", replace
